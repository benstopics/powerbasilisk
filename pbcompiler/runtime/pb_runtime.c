/*
 * pb_runtime.c — PowerBASIC runtime support for pbcompiler
 *
 * Implements FORMAT$, PARSE$, REPLACE, and other PB-specific functions
 * that don't have direct C library equivalents.
 *
 * Build: clang -c pb_runtime.c -o pb_runtime.obj
 * Link:  clang program.ll pb_runtime.obj -o program.exe
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <math.h>

#ifdef _WIN32
/* Declare only what we need from oleaut32 — avoids pulling in all of windows.h */
__declspec(dllimport) char* __stdcall SysAllocStringByteLen(const char* psz, unsigned int len);
__declspec(dllimport) void __stdcall SysFreeString(char* bstrString);
/* Win32 API for crash handling */
__declspec(dllimport) void* __stdcall AddVectoredExceptionHandler(unsigned long First, void* Handler);
__declspec(dllimport) void __stdcall ExitProcess(unsigned int uExitCode);
__declspec(dllimport) int __stdcall MessageBoxA(void* hWnd, const char* lpText, const char* lpCaption, unsigned int uType);
#endif

/* ===== Debug/crash reporting ===== */
static const char* pb_debug_current_func = "unknown";
int pb_debug_line = 0; /* Current source line — updated by codegen */
static int pb_debug_enabled = 0;
static int pb_debug_verbose = 0; /* Set to 1 for verbose modal/function logging */
static FILE* pb_debug_log = NULL;

/* Circular buffer for recent call trace */
#define PB_TRACE_SIZE 50
static const char* pb_trace_buf[PB_TRACE_SIZE];
static int pb_trace_idx = 0;
static int pb_modal_count = 0;

/* Call this from codegen at the start of each function for debug mode */
void pb_debug_enter(const char* func_name) {
    pb_debug_current_func = func_name;
    pb_trace_buf[pb_trace_idx % PB_TRACE_SIZE] = func_name;
    pb_trace_idx++;
    if (pb_debug_log) {
        fprintf(pb_debug_log, "[ENTER] %s\n", func_name);
        fflush(pb_debug_log);
    }
}

void pb_debug_init(void) {
    pb_debug_enabled = 1;
    pb_debug_log = fopen("pb_debug.log", "w");
    if (pb_debug_log) {
        fprintf(pb_debug_log, "=== PowerBasilisk Debug Log ===\n");
        fflush(pb_debug_log);
    }
    /* Check environment variable for verbose debug mode */
    const char* verbose = getenv("PB_DEBUG_VERBOSE");
    if (verbose && (verbose[0] == '1' || verbose[0] == 'Y' || verbose[0] == 'y')) {
        pb_debug_verbose = 1;
        if (pb_debug_log) {
            fprintf(pb_debug_log, "[DEBUG] Verbose mode enabled via PB_DEBUG_VERBOSE\n");
            fflush(pb_debug_log);
        }
    }
}

/* ===== Modal/Dialog debug logging ===== */

/* Log modal dialog text for test debugging.
 * Called from PB code (ui_stubs or ui.inc) when debug mode is enabled.
 * Writes to pb_debug.log with modal type, title, and full text content.
 * When tests hang, this log shows the last modal that was displayed. */
void pb_debug_modal(const char* modal_type, const char* title, const char* text) {
    pb_modal_count++;
    if (pb_debug_log) {
        fprintf(pb_debug_log, "[MODAL #%d] type=%s title=\"%s\" text=\"%.500s\"\n",
                pb_modal_count,
                modal_type ? modal_type : "(null)",
                title ? title : "(no title)",
                text ? text : "(no text)");
        fflush(pb_debug_log);
    }
    /* Also write to stderr for immediate visibility in test output */
    fprintf(stderr, "[MODAL #%d] type=%s title=\"%s\"\n",
            pb_modal_count,
            modal_type ? modal_type : "(null)",
            title ? title : "(no title)");
}

/* Log a generic debug message from PB code */
void pb_debug_log_msg(const char* category, const char* message) {
    if (pb_debug_log) {
        fprintf(pb_debug_log, "[%s] in %s (line %d): %s\n",
                category ? category : "DEBUG",
                pb_debug_current_func, pb_debug_line,
                message ? message : "");
        fflush(pb_debug_log);
    }
}

#ifdef _WIN32
/* Vectored exception handler — catches crashes and reports the current function */
static long __stdcall pb_crash_handler(void* exception_pointers) {
    /* EXCEPTION_POINTERS* ep = (EXCEPTION_POINTERS*)exception_pointers; */
    /* Extract exception record */
    unsigned long* ep = (unsigned long*)exception_pointers;
    unsigned long* er = (unsigned long*)ep[0]; /* EXCEPTION_RECORD* */
    unsigned long code = er[0]; /* ExceptionCode */
    unsigned long addr = er[3]; /* ExceptionAddress — offset 12 bytes (3 DWORDs) */

    /* Only catch fatal exceptions — ignore informational/debug exceptions */
    /* High bit 0xC = fatal, 0x4 = informational */
    if ((code & 0xF0000000) != 0xC0000000) {
        return 0; /* EXCEPTION_CONTINUE_SEARCH — let system handle it */
    }

    char msg[1024];
    sprintf(msg,
        "CRASH in function: %s\n"
        "Source line: %d\n"
        "Exception code: 0x%08lX\n"
        "Exception address: 0x%08lX\n"
        "\nCheck pb_debug.log for call trace.",
        pb_debug_current_func, pb_debug_line, code, addr);

    /* Write call trace to debug log */
    {
        FILE* trace_f = fopen("pb_debug.log", "w");
        if (trace_f) {
            fprintf(trace_f, "=== Call Trace (last %d entries) ===\n", PB_TRACE_SIZE);
            int start = (pb_trace_idx > PB_TRACE_SIZE) ? pb_trace_idx - PB_TRACE_SIZE : 0;
            for (int i = start; i < pb_trace_idx; i++) {
                const char* fn = pb_trace_buf[i % PB_TRACE_SIZE];
                if (fn) fprintf(trace_f, "[%d] %s\n", i - start, fn);
            }
            fprintf(trace_f, "\n!!! CRASH !!!\n%s\n", msg);
            fclose(trace_f);
        }
    }

    /* Also write to stderr */
    fprintf(stderr, "\n!!! RUNTIME CRASH !!!\n%s\n", msg);
    /* Print last 10 call trace entries to stderr */
    {
        int start = (pb_trace_idx > 10) ? pb_trace_idx - 10 : 0;
        fprintf(stderr, "Call trace (last entries):\n");
        for (int i = start; i < pb_trace_idx; i++) {
            const char* fn = pb_trace_buf[i % PB_TRACE_SIZE];
            if (fn) fprintf(stderr, "  [%d] %s\n", i - start, fn);
        }
    }
    fflush(stderr);

    /* Write crash info to a file that Electron/Claude can read */
    FILE* f = fopen("pb_crash.log", "w");
    if (f) {
        fprintf(f, "%s\n", msg);
        fclose(f);
    }

    ExitProcess(99);
    return 0; /* EXCEPTION_CONTINUE_SEARCH */
}
#endif

/* Called from main() wrapper before PBMAIN */
void pb_install_crash_handler(void) {
#ifdef _WIN32
    AddVectoredExceptionHandler(1, pb_crash_handler);
    pb_debug_init();
#endif
}

/* Public BSTR allocation wrapper — called from LLVM IR codegen (cdecl) */
char* pb_bstr_alloc(const char* src, unsigned int len) {
#ifdef _WIN32
    return SysAllocStringByteLen(src, len);
#else
    char* buf = (char*)malloc(len + 1);
    if (src) memcpy(buf, src, len);
    buf[len] = '\0';
    return buf;
#endif
}

/* Public BSTR free wrapper — called from LLVM IR codegen (cdecl) */
void pb_bstr_free(char* bstr) {
#ifdef _WIN32
    SysFreeString(bstr);
#else
    free(bstr);
#endif
}

/* Helper: allocate a BSTR from a C string buffer, then free the buffer */
static char* bstr_from_buf(char* buf) {
    int len = (int)strlen(buf);
    char* bstr = pb_bstr_alloc(buf, len);
    free(buf);
    return bstr;
}

/* ============================================================
 * FORMAT$ — Simplified PB number formatting
 *
 * PB FORMAT$ uses picture strings like "#,###.##", "0.00", etc.
 * This implementation covers the most common patterns:
 *   - "#" = digit or space
 *   - "0" = digit or zero
 *   - "." = decimal point
 *   - "," = thousands separator (if before ".")
 *   - "$" = dollar sign
 *   - "%" = percent (multiply by 100)
 *   - Any other characters are passed through literally
 * ============================================================ */
char* pb_format(double val, const char* fmt) {
    char buf[256];

    if (fmt == NULL || fmt[0] == '\0') {
        /* No format string — use default */
        snprintf(buf, sizeof(buf), "%g", val);
        char* result = (char*)malloc(strlen(buf) + 1);
        strcpy(result, buf);
        return bstr_from_buf(result);
    }

    /* Count decimal places from format string */
    int decimals = -1;
    int has_comma = 0;
    int has_dollar = 0;
    int has_percent = 0;
    const char* dot_pos = strchr(fmt, '.');

    if (dot_pos) {
        decimals = 0;
        for (const char* p = dot_pos + 1; *p == '#' || *p == '0'; p++) {
            decimals++;
        }
    }

    for (const char* p = fmt; *p; p++) {
        if (*p == ',') has_comma = 1;
        if (*p == '$') has_dollar = 1;
        if (*p == '%') has_percent = 1;
    }

    if (has_percent) val *= 100.0;

    /* Format the number */
    if (decimals >= 0) {
        snprintf(buf, sizeof(buf), "%.*f", decimals, val);
    } else {
        snprintf(buf, sizeof(buf), "%.0f", val);
    }

    if (has_comma) {
        /* Insert thousands separators */
        char formatted[256];
        char* src = buf;
        char* dst = formatted;
        int is_neg = 0;

        if (*src == '-') {
            *dst++ = *src++;
            is_neg = 1;
        }

        /* Find decimal point in formatted number */
        char* num_dot = strchr(src, '.');
        int int_len = num_dot ? (int)(num_dot - src) : (int)strlen(src);

        /* Copy integer part with commas */
        for (int i = 0; i < int_len; i++) {
            int remaining = int_len - i;
            if (i > 0 && remaining % 3 == 0) {
                *dst++ = ',';
            }
            *dst++ = src[i];
        }

        /* Copy decimal part */
        if (num_dot) {
            strcpy(dst, num_dot);
        } else {
            *dst = '\0';
        }

        strcpy(buf, formatted);
    }

    /* Build final result */
    int prefix_len = has_dollar ? 1 : 0;
    int suffix_len = has_percent ? 1 : 0;
    int buf_len = (int)strlen(buf);
    char* result = (char*)malloc(prefix_len + buf_len + suffix_len + 1);
    char* out = result;

    if (has_dollar) *out++ = '$';
    memcpy(out, buf, buf_len);
    out += buf_len;
    if (has_percent) *out++ = '%';
    *out = '\0';

    return bstr_from_buf(result);
}

/* ============================================================
 * PARSE$ — Split string by delimiter, extract Nth field (1-based)
 *
 * PARSE$(string, delimiter, index)  — returns the index'th field
 * PARSE$(string, delimiter)          — returns field count
 *
 * When index <= 0, returns the count of fields as a string-encoded integer.
 * When called with 3 args from PB: index is the field number (1-based).
 * ============================================================ */
char* pb_parse(const char* str, const char* delim, int index) {
    if (str == NULL || delim == NULL) {
        char* empty = (char*)malloc(1);
        empty[0] = '\0';
        return bstr_from_buf(empty);
    }

    int delim_len = (int)strlen(delim);
    if (delim_len == 0) {
        char* result = (char*)malloc(strlen(str) + 1);
        strcpy(result, str);
        return bstr_from_buf(result);
    }

    /* Count fields and find the index'th one */
    int field_num = 0;
    const char* p = str;
    const char* field_start = str;

    while (1) {
        /* Check for delimiter match (single-char: match any char in delim) */
        int found_delim = 0;
        if (delim_len == 1) {
            if (*p == delim[0]) found_delim = 1;
        } else {
            /* Multi-char delimiter: match exact substring */
            if (strncmp(p, delim, delim_len) == 0) found_delim = 1;
        }

        if (found_delim || *p == '\0') {
            field_num++;
            if (field_num == index) {
                /* Found the field we want */
                int len = (int)(p - field_start);
                char* result = (char*)malloc(len + 1);
                memcpy(result, field_start, len);
                result[len] = '\0';
                return bstr_from_buf(result);
            }
            if (*p == '\0') break;
            p += (delim_len == 1) ? 1 : delim_len;
            field_start = p;
        } else {
            p++;
        }
    }

    if (index <= 0) {
        /* Return field count */
        char buf[16];
        snprintf(buf, sizeof(buf), "%d", field_num);
        char* result = (char*)malloc(strlen(buf) + 1);
        strcpy(result, buf);
        return bstr_from_buf(result);
    }

    /* Field index out of range — return empty */
    char* empty = (char*)malloc(1);
    empty[0] = '\0';
    return bstr_from_buf(empty);
}

/* ============================================================
 * PARSECOUNT — Return number of fields in a delimited string
 * ============================================================ */
int pb_parsecount(const char* str, const char* delim) {
    if (str == NULL || delim == NULL || str[0] == '\0') return 0;

    int delim_len = (int)strlen(delim);
    if (delim_len == 0) return 1;

    int count = 1;
    const char* p = str;
    while (*p) {
        if (delim_len == 1) {
            if (*p == delim[0]) count++;
            p++;
        } else {
            if (strncmp(p, delim, delim_len) == 0) {
                count++;
                p += delim_len;
            } else {
                p++;
            }
        }
    }
    return count;
}

/* ============================================================
 * REPLACE — Replace all occurrences of old_str with new_str in target
 *
 * Modifies the target string pointer in-place (PB semantics).
 * ============================================================ */
void pb_replace(char** target, const char* old_str, const char* new_str) {
    if (target == NULL || *target == NULL || old_str == NULL || new_str == NULL) return;

    int old_len = (int)strlen(old_str);
    int new_len = (int)strlen(new_str);
    if (old_len == 0) return;

    /* Count occurrences */
    int count = 0;
    const char* p = *target;
    while ((p = strstr(p, old_str)) != NULL) {
        count++;
        p += old_len;
    }
    if (count == 0) return;

    /* Build result */
    int orig_len = (int)strlen(*target);
    int result_len = orig_len + count * (new_len - old_len);
    char* result = (char*)malloc(result_len + 1);
    char* dst = result;
    const char* src = *target;

    while (*src) {
        if (strncmp(src, old_str, old_len) == 0) {
            memcpy(dst, new_str, new_len);
            dst += new_len;
            src += old_len;
        } else {
            *dst++ = *src++;
        }
    }
    *dst = '\0';

    *target = bstr_from_buf(result);
}

/* ============================================================
 * USING$ — Format number using a PRINT USING-style format string
 *
 * For now, delegates to pb_format (which handles the common cases).
 * ============================================================ */
char* pb_using(const char* fmt, double val) {
    return pb_format(val, fmt);
}

/* ============================================================
 * REMOVE$ — Remove all occurrences of characters in chars from str
 * ============================================================ */
char* pb_remove(const char* str, const char* chars) {
    if (str == NULL) {
        char* empty = (char*)malloc(1);
        empty[0] = '\0';
        return bstr_from_buf(empty);
    }
    if (chars == NULL || chars[0] == '\0') {
        char* result = (char*)malloc(strlen(str) + 1);
        strcpy(result, str);
        return bstr_from_buf(result);
    }

    int len = (int)strlen(str);
    char* result = (char*)malloc(len + 1);
    char* dst = result;

    for (const char* p = str; *p; p++) {
        if (strchr(chars, *p) == NULL) {
            *dst++ = *p;
        }
    }
    *dst = '\0';
    return bstr_from_buf(result);
}

/* ============================================================
 * File I/O Runtime Functions
 * ============================================================ */

#define MAX_FILE_HANDLES 256
static FILE* file_handles[MAX_FILE_HANDLES] = {0};

int pb_freefile(void) {
    for (int i = 1; i < MAX_FILE_HANDLES; i++) {
        if (file_handles[i] == NULL) return i;
    }
    return 0;
}

int pb_open(const char* path, int mode, int filenum) {
    if (filenum < 1 || filenum >= MAX_FILE_HANDLES) return -1;
    const char* fmode;
    switch (mode) {
        case 0: fmode = "r"; break;   /* INPUT */
        case 1: fmode = "w"; break;   /* OUTPUT */
        case 2: fmode = "a"; break;   /* APPEND */
        default: fmode = "r"; break;
    }
    file_handles[filenum] = fopen(path, fmode);
    return (file_handles[filenum] != NULL) ? 0 : -1;
}

void pb_close(int filenum) {
    if (filenum >= 1 && filenum < MAX_FILE_HANDLES && file_handles[filenum]) {
        fclose(file_handles[filenum]);
        file_handles[filenum] = NULL;
    }
}

void pb_print_file(int filenum, const char* text) {
    if (filenum >= 1 && filenum < MAX_FILE_HANDLES && file_handles[filenum]) {
        fputs(text, file_handles[filenum]);
    }
}

void pb_print_file_newline(int filenum) {
    if (filenum >= 1 && filenum < MAX_FILE_HANDLES && file_handles[filenum]) {
        fputc('\n', file_handles[filenum]);
    }
}

char* pb_line_input(int filenum) {
    if (filenum < 1 || filenum >= MAX_FILE_HANDLES || !file_handles[filenum]) {
        char* empty = (char*)malloc(1);
        empty[0] = '\0';
        return bstr_from_buf(empty);
    }
    char buf[4096];
    if (fgets(buf, sizeof(buf), file_handles[filenum]) == NULL) {
        char* empty = (char*)malloc(1);
        empty[0] = '\0';
        return bstr_from_buf(empty);
    }
    /* Strip trailing newline */
    int len = (int)strlen(buf);
    if (len > 0 && buf[len-1] == '\n') {
        buf[len-1] = '\0';
        len--;
    }
    if (len > 0 && buf[len-1] == '\r') {
        buf[len-1] = '\0';
        len--;
    }
    char* result = (char*)malloc(len + 1);
    memcpy(result, buf, len);
    result[len] = '\0';
    return bstr_from_buf(result);
}

int pb_eof(int filenum) {
    if (filenum < 1 || filenum >= MAX_FILE_HANDLES || !file_handles[filenum]) return -1;
    /* PB EOF() returns true when no more data to read — need to peek ahead */
    int ch = fgetc(file_handles[filenum]);
    if (ch == EOF) return -1;  /* PB: -1 = true (EOF) */
    ungetc(ch, file_handles[filenum]);
    return 0;  /* PB: 0 = false (not EOF) */
}

void pb_input_file_str(int filenum, char** dest) {
    if (filenum < 1 || filenum >= MAX_FILE_HANDLES || !file_handles[filenum]) return;
    char buf[4096];
    int i = 0;
    int ch;
    while ((ch = fgetc(file_handles[filenum])) != EOF) {
        if (ch == ',' || ch == '\n') break;
        if (ch == '\r') continue;
        if (i < (int)sizeof(buf) - 1) buf[i++] = (char)ch;
    }
    buf[i] = '\0';
    char* result = (char*)malloc(i + 1);
    memcpy(result, buf, i + 1);
    *dest = bstr_from_buf(result);
}

void pb_input_file_int(int filenum, int* dest) {
    if (filenum < 1 || filenum >= MAX_FILE_HANDLES || !file_handles[filenum]) return;
    char buf[64];
    int i = 0;
    int ch;
    while ((ch = fgetc(file_handles[filenum])) != EOF) {
        if (ch == ',' || ch == '\n') break;
        if (ch == '\r') continue;
        if (i < (int)sizeof(buf) - 1) buf[i++] = (char)ch;
    }
    buf[i] = '\0';
    *dest = atoi(buf);
}

void pb_input_file_dbl(int filenum, double* dest) {
    if (filenum < 1 || filenum >= MAX_FILE_HANDLES || !file_handles[filenum]) return;
    char buf[64];
    int i = 0;
    int ch;
    while ((ch = fgetc(file_handles[filenum])) != EOF) {
        if (ch == ',' || ch == '\n') break;
        if (ch == '\r') continue;
        if (i < (int)sizeof(buf) - 1) buf[i++] = (char)ch;
    }
    buf[i] = '\0';
    *dest = atof(buf);
}

void pb_kill(const char* path) {
    if (path) remove(path);
}

/* ===== System builtins ===== */

#ifdef _WIN32
__declspec(dllimport) unsigned long __stdcall GetEnvironmentVariableA(const char* lpName, char* lpBuffer, unsigned long nSize);
__declspec(dllimport) unsigned long __stdcall GetModuleFileNameA(void* hModule, char* lpFilename, unsigned long nSize);
#endif

/* ENVIRON$("VARNAME") — returns environment variable value */
char* pb_environ(const char* var_name) {
    if (!var_name || !var_name[0]) {
        char* empty = (char*)malloc(1);
        empty[0] = '\0';
        return bstr_from_buf(empty);
    }
#ifdef _WIN32
    char buf[4096];
    unsigned long len = GetEnvironmentVariableA(var_name, buf, sizeof(buf));
    if (len == 0 || len >= sizeof(buf)) {
        char* empty = (char*)malloc(1);
        empty[0] = '\0';
        return bstr_from_buf(empty);
    }
    char* result = (char*)malloc(len + 1);
    memcpy(result, buf, len);
    result[len] = '\0';
    return bstr_from_buf(result);
#else
    const char* val = getenv(var_name);
    if (!val) val = "";
    int len = (int)strlen(val);
    char* result = (char*)malloc(len + 1);
    memcpy(result, val, len);
    result[len] = '\0';
    return bstr_from_buf(result);
#endif
}

/* EXE.PATH$ — returns directory of current executable */
char* pb_exe_path(void) {
#ifdef _WIN32
    char buf[4096];
    unsigned long len = GetModuleFileNameA(0, buf, sizeof(buf));
    if (len == 0) {
        char* empty = (char*)malloc(1);
        empty[0] = '\0';
        return bstr_from_buf(empty);
    }
    /* Strip filename, keep directory with trailing backslash */
    int i = (int)len - 1;
    while (i >= 0 && buf[i] != '\\' && buf[i] != '/') i--;
    if (i >= 0) buf[i + 1] = '\0';
    int plen = (int)strlen(buf);
    char* result = (char*)malloc(plen + 1);
    memcpy(result, buf, plen);
    result[plen] = '\0';
    return bstr_from_buf(result);
#else
    char* empty = (char*)malloc(1);
    empty[0] = '\0';
    return bstr_from_buf(empty);
#endif
}

/* EXE.NAME$ — returns filename of current executable */
char* pb_exe_name(void) {
#ifdef _WIN32
    char buf[4096];
    unsigned long len = GetModuleFileNameA(0, buf, sizeof(buf));
    if (len == 0) {
        char* empty = (char*)malloc(1);
        empty[0] = '\0';
        return bstr_from_buf(empty);
    }
    /* Find last path separator */
    int i = (int)len - 1;
    while (i >= 0 && buf[i] != '\\' && buf[i] != '/') i--;
    const char* name = buf + i + 1;
    int nlen = (int)strlen(name);
    char* result = (char*)malloc(nlen + 1);
    memcpy(result, name, nlen);
    result[nlen] = '\0';
    return bstr_from_buf(result);
#else
    char* empty = (char*)malloc(1);
    empty[0] = '\0';
    return bstr_from_buf(empty);
#endif
}

/* ===== Null-safe string concatenation ===== */

/* pb_str_concat — concatenates two strings, treating null as empty */
char* pb_str_concat(const char* a, const char* b) {
    static const char empty[] = "";
    if (!a) a = empty;
    if (!b) b = empty;
    int len_a = (int)strlen(a);
    int len_b = (int)strlen(b);
    int total = len_a + len_b;
    char* tmp = (char*)malloc(total + 1);
    memcpy(tmp, a, len_a);
    memcpy(tmp + len_a, b, len_b);
    tmp[total] = '\0';
    char* result = pb_bstr_alloc(tmp, total);
    free(tmp);
    return result;
}

/* ===== DATE$ and TIME$ builtins ===== */

#ifdef _WIN32
typedef struct {
    unsigned short wYear;
    unsigned short wMonth;
    unsigned short wDay;
    unsigned short wDayOfWeek;
    unsigned short wHour;
    unsigned short wMinute;
    unsigned short wSecond;
    unsigned short wMilliseconds;
} PB_SYSTEMTIME;

__declspec(dllimport) void __stdcall GetLocalTime(PB_SYSTEMTIME* lpSystemTime);
#endif

/* DATE$ — returns "MM-DD-YYYY" */
char* pb_date(void) {
    char buf[12];
#ifdef _WIN32
    PB_SYSTEMTIME st;
    GetLocalTime(&st);
    sprintf(buf, "%02d-%02d-%04d", st.wMonth, st.wDay, st.wYear);
#else
    strcpy(buf, "01-01-2026");
#endif
    return pb_bstr_alloc(buf, (int)strlen(buf));
}

/* TIME$ — returns "HH:MM:SS" */
char* pb_time(void) {
    char buf[10];
#ifdef _WIN32
    PB_SYSTEMTIME st;
    GetLocalTime(&st);
    sprintf(buf, "%02d:%02d:%02d", st.wHour, st.wMinute, st.wSecond);
#else
    strcpy(buf, "00:00:00");
#endif
    return pb_bstr_alloc(buf, (int)strlen(buf));
}

/* ===== Stubs for external DLL functions not yet available ===== */

#ifdef _WIN32
#define STDCALL __stdcall
#else
#define STDCALL
#endif

/* VARICHEK.DLL — HiScore: checks/updates high score table
 * Returns: 2 if highest score, else 1; x=-1 for check-only
 * Stubbed: linked directly into exe (cdecl, not dllimport) */
int HiScore(int x, const char* Winner, const char* PathSpec) {
    (void)x; (void)Winner; (void)PathSpec;
    return 1;  /* Not highest score */
}

/* Example DLL stub — RemarksData: returns STRPTR to a string
 * Stubbed: linked directly into exe (cdecl, not dllimport) */
int RemarksData(int x) {
    static char humor[] = "No remarks available.";
    (void)x;
    return (int)(size_t)humor;  /* Return pointer as integer (32-bit) */
}

/* EZLIB functions are now nooped in the compiler (should_noop_function).
 * No runtime stubs needed. */
