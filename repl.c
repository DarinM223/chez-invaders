#include "repl.h"
#include "raylib.h"
#include "scheme.h"
#include <stddef.h>
#include <stdio.h>
#include <string.h>

size_t scheme_eval(ptr eval_string_proc, const char *source,
                   size_t source_length, char *output, size_t output_capacity)
{
    if (output_capacity <= 0) return 0;

    ptr source_string = Sstring_utf8(source, source_length);
    ptr result = Scall1(eval_string_proc, source_string);
    size_t result_length = Sstring_length(result);
    if (result_length >= output_capacity) {
        result_length = output_capacity - 1;
    }
    for (size_t i = 0; i < result_length; i++) {
        output[i] = Sstring_ref(result, i);
    }
    output[result_length] = '\0';
    return result_length;
}

static inline int16_t repl_history_prev(int16_t index)
{
    return index - 1 < 0 ? HISTORY_CAPACITY - 1 : index - 1;
}

static inline int16_t repl_history_next(int16_t index)
{
    return (index + 1) % HISTORY_CAPACITY;
}

static void repl_history_print(const repl_history *history, FILE *out)
{
    fprintf(out, "History:\n");
    for (int16_t i = history->start; i != history->end;
         i = repl_history_next(i)) {
        fprintf(out, "History at %d: %s\n", i, history->data[i]);
    }
}

static void repl_history_add(repl_history *history, const char *output)
{
    strcpy(history->data[history->end], output);
    history->end = repl_history_next(history->end);
    if (history->end == history->start) {
        history->start = repl_history_next(history->start);
    }
}

static void repl_scroll_up(repl *repl)
{
    if (repl->history.dirty) {
        repl->history.current_selection = repl->history.end;
        repl->history.dirty = false;
    }
    if (repl->history.current_selection != repl->history.start) {
        repl->history.current_selection =
            repl_history_prev(repl->history.current_selection);
    }
    strcpy(repl->input_buf,
           repl->history.data[repl->history.current_selection]);
    repl->input_buf_count = strlen(repl->input_buf);
}

static void repl_scroll_down(repl *repl)
{
    if (repl->history.dirty) {
        repl->history.current_selection = repl->history.end;
        repl->history.dirty = false;
    }
    if (repl->history.current_selection != repl->history.end) {
        repl->history.current_selection =
            repl_history_next(repl->history.current_selection);
    }
    if (repl->history.current_selection == repl->history.end) {
        memset(repl->input_buf, 0, TEXT_CAPACITY);
        repl->input_buf_count = 0;
    } else {
        strcpy(repl->input_buf,
               repl->history.data[repl->history.current_selection]);
        repl->input_buf_count = strlen(repl->input_buf);
    }
}

void repl_update(repl *repl)
{
    if (IsKeyPressed(KEY_ENTER)) {
        // If keypress is enter, then eval into output buffer.
        ptr eval = Stop_level_value(Sstring_to_symbol("eval-string"));
        if (Sprocedurep(eval)) {
            printf("Evaluating %s\n", repl->input_buf);
            repl_history_add(&repl->history, repl->input_buf);
            size_t result_length =
                scheme_eval(eval, repl->input_buf, repl->input_buf_count,
                            repl->output_buf, TEXT_CAPACITY);
        }
        repl->input_buf_count = 0;
        repl->input_buf[repl->input_buf_count] = '\0';
        repl->history.dirty = true;
    } else if (IsKeyDown(KEY_BACKSPACE)) {
        repl->history.dirty = true;
        double current_time = GetTime();
        if (current_time - repl->delay_start < 0.06) {
            return;
        }
        // If keypress is backspace, remove character from input buffer.
        if (repl->input_buf_count > 0) {
            repl->input_buf_count--;
            repl->input_buf[repl->input_buf_count] = '\0';
        }
        repl->delay_start = current_time;
    } else if (IsKeyPressed(KEY_UP)) {
        repl_scroll_up(repl);
    } else if (IsKeyPressed(KEY_DOWN)) {
        repl_scroll_down(repl);
    } else if (IsKeyPressed(KEY_V) &&
               (IsKeyDown(KEY_LEFT_CONTROL) || IsKeyDown(KEY_RIGHT_CONTROL))) {
        const char *clipboard = GetClipboardText();
        if (clipboard) {
            repl->history.dirty = true;
            size_t i = 0;
            size_t clipboard_end = repl->input_buf_count + strlen(clipboard);
            while (repl->input_buf_count < TEXT_CAPACITY - 1 &&
                   repl->input_buf_count < clipboard_end) {
                repl->input_buf[repl->input_buf_count++] = clipboard[i++];
            }
            repl->input_buf[repl->input_buf_count] = '\0';
        }
    } else {
        // Listen to keypresses, then add key to input buffer.
        char ch;
        if ((ch = GetCharPressed()) &&
            repl->input_buf_count < TEXT_CAPACITY - 1) {
            repl->input_buf[repl->input_buf_count++] = ch;
            repl->input_buf[repl->input_buf_count] = '\0';
            repl->history.dirty = true;
        }
    }
}

void repl_draw(repl const *repl)
{
    DrawRectangle(0, 0, GetScreenWidth(), 40, BLACK);
    DrawText("> ", 0, 0, 20, WHITE);
    DrawText(repl->input_buf, 20, 0, 20, WHITE);
    DrawText(repl->output_buf, 0, 20, 20, WHITE);
}
