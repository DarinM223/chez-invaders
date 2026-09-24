#ifndef REPL_H
#define REPL_H

#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#define TEXT_CAPACITY 512
#define HISTORY_CAPACITY 5

typedef struct repl_history {
    char data[HISTORY_CAPACITY + 1][TEXT_CAPACITY];
    bool dirty;
    int16_t current_selection;
    int16_t start;
    int16_t end;
} repl_history;

typedef struct repl {
    char input_buf[TEXT_CAPACITY];
    size_t input_buf_count;
    char output_buf[TEXT_CAPACITY];
    repl_history history;
    double delay_start;
} repl;

void repl_update(repl *repl);
void repl_draw(repl const *repl);

#endif // REPL_H