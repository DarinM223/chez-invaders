#include "raylib.h"
#include "repl.h"
#include "scheme.h" // Provided by Chez Scheme installation
#include <stdio.h>

// Helper to safely load/reload the scheme file
void reload_game_logic()
{
    printf("[Engine] Reloading game.ss...\n");
    ptr safe_load_proc = Stop_level_value(Sstring_to_symbol("safe-load-file"));
    if (!Sprocedurep(safe_load_proc)) {
        printf("[Fatal C Error] safe-load routine is not bound in Scheme "
               "memory.\n");
        return;
    }

    Scall1(safe_load_proc, Sstring("game.ss"));
}

typedef enum main_state {
    STATE_RUNNING = 1,
    STATE_REPL = 2,
} main_state;

int main(int argc, char *argv[])
{
    Sscheme_init(NULL);

    // Register the fundamental boot files required by the Chez VM
    // (Adjust these paths to your native Chez Scheme installation directory)
    Sregister_boot_file("/usr/lib/csv10.4.1/ta6le/petite.boot");
    Sregister_boot_file("/usr/lib/csv10.4.1/ta6le/scheme.boot");
    Sbuild_heap(NULL, NULL);

    ptr load_symbol = Stop_level_value(Sstring_to_symbol("load"));
    Scall1(load_symbol, Sstring("boot.ss"));
    reload_game_logic();

    ptr game_init = Stop_level_value(Sstring_to_symbol("game-init"));
    if (Sprocedurep(game_init)) {
        Scall0(game_init);
    }
    InitWindow(800, 450, "Raylib + Chez Scheme Hot Reloading");
    SetTargetFPS(60);

    main_state state = STATE_RUNNING;
    repl repl = {0};
    while (!WindowShouldClose()) {
        // Trigger Hot Reload on 'p' keypress
        if (IsKeyPressed(KEY_P)) {
            printf("Hot reloading....\n");
            reload_game_logic();
        }

        ptr game_update = Stop_level_value(Sstring_to_symbol("game-update"));
        if (Sprocedurep(game_update)) {
            Scall0(game_update);
        }

        if (IsKeyPressed(KEY_GRAVE)) {
            GetCharPressed(); // Eat grave key to prevent it from being added to
                              // input buffer.
            if (state == STATE_REPL) {
                state = STATE_RUNNING;
            } else {
                state = STATE_REPL;
            }
        }

        if (state == STATE_REPL) {
            repl_update(&repl);
        }

        BeginDrawing();
        ClearBackground(RAYWHITE);

        ptr game_draw = Stop_level_value(Sstring_to_symbol("game-draw"));
        if (Sprocedurep(game_draw)) {
            Scall0(game_draw);
        }

        if (state == STATE_REPL) {
            repl_draw(&repl);
        } else {
            DrawText("Press p to hot reload, ` to go to REPL", 10, 10, 20,
                     MAGENTA);
        }

        EndDrawing();
    }

    CloseWindow();
    Sscheme_deinit();
    return 0;
}
