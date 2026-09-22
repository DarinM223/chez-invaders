#include "raylib.h"
#include "scheme.h" // Provided by Chez Scheme installation
#include <stdio.h>
#include <string.h>

// Helper to safely load/reload the scheme file
void reload_game_logic()
{
    printf("[Engine] Reloading game.ss...\n");
    ptr load_symbol = Stop_level_value(Sstring_to_symbol("load"));
    Scall1(load_symbol, Sstring("boot.ss"));

    ptr safe_load_proc = Stop_level_value(Sstring_to_symbol("safe-load-file"));
    if (!Sprocedurep(safe_load_proc)) {
        printf("[Fatal C Error] safe-load routine is not bound in Scheme "
               "memory.\n");
        return;
    }

    Scall1(safe_load_proc, Sstring("game.ss"));
}

int main(int argc, char *argv[])
{
    Sscheme_init(NULL);

    // Register the fundamental boot files required by the Chez VM
    // (Adjust these paths to your native Chez Scheme installation directory)
    Sregister_boot_file("/usr/lib/csv10.4.1/ta6le/petite.boot");
    Sregister_boot_file("/usr/lib/csv10.4.1/ta6le/scheme.boot");
    Sbuild_heap(NULL, NULL);

    reload_game_logic();

    ptr game_init = Stop_level_value(Sstring_to_symbol("game-init"));
    if (Sprocedurep(game_init)) {
        Scall0(game_init);
    }
    InitWindow(800, 450, "Raylib + Chez Scheme Hot Reloading");
    SetTargetFPS(60);

    while (!WindowShouldClose()) {
        // Trigger Hot Reload on 'p' keypress
        if (IsKeyPressed(KEY_P)) {
            printf("Hot reloading....\n");
            reload_game_logic();
        }

        // Fetch frame pointers dynamically in case they were updated/redefined
        ptr game_update = Stop_level_value(Sstring_to_symbol("game-update"));
        ptr game_draw = Stop_level_value(Sstring_to_symbol("game-draw"));

        if (Sprocedurep(game_update)) {
            Scall0(game_update);
        }

        BeginDrawing();
        ClearBackground(RAYWHITE);
        if (Sprocedurep(game_draw)) {
            Scall0(game_draw);
        }
        DrawText("Press p to Hot Reload game.ss", 10, 10, 20, MAGENTA);
        EndDrawing();
    }

    CloseWindow();
    Sscheme_deinit();
    return 0;
}
