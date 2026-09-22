#include "raylib.h"
#include "scheme.h" // Provided by Chez Scheme installation
#include <stdio.h>
#include <stdlib.h>

// Helper to safely load/reload the scheme file
void reload_game_logic()
{
    printf("[Engine] Reloading game.ss...\n");
    // Equivalent to executing (load "game.ss") in Scheme
    ptr load_symbol = Stop_level_value(Sstring_to_symbol("load"));
    Scall1(load_symbol, Sstring("game.ss"));
}

int main(int argc, char *argv[])
{
    // 1. Initialize Chez Scheme
    Sscheme_init(NULL);

    // Register the fundamental boot files required by the Chez VM
    // (Adjust these paths to your native Chez Scheme installation directory)
    Sregister_boot_file("/usr/lib/csv10.4.1/ta6le/petite.boot");
    Sregister_boot_file("/usr/lib/csv10.4.1/ta6le/scheme.boot");
    Sbuild_heap(NULL, NULL);

    // Load game logic initially
    reload_game_logic();

    // Call game-init from Scheme
    ptr game_init = Stop_level_value(Sstring_to_symbol("game-init"));
    if (Sprocedurep(game_init)) {
        Scall0(game_init);
    }

    // 2. Initialize Raylib
    InitWindow(800, 450, "Raylib + Chez Scheme Hot Reloading");
    SetTargetFPS(60);

    // 3. Main Game Loop
    while (!WindowShouldClose()) {
        // Trigger Hot Reload on F5 keypress
        if (IsKeyPressed(KEY_P)) {
            printf("Hot reloading....\n");
            reload_game_logic();
        }

        // Fetch frame pointers dynamically in case they were updated/redefined
        ptr game_update = Stop_level_value(Sstring_to_symbol("game-update"));
        ptr game_draw = Stop_level_value(Sstring_to_symbol("game-draw"));

        // Update step
        if (Sprocedurep(game_update)) {
            Scall0(game_update);
        }

        // Draw step
        BeginDrawing();
        ClearBackground(RAYWHITE);
        DrawText("Press p to Hot Reload game.ss", 10, 10, 20, MAGENTA);

        if (Sprocedurep(game_draw)) {
            Scall0(game_draw);
        }

        EndDrawing();
    }

    // 4. Cleanup
    CloseWindow();
    Sscheme_deinit();

    return 0;
}
