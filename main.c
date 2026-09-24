#include "inotify_watcher.h"
#include "raylib.h"
#include "repl.h"
#include "scheme.h" // Provided by Chez Scheme installation
#include <stdio.h>
#include <string.h>

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

const char *get_filename_ext(const char *filename)
{
    const char *dot = strrchr(filename, '.');
    if (!dot || dot == filename) return "";
    return dot + 1;
}

void inotify_callback(const char *filename, void *ctx)
{
    (void)ctx;
    printf("File changed: %s\n", filename);
    const char *ext = get_filename_ext(filename);
    if (!ext) {
        printf("Failure getting file extension\n");
        return;
    }
    // Reload scheme files and shaders.
    if (strcmp(ext, "ss") == 0 || strcmp(ext, "scm") == 0 ||
        strcmp(ext, "sps") == 0 || strcmp(ext, "sls") == 0 ||
        strcmp(ext, "sld") == 0 || strcmp(ext, "glsl") == 0 ||
        strcmp(ext, "glslv") == 0 || strcmp(ext, "glslf") == 0 ||
        strcmp(ext, "vert") == 0 || strcmp(ext, "frag") == 0) {
        reload_game_logic();
    } else {
        printf("Unrecognized file extension: %s\n", ext);
    }
}

int main(int argc, char *argv[])
{
    Sscheme_init(NULL);
    inotify_watcher watcher = {.callback = inotify_callback};
    inotify_error err;
    if ((err = inotify_watcher_init(&watcher)) ||
        (err = inotify_watcher_add_watch(&watcher, "."))) {
        printf("Got error initializing inotify\n");
        return err;
    }

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

    double last_time = GetTime();
    bool reload_on_save = false;
    while (!WindowShouldClose()) {
        // Trigger Hot Reload on 'p' keypress
        if (IsKeyPressed(KEY_P)) {
            if (IsKeyDown(KEY_LEFT_SHIFT) || IsKeyDown(KEY_RIGHT_SHIFT)) {
                reload_on_save = !reload_on_save;
                printf("Toggling reload on save to %b\n", reload_on_save);
                inotify_watcher_drain_files(&watcher);
            } else {
                reload_game_logic();
            }
        }

        if (reload_on_save) {
            double current_time = GetTime();
            if (current_time - last_time >= 1) {
                last_time = current_time;
                inotify_watcher_watch_files(&watcher, NULL);
            }
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

        switch (state) {
        case STATE_RUNNING: {
            ptr game_update =
                Stop_level_value(Sstring_to_symbol("game-update"));
            if (Sprocedurep(game_update)) {
                Scall0(game_update);
            }
            break;
        }
        case STATE_REPL:
            repl_update(&repl);
            break;
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
            DrawText(
                "Press p to reload, P to reload on save, and ` to go to REPL",
                10, 10, 20, reload_on_save ? RED : MAGENTA);
        }

        EndDrawing();
    }

    CloseWindow();
    Sscheme_deinit();
    return 0;
}
