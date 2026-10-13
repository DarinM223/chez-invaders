;; Need these imports for `define-generational` to work when compiling.
(import (raylib raylib (0 3)))
(import (game-types))

(compile-imported-libraries #t)
(compile-program "game.ss" "game.so")

(make-boot-file "game.boot"
  '("petite" "scheme")
  "arena.so"
  "utils.so"
  "game-types.so"
  "intercept-alloc.so"
  "generational.so"
  "match.so"
  "ftypes.so"
  "coroutine.so"
  "raylib/raylib.so"
  "game.so")