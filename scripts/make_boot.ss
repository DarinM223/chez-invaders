(compile-imported-libraries #t)
(compile-program "game.ss" "game.so")

(make-boot-file "game.boot"
  '("petite" "scheme")
  "arena.so"
  "utils.so"
  "intercept-alloc.so"
  "generational.so"
  "match.so"
  "ftypes.so"
  "coroutine.so"
  "raylib/raylib.so"
  "game.so")
