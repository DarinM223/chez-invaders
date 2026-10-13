(library (game-types)
  (export ShipEnemy Bullet Star)
  (import (except (chezscheme) foreign-alloc)
          (raylib raylib (0 3)))

  (define-ftype ShipEnemy
    (struct
      [position Vector-2]
      [angle double]
      [state integer-8]))

  (define-ftype Bullet
    (struct
      [position Vector-2]
      [velocity Vector-2]))

  (define-ftype Star
    (struct
      [position Vector-2]
      [depth integer-8]
      [color Color])))
