; (load-shared-object "./libraylib.so")
(import (raylib rlgl (0 3)))

;; game.ss

;; 1. Use top-level-bound? to protect game state from resets on reload
(define *player-pos* '(100.0 . 200.0))
(define *speed* 4.0)

;; 2. Logic loop (updates values)
(define (game-init)
  (display "Game logic initialized!\n"))

(define (game-update)
  ;; We can access Raylib functions if we bind them via Chez FFI,
  ;; or mutate variables locally.
  (let ((x (car *player-pos*))
        (y (cdr *player-pos*)))
    ;; Example simulation: simply drift the entity to show it's updating
    (set! *player-pos* (cons (+ x 0.5) y))))

;; 3. Render loop
(define (game-draw)
  ;; In a full framework, you'd use (foreign-procedure "DrawCircle" ...)
  ;; For this boilerplate, we'll let C fetch data, or write output:
  (draw-rectangle 0 0 100 100)
  '())
; (display "Player Position X: ")
; (display (car *player-pos*))
; (newline)

