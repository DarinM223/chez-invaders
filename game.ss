(import (raylib raylib (0 3)))

(define-syntax define-toplevel
  (lambda (stx)
    (syntax-case stx ()
      [(_ id value) (identifier? #'id)
       #'(unless (top-level-bound? (quote id))
           (define-top-level-value (quote id) value))])))

(define (safe-call thunk)
  (guard (condition
          [else
           (display "[Scheme Error]: ")
           (display-condition condition)
           (newline)])
    (thunk)))

(define-toplevel rectangles
  (list
   (make-rectangle 50.0 50.0 100.0 100.0)
   (make-rectangle 100.0 100.0 100.0 100.0)))

(define (drop-rectangle rect)
  (when (<= (+ (rectangle-get rect (height))
               (rectangle-get rect (y))) (get-screen-height))
    (rectangle-set! rect (y) (fl+ 1.0 (rectangle-get rect (y))))))

(define (user-game-init)
  (display "Game logic initialized!\n"))

(define (user-game-update)
  (for-each drop-rectangle rectangles))

(define (user-game-draw)
  (for-each (lambda (rect) (draw-rectangle-rec rect BLACK)) rectangles)
  (draw-circle 200 200 50.0 BLUE)
  (collect))

(define (game-init) (safe-call user-game-init))
(define (game-update) (safe-call user-game-update))
(define (game-draw) (safe-call user-game-draw))
