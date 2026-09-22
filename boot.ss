(define (safe-load-file filename)
  (guard (condition
          [else 
           (display "[Loader Error] Failed to compile or parse file: ")
           (display filename)
           (newline)])
    (load filename)))
