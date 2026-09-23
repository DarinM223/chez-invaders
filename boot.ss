(define (safe-load-file filename)
  (guard (condition
          [else
           (display "[Loader Error] Failed to compile or parse file: ")
           (display filename)
           (newline)])
    (load filename)))

(import (chezscheme))

(define (eval-string source)
  (let ([in  (open-input-string source)]
        [out (open-output-string)])
    (define (loop)
      (guard (condition
              [else
               (display-condition condition out)
               (newline out)
               (get-output-string out)])
        (let ([form (read in)])
          (if (eof-object? form)
              (get-output-string out)
              (begin
                (let ([result (eval form)])
                  (unless (eq? result (void))
                    (write result out)
                    (newline out)))
                (loop))))))
    (loop)))
