(define (read-text-file path)
  (call-with-input-file path get-string-all))

(define (write-embedded-shaders output shader-dir)
  (let loop ([names (directory-list shader-dir)]
             [files '()])
    (if (null? names)
        (let ([files (reverse files)])
          ;; Avoid appending to an output file left from a previous build.
          (when (file-exists? output)
            (delete-file output))

          (call-with-output-file output
            (lambda (port)
              (display "(define embedded-shader-sources\n  '(\n" port)
              (for-each
                (lambda (filename)
                  (let ([path (string-append shader-dir "/" filename)])
                    (write (cons path (read-text-file path)) port)
                    (newline port)))
                files)
              (display "  ))\n" port))))
        (let* ([name (car names)]
               [path (string-append shader-dir "/" name)])
          (loop (cdr names)
                (if (file-directory? path)
                    files
                    (cons name files)))))))

(putenv "BUILD_MODE_RELEASE" "1")

(write-embedded-shaders "embedded-shaders.ss" "shaders")

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
  "shader.so"
  "raylib/raylib.so"
  "game.so")
