#!/usr/bin/env scheme-script
(import (chezscheme))

(define (usage port)
  (display "Usage:\n" port)
  (display "  chok from-txt <input.txt> <output.chok>\n" port)
  (display "  chok to-dot <input.chok> <output.dot>\n" port))

(define (validate-args args min-count error-message)
  (when (< (length args) min-count)
    (usage (current-error-port))
    (error 'chok error-message)))

(define (dump-pairs file-path pairs)
  (unless (list? pairs)
    (error 'dump-pairs "expected list of pairs"))

  (let* ([pair-count (length pairs)]
         [buffer (make-bytevector (* pair-count 8))])
    (for-each
      (lambda (pair i)
        (unless (and (list? pair) (= 2 (length pair)))
          (error 'dump-pairs (format "invalid pair at index ~d: ~s" i pair)))
        (bytevector-u32-set! buffer (* i 8) (car pair) (endianness little))
        (bytevector-u32-set! buffer (+ (* i 8) 4) (cadr pair) (endianness little)))
      pairs (iota pair-count))

    (call-with-port
      (open-file-output-port file-path (file-options no-fail))
      (lambda (port)
        (put-bytevector port buffer)
        (unless (= (port-position port) (bytevector-length buffer))
          (error 'chok "incomplete write operation"))))))

(define (load-pairs file-path)
  (call-with-port
    (open-file-input-port file-path)
    (lambda (port)
      (let ([file-size (port-length port)])
        (cond
          ((zero? file-size)
           (error 'chok "empty file"))
          ((not (zero? (mod file-size 8)))
           (error 'chok (format "file size ~d not divisible by 8"
                                file-size)))
          (else
            (let* ([buffer (make-bytevector file-size)]
                   [bytes-read (get-bytevector-n! port buffer 0 file-size)])
              (unless (= bytes-read file-size)
                (error 'chok (format "read ~d bytes, expected ~d"
                                     bytes-read file-size)))
              (let loop ([i 0] [pairs '()])
                (if (>= (* i 8) file-size)
                  (reverse pairs)
                  (loop (+ i 1)
                        (cons (list (bytevector-u32-ref buffer
                                                        (* i 8)
                                                        (endianness little))
                                    (bytevector-u32-ref buffer
                                                        (+ (* i 8) 4)
                                                        (endianness little)))
                              pairs)))))))))))

(define (render-dot pairs port)
  (display "digraph Pairs {\n" port)
  (let ([pair-vector (list->vector (reverse pairs))]
        [token-count (length pairs)])
    (do ([token 0 (+ token 1)])
        ((>= token token-count))
      (let ([pair (vector-ref pair-vector token)])
        (unless (= token (car pair))
          (fprintf port "  ~d -> ~d\n" token (car pair))
          (fprintf port "  ~d -> ~d\n" token (cadr pair))))))
  (display "}\n" port))

(define (process-tokens tokens freqs)
  (hashtable-clear! freqs)
  (let loop ([prev (car tokens)] [remaining (cdr tokens)])
    (unless (null? remaining)
      (hashtable-update! freqs (list prev (car remaining)) add1 0)
      (loop (car remaining) (cdr remaining)))))

(define (find-max-freq freqs)
  (let ([max-freq (list '() 0)])
    (let-values ([(keys values) (hashtable-entries freqs)])
      (vector-for-each
        (lambda (key value)
          (when (> value (cadr max-freq))
            (set! max-freq (list key value))))
        keys values))
    max-freq))

(define (replace-tokens tokens max-pair new-token)
  (let process ([remaining tokens] [acc '()])
    (cond
      [(null? remaining) (reverse acc)]
      [(null? (cdr remaining)) (reverse (cons (car remaining) acc))]
      [else
        (let ([pair (list (car remaining) (cadr remaining))])
          (if (equal? pair max-pair)
            (process (cddr remaining) (cons new-token acc))
            (process (cdr remaining) (cons (car remaining) acc))))])))

(define (main-from-txt args)
  (validate-args args 1 "no input path was provided")
  (validate-args args 2 "no output path was provided")

  (let* ([input-path (car args)]
         [output-path (cadr args)]
         [text (call-with-input-file input-path get-string-all)]
         [freqs (make-hashtable equal-hash equal?)]
         [pairs (reverse (map (lambda (n) (list n 0)) (iota 256)))]
         [tokens-in (map char->integer (string->list text))])
    (printf "Info: processing ~d initial tokens...\n" (length tokens-in))
    (call/cc
      (lambda (break)
        (let loop ([iteration 0])
          (process-tokens tokens-in freqs)
          (let ([max-freq (find-max-freq freqs)])
            (when (<= (cadr max-freq) 1) (break))

            (set! pairs (cons (car max-freq) pairs))
            (set! tokens-in (replace-tokens tokens-in (car max-freq)
                                            (+ 256 iteration)))
            (printf "\rIteration ~d: ~d tokens remaining"
                    iteration
                    (length tokens-in))
            (loop (+ iteration 1))))))
    (display "\r")
    (dump-pairs output-path pairs)
    (printf "Info: generated ~d token pairs in ~a\n" (length pairs) output-path)))

(define (main-to-dot args)
  (validate-args args 1 "no input path was provided")
  (validate-args args 2 "no output path was provided")

  (let ([input-path (car args)]
        [output-path (cadr args)])
    (call-with-port
      (open-file-output-port output-path
                             (file-options no-fail)
                             'block
                             (native-transcoder))
      (lambda (port)
        (render-dot (load-pairs input-path) port)))
    (printf "Info: generated dot file ~a~%" output-path)))

(define (main args)
  (validate-args args 1 "no command was provided")

  (guard (ex
           ((and (error? ex)
                 (eq? (condition-who ex) 'chok))
            (fprintf (current-error-port) "Error: ~a~%" (condition-message ex))
            (exit 1))
           (else (raise ex)))
    (let ([command (car args)])
      (case command
        ("from-txt" (main-from-txt (cdr args)))
        ("to-dot" (main-to-dot (cdr args)))
        (else
          (usage (current-error-port))
          (error 'chok (format "unknown command '~a'" command)))))))

(main (cdr (command-line)))
