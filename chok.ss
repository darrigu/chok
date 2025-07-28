#!/usr/bin/env scheme-script
(import (chezscheme))

(define-record-type ordered-hashtable
  (fields
   ht
   (mutable order)
   (mutable length))
  (protocol
   (lambda (new)
     (lambda (hash-function equiv-function)
       (new (make-hashtable hash-function equiv-function) '() 0)))))

(define (oht-get oht key . default)
  (if (hashtable-contains? (ordered-hashtable-ht oht) key)
    (hashtable-ref (ordered-hashtable-ht oht) key #f)
    (if (null? default)
      (error 'oht-get "key not found and no default value provided")
      (car default))))

(define (oht-set! oht key value)
  (let ([ht (ordered-hashtable-ht oht)])
    (unless (hashtable-contains? ht key)
      (ordered-hashtable-order-set! oht (cons key (ordered-hashtable-order oht)))
      (ordered-hashtable-length-set! oht (+ (ordered-hashtable-length oht) 1)))
    (hashtable-set! ht key value)))

(define (oht-update! oht key proc . default)
  (let* ([current-value (apply oht-get oht key default)]
         [new-value (proc current-value)])
    (oht-set! oht key new-value)
    new-value))

(define (oht-keys oht)
  (reverse (ordered-hashtable-order oht)))

(define (oht-values oht)
  (map (lambda (key) (oht-get oht key #f)) (oht-keys oht)))

(define (oht-clear! oht)
  (hashtable-clear! (ordered-hashtable-ht oht))
  (ordered-hashtable-order-set! oht '())
  (ordered-hashtable-length-set! oht 0))

(define (usage port)
  (display "Usage:\n" port)
  (display "  chok from-txt <input.txt> <output.chok>\n" port)
  (display "  chok to-dot <input.chok> <output.dot>\n" port)
  (display "  chok inspect <input.chok>\n" port))

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
        (unless (and (pair? pair) (not (pair? (cdr pair))))
          (error 'dump-pairs (format "invalid pair at index ~d: ~s" i pair)))
        (bytevector-u32-set! buffer (* i 8) (car pair) (endianness little))
        (bytevector-u32-set! buffer (+ (* i 8) 4) (cdr pair) (endianness little)))
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
                        (cons (cons (bytevector-u32-ref buffer
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
          (fprintf port "  ~d -> ~d\n" token (cdr pair))))))
  (display "}\n" port))

(define (process-tokens tokens freqs)
  (oht-clear! freqs)
  (let loop ([prev (car tokens)] [remaining (cdr tokens)])
    (unless (null? remaining)
      (oht-update! freqs (cons prev (car remaining)) add1 0)
      (loop (car remaining) (cdr remaining)))))

(define (find-max-freq freqs)
  (let ([max-freq (cons '() 0)])
    (for-each
      (lambda (key value)
        (when (> value (cdr max-freq))
          (set! max-freq (cons key value))))
      (oht-keys freqs) (oht-values freqs))
    max-freq))

(define (replace-tokens tokens max-pair new-token)
  (let process ([remaining tokens] [acc '()])
    (cond
      [(null? remaining) (reverse acc)]
      [(null? (cdr remaining)) (reverse (cons (car remaining) acc))]
      [else
        (let ([pair (cons (car remaining) (cadr remaining))])
          (if (equal? pair max-pair)
            (process (cddr remaining) (cons new-token acc))
            (process (cdr remaining) (cons (car remaining) acc))))])))

(define (main-from-txt args)
  (validate-args args 1 "no input path was provided")
  (validate-args args 2 "no output path was provided")

  (let* ([input-path (car args)]
         [output-path (cadr args)]
         [text (call-with-input-file input-path get-string-all)]
         [freqs (make-ordered-hashtable equal-hash equal?)]
         [pairs (reverse (map (lambda (n) (cons n 0)) (iota 256)))]
         [tokens-in (map char->integer (string->list text))])
    (printf "Info: processing ~d initial tokens...\n" (length tokens-in))
    (call/cc
      (lambda (break)
        (let loop ([iteration 0])
          (process-tokens tokens-in freqs)
          (let ([max-freq (find-max-freq freqs)])
            (when (<= (cdr max-freq) 1) (break))

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

(define (render-token pair-vector token port)
  (let* ([pair (vector-ref pair-vector token)]
         [left (car pair)]
         [right (cdr pair)])
    (if (= token left)
      (display (integer->char token) port)
      (begin
        (render-token pair-vector left port)
        (render-token pair-vector right port)))))

(define (main-inspect args)
  (validate-args args 1 "no input path was provided")

  (let* ([pairs (load-pairs (car args))]
         [pair-vector (list->vector (reverse pairs))]
         [str (open-output-string)])
    (do ([token 1 (+ token 1)])
        ((>= token (length pairs)))
      (printf "~d => |" token)
      (render-token pair-vector token str)
      (string-for-each
        (lambda (char)
          (if (char>=? char #\space)
            (display char)
            (printf "\\x~2,'0x" (char->integer char))))
        (get-output-string str))
      (display "|\n"))))

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
        ("inspect" (main-inspect (cdr args)))
        (else
          (usage (current-error-port))
          (error 'chok (format "unknown command '~a'" command)))))))

(main (cdr (command-line)))
