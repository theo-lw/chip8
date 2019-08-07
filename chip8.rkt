#lang typed/racket

(require typed/racket/gui/base)
(require "structs.rkt")

(provide (except-out (prefix-out chip8/ (all-defined-out)) sprites delay-timer sound-timer core-dump))

(: load-file (-> Path-String Void))
(define (load-file fpath)
  (with-input-from-file fpath (lambda () (load-memory PC))))

(: load-memory (-> Integer Void))
(define (load-memory index)
  (when (not (eof-object? (peek-byte)))
    (let ([b (read-byte)])
      (when (integer? b)
        (begin
          (vector-set! memory index b)
          (load-memory (+ 1 index)))))))

(: memory (Vectorof Integer))
(define memory (make-vector 4096 0))

(define V (make-vector 16 0))
(define I 0)
(define DT 0)
(define ST 0)
(define PC 512)
(define SP 0)
(define stack (make-vector 16 0))

(define (reset!)
  (set! memory (make-vector 4096 0))
  (set! V (make-vector 16 0))
  (set! I 0)
  (set! DT 0)
  (set! ST 0)
  (set! PC 512)
  (set! SP 0)
  (set! stack (make-vector 16 0))
  (vector-copy! memory 0 sprites))

(: delay-timer (Instance Timer%))
(define delay-timer (new timer% [notify-callback 
                                    (lambda () (if (zero? DT)
                                                    (send delay-timer stop)
                                                    (set! DT (sub1 DT))))] 
                                [interval 16]))

(: sound-timer (Instance Timer%))
(define sound-timer (new timer% [notify-callback 
                                    (lambda () (if (zero? ST)
                                                    (send sound-timer stop)
                                                    (set! ST (sub1 ST))))]
                                [interval 16]))

(: set-DT! (-> Integer Void))
(define (set-DT! value) (set! DT value) (send delay-timer start 16))

(: set-ST! (-> Integer Void))
(define (set-ST! value) (set! ST value) (send sound-timer start 16))

(: set-V! (-> Integer Integer Void))
(define (set-V! index value) (vector-set! V index value))

(: V-at (-> Integer Integer))
(define (V-at index) (vector-ref V index))

(: set-I! (-> Integer Void))
(define (set-I! value) (set! I value))

(: push-stack! (-> Integer Void))
(define (push-stack! value) (vector-set! stack SP value) (set! SP (add1 SP)))

(: pop-stack! (-> Void))
(define (pop-stack!) (set! SP (sub1 SP)))

(: top-stack (-> Integer))
(define (top-stack) (vector-ref stack (sub1 SP)))

(: fetch-instruction (-> instruction))
(define (fetch-instruction)
  
    (: first-nibble (-> Integer Integer))
    (define (first-nibble byte) (quotient byte 16))

    (: second-nibble (-> Integer Integer))
    (define (second-nibble byte) (modulo byte 16))
  
    (define first-byte (vector-ref memory PC))
    (define second-byte (vector-ref memory (add1 PC)))

    (if (and (exact-integer? first-byte) (exact-integer? second-byte))
        (instruction (first-nibble first-byte) (second-nibble first-byte) (first-nibble second-byte) (second-nibble second-byte))
        (error "Bad instruction!")))

(define (increment-PC!) (set! PC (+ PC 2)))

(: set-PC! (-> Integer Void))
(define (set-PC! value) (set! PC (- value 2)))

(: set-memory! (-> Integer Integer Void))
(define (set-memory! index value) (vector-set! memory index value))

(: memory-at (-> Integer Integer))
(define (memory-at index) (vector-ref memory index))
(define (core-dump) (display "Program Counter: ")
                    (pretty-display PC)
                    (display "Memory: ")
                    (pretty-display memory)
                    (display "Registers: ")
                    (pretty-display V)
                    (display "Register I: ")
                    (pretty-display I)
                    (display "Delay Timer: ")
                    (pretty-display DT)
                    (display "Sound Timer: ")
                    (pretty-display ST)
                    (display "Stack Pointer: ")
                    (pretty-display SP)
                    (display "Stack: ")
                    (pretty-display stack))

(: core-dump-file (-> Path-String Void))
(define (core-dump-file fpath) (with-output-to-file fpath (lambda () (core-dump))))

(: sprites (Vectorof Integer))
(define sprites (vector
    #b11110000
    #b10010000
    #b10010000
    #b10010000
    #b11110000

    #b00100000
    #b01100000
    #b00100000
    #b00100000
    #b01110000

    #b11110000
    #b00010000
    #b11110000
    #b10000000
    #b11110000

    #b11110000
    #b00010000
    #b11110000
    #b00010000
    #b11110000

    #b10010000
    #b10010000
    #b11110000
    #b00010000
    #b00010000

    #b11110000
    #b10000000
    #b11110000
    #b00010000
    #b11110000

    #b11110000
    #b10000000
    #b11110000
    #b10010000
    #b11110000

    #b11110000
    #b00010000
    #b00100000
    #b01000000
    #b01000000

    #b11110000
    #b10010000
    #b11110000
    #b10010000
    #b11110000

    #b11110000
    #b10010000
    #b11110000
    #b00010000
    #b11110000

    #b11110000
    #b10010000
    #b11110000
    #b10010000
    #b10010000

    #b11100000
    #b10010000
    #b11100000
    #b10010000
    #b11100000

    #b11110000
    #b10000000
    #b10000000
    #b10000000
    #b11110000

    #b11100000
    #b10010000
    #b10010000
    #b10010000
    #b11100000

    #b11110000
    #b10000000
    #b11110000
    #b10000000
    #b11110000

    #b11110000
    #b10000000
    #b11110000
    #b10000000
    #b10000000
))

(vector-copy! memory 0 sprites)