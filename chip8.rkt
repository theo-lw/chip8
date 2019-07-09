#lang racket

(require racket/gui/base)

(struct instruction (first second third fourth) #:transparent)

(provide chip8% instruction)

(define chip8% (class object%
    (super-new)
    (init filepath)
    (define (load-memory fpath)
        (define memory (make-vector 4096))
        (vector-copy! memory 512 (list->vector (port->list read-byte (open-input-file fpath))))
        (vector-copy! memory 0 sprites)
        memory)
    (define memory (load-memory filepath))
    (define V (make-vector 16 0))
    (define I 0)
    (define DT 0)
    (define ST 0)
    (define PC 512)
    (define SP 0)
    (define stack (make-vector 16 0))

    (define delay-timer (new timer% [notify-callback 
                                        (lambda (x) (if (zero? DT)
                                                        (send delay-timer stop)
                                                        (set! DT (sub1 DT))))] 
                                    [interval 16]))
    
    (define sound-timer (new timer% [notify-callback 
                                        (lambda (x) (if (zero? ST)
                                                        (send sound-timer stop)
                                                        (set! ST (sub1 ST))))]))
    
    (define/public (set-DT! value) (set! DT value))
    (define/public (set-ST! value) (set! ST value))
    (define/public (get-DT) DT)
    (define/public (get-ST) ST)
    (define/public (set-V! index value) (vector-set! V index value))
    (define/public (get-V index) (vector-ref V index))
    (define/public (set-I! value) (set! I value))
    (define/public (get-I) I)
    (define/public (push-stack! value) (vector-set! stack SP value) (set! SP (add1 SP)))
    (define/public (pop-stack!) (set! SP (sub1 SP)))
    (define/public (top-stack) (vector-ref stack (sub1 SP)))
    (define/public (fetch-instruction) 
        (define (first-nibble byte) (quotient byte 16))
        (define (second-nibble byte) (modulo byte 16))
        (define first-byte (vector-ref memory PC))
        (define second-byte (vector-ref memory (add1 PC)))
        (instruction (first-nibble first-byte) (second-nibble first-byte) (first-nibble second-byte) (second-nibble second-byte)))
    (define/public (increment-PC!) (set! PC (+ PC 2)))
    (define/public (set-PC! value) (set! PC value))
    (define/public (set-memory! index value) (vector-set! memory index value))
    (define/public (get-memory index) (vector-ref memory index))
    (define/public (core-dump) (display "Program Counter: ")
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
    (define/public (core-dump-file fpath) (with-output-to-file fpath (lambda () (core-dump))))))

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