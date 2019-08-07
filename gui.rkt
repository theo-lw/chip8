#lang typed/racket

(require typed/racket/gui)
(require typed/racket/class)

(provide (prefix-out chip8/ canvas))

(define frame (new frame%
                   [label "Example"]
                   [width 300]
                   [height 300]))

(define display%
  (class canvas%
    (inherit get-width get-height)
    
    (: chip8-keyboard (Vectorof Boolean))
    (define chip8-keyboard (make-vector 16 #f))

    (: actual-keyboard (Mutable-HashTable Char Integer))
    (define actual-keyboard (make-hash '((#\1 . #x1) (#\2 . #x2) (#\3 . #x3) (#\4 . #xC)
                                           (#\q . #x4) (#\w . #x5) (#\e . #x6) (#\r . #xD)
                                           (#\a . #x7) (#\s . #x8) (#\d . #x9) (#\f . #xE)
                                           (#\z . #xA) (#\x . #x0) (#\c . #xB) (#\v . #xF))))
    (: key-press (Pairof Char Real))
    (define key-press (cons #\0 (current-inexact-milliseconds)))
    (define width 64)
    (define height 32)

    (: display (Vectorof (Vectorof Integer)))
    (define display (let ([result (make-vector height (make-vector width 1))])
                      (for ([i height]) (vector-set! result i (make-vector width 1)))
                      result))
    
    (: display-get (-> Integer Integer Integer))
    (define/public (display-get x y)
      (vector-ref (vector-ref display (modulo y height)) (modulo x width)))

    (: display-set! (-> Integer Integer Integer Void))
    (define/public (display-set! x y val)
      (vector-set! (vector-ref display (modulo y height))
                   (modulo x width)
                   (bitwise-xor (display-get x y) val)))

    (define/public (display-reset!)
      (for ([i height]) (vector-set! display i (make-vector width 1))))

    (: on-char (-> (Instance Key-Event%) Void))
    (define/override (on-char key)
      (let ([press (send key get-key-code)] [release  (send key get-key-release-code)])
        (cond
          [(and (char? press) (hash-has-key? actual-keyboard press))
            (set! key-press (cons press (current-inexact-milliseconds)))
            (vector-set! chip8-keyboard (hash-ref actual-keyboard press) #t)]
          [(and (char? release) (hash-has-key? actual-keyboard release))
            (vector-set! chip8-keyboard (hash-ref actual-keyboard release) #f)])))

    (: pressed? (-> Integer Boolean))
    (define/public (pressed? key)
      (and (> 16 key) (>= key 0) (vector-ref chip8-keyboard key)))

    (: get-key-timestamp (-> Real))
    (define/public (get-key-timestamp) (cdr key-press))

    (: get-key (-> Integer))
    (define/public (get-key) (hash-ref actual-keyboard (car key-press)))
    
    (super-new [paint-callback (lambda (canvas dc)
                                 (let ([size (min (quotient (get-width) 64) (quotient (get-height) 32))])
                                   (for ([y height])
                                     (for ([x width])
                                       (send dc set-brush
                                             (if (= 1 (display-get x y))
                                                 "black"
                                                 "white")
                                             'solid)
                                       (send dc set-pen
                                             (if (= 1 (display-get x y))
                                                 "black"
                                                 "white")
                                             (send (send dc get-pen) get-width)
                                             'solid)
                                       (send dc draw-rectangle (* x size) (* y size) size size)))))])))

(define canvas (new display% [parent frame]))
                                   
(send frame show #t)