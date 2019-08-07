#lang typed/racket

(require "chip8.rkt")
(require "gui.rkt")

(provide (except-out (all-defined-out) draw-byte))

(define (SYS addr)
  (chip8/set-PC! addr))

(define (CLS)
  (send chip8/canvas display-reset!)
  (send chip8/canvas refresh-now))

(define (RET)
  (chip8/set-PC! (chip8/top-stack))
  (chip8/increment-PC!)
  (chip8/pop-stack!))

(: JP (-> Integer Void))
(define (JP addr)
  (chip8/set-PC! addr))

(: VJP (-> Integer Void))
(define (VJP addr)
  (chip8/set-PC! (+ addr (chip8/V-at 0))))

(: CALL (-> Integer Void))
(define (CALL addr)
  (chip8/push-stack! chip8/PC)
  (chip8/set-PC! addr))

(: LD (-> (-> Integer Void) Integer Void))
(define (LD f v2) (f v2))

(: ADD (-> Integer Integer Void))
(define (ADD v1 v2)
  (chip8/set-V! v1 (modulo (+ (chip8/V-at v1) v2) 256)))

(: IADD (-> Integer Void))
(define (IADD v1)
  (chip8/set-I! (modulo (+ chip8/I (chip8/V-at v1)) 4096)))

(: CADD (-> Integer Integer Void))
(define (CADD v1 v2)
  (define sum (+ (chip8/V-at v1) (chip8/V-at v2)))
  (chip8/set-V! v1 (modulo sum 256))
  (chip8/set-V! #xF (if (> sum 255) 1 0)))

(: SUB (-> Integer Integer Void))
(define (SUB v1 v2)
  (chip8/set-V! #xF (if (> (chip8/V-at v1) (chip8/V-at v2)) 1 0))
  (chip8/set-V! v1 (modulo (- (chip8/V-at v1) (chip8/V-at v2)) 256)))

(: SE (-> Integer Integer Void))
(define (SE v1 v2)
  (when (= (chip8/V-at v1) v2) (chip8/increment-PC!)))

(: SNE (-> Integer Integer Void))
(define (SNE v1 v2)
  (when (not (= (chip8/V-at v1) v2)) (chip8/increment-PC!)))

(: OR (-> Integer Integer Void))
(define (OR v1 v2)
  (chip8/set-V! v1 (bitwise-ior (chip8/V-at v1) (chip8/V-at v2))))

(: AND (-> Integer Integer Void))
(define (AND v1 v2)
  (chip8/set-V! v1 (bitwise-and (chip8/V-at v1) (chip8/V-at v2))))

(: XOR (-> Integer Integer Void))
(define (XOR v1 v2)
  (chip8/set-V! v1 (bitwise-xor (chip8/V-at v1) (chip8/V-at v2))))

(: SHR (-> Integer Void))
(define (SHR v1)
  (chip8/set-V! #xF (remainder (chip8/V-at v1) 2))
  (chip8/set-V! v1 (quotient (chip8/V-at v1) 2)))

(: SHL (-> Integer Void))
(define (SHL v1)
  (chip8/set-V! #xF (quotient (chip8/V-at v1) 128))
  (chip8/set-V! v1 (modulo (* (chip8/V-at v1) 2) 256)))

(: RND (-> Integer Integer Void))
(define (RND v1 v2)
  (chip8/set-V! v1 (bitwise-and (random 256) v2)))

(: SKP (-> Integer Void))
(define (SKP v1)
  (when (send chip8/canvas pressed? (chip8/V-at v1))
    (chip8/increment-PC!)))

(: SKNP (-> Integer Void))
(define (SKNP v1)
  (when (not (send chip8/canvas pressed? (chip8/V-at v1)))
    (chip8/increment-PC!)))

(: DRW (-> Integer Integer Integer Void))
(define (DRW x y n)
  (for ([i n])
    (draw-byte (chip8/V-at x) (+ (chip8/V-at y) i) (chip8/memory-at (+ chip8/I i))))
  (send chip8/canvas refresh-now))

(: draw-byte (-> Integer Integer Integer Void))
(define (draw-byte x y b)
   (for ([i 8])
     (let ([ith (quotient b (expt 2 i))])
       (when (and (odd? ith) (odd? (send chip8/canvas display-get x y))) (chip8/set-V! #xF 1))
       (send chip8/canvas display-set! (+ x (- 7 i)) y (modulo ith 2)))))
  


  
