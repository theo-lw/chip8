#lang typed/racket

(require "instructions.rkt")
(require "disassembler.rkt")
(require "structs.rkt")
(require "chip8.rkt")
(require "gui.rkt")

(: address (-> Integer Integer Integer Integer))
(define (address x y z) (+ z (* 16 (+ y (* 16 x)))))

(: byte (-> Integer Integer Integer))
(define (byte x y) (+ y (* 16 x)))

(: dispatch-instruction (-> instruction Void))
(define (dispatch-instruction instr)
  (match instr
    [(instruction #x0 #x0 #xE #x0) (CLS)]
    [(instruction #x0 #x0 #xE #xE) (RET)]
    [(instruction #x0 n1 n2 n3) (SYS (address n1 n2 n3))]
    [(instruction #x1 n1 n2 n3) (JP (address n1 n2 n3))]
    [(instruction #x2 n1 n2 n3) (CALL (address n1 n2 n3))]
    [(instruction #x3 x k1 k2) (SE x (byte k1 k2))]
    [(instruction #x4 x k1 k2) (SNE x (byte k1 k2))]
    [(instruction #x5 x y #x0) (SE x (chip8/V-at y))]
    [(instruction #x6 x k1 k2) (LD (chip8-set-V! x) (byte k1 k2))]
    [(instruction #x7 x k1 k2) (ADD x (byte k1 k2))]
    [(instruction #x8 x y #x0) (LD (chip8-set-V! x) (chip8/V-at y))]
    [(instruction #x8 x y #x1) (OR x y)]
    [(instruction #x8 x y #x2) (AND x y)]
    [(instruction #x8 x y #x3) (XOR x y)]
    [(instruction #x8 x y #x4) (CADD x y)]
    [(instruction #x8 x y #x5) (SUB x y)]
    [(instruction #x8 x y #x6) (SHR x)]
    [(instruction #x8 x y #x7) (SUB y x)]
    [(instruction #x8 x y #xE) (SHL x)]
    [(instruction #x9 x y #x0) (SNE x (chip8/V-at y))]
    [(instruction #xA n1 n2 n3) (LD chip8/set-I! (address n1 n2 n3))]
    [(instruction #xB n1 n2 n3) (VJP (address n1 n2 n3))]
    [(instruction #xC x k1 k2) (RND x (byte k1 k2))]
    [(instruction #xD x y n) (DRW x y n)]
    [(instruction #xE x #x9 #xE) (SKP x)]
    [(instruction #xE x #xA #x1) (SKNP x)]
    [(instruction #xF x #x0 #x7) (LD (chip8-set-V! x) chip8/DT)]
    [(instruction #xF x #x0 #xA) (LD (chip8-set-V! x) (get-key (current-inexact-milliseconds)))]
    [(instruction #xF x #x1 #x5) (LD chip8/set-DT! (chip8/V-at x))]
    [(instruction #xF x #x1 #x8) (LD chip8/set-ST! (chip8/V-at x))]
    [(instruction #xF x #x1 #xE) (IADD x)]
    [(instruction #xF x #x2 #x9) (LD chip8/set-I! (* 5 (chip8/V-at x)))]
    [(instruction #xF x #x3 #x3) (for ([i 3])
                                   (LD (chip8-set-memory! (+ chip8/I (- 2 i)))
                                       (modulo (quotient (chip8/V-at x) (expt 10 i)) 10)))]
    [(instruction #xF x #x5 #x5) (for ([i (add1 x)])
                                   (LD (chip8-set-memory! (+ chip8/I i))
                                       (chip8/V-at i)))]
    [(instruction #xF x #x6 #x5) (for ([i (add1 x)])
                                   (LD (chip8-set-V! i) (chip8/memory-at (+ chip8/I i))))]
    [x (error "instruction doesn't exist")]
))

(: chip8-set-V! (-> Integer (-> Integer Void)))
(define (chip8-set-V! x)
  (lambda (y) (chip8/set-V! x y)))

(: chip8-set-memory! (-> Integer (-> Integer Void)))
(define (chip8-set-memory! x)
  (lambda (y) (chip8/set-memory! x y)))

(: get-key (-> Real Integer))
(define (get-key timestamp)
  (if (> (send chip8/canvas get-key-timestamp) timestamp)
      (send chip8/canvas get-key)
      (begin (display "start sleep")
             (sleep 0.1)
             (display "end sleep")
             (get-key timestamp))))

(: execute-file (-> Void))
(define (execute-file)
  (when (< chip8/PC 4096)
        (dispatch-instruction (chip8/fetch-instruction))
        (chip8/increment-PC!)
        (execute-file)))

(define (debug-instr)
  (let ([instr (chip8/fetch-instruction)])
        (print chip8/PC)
        (display ": ")
        (pretty-print (debug/disassemble-instruction instr))
        (dispatch-instruction instr)
        (chip8/increment-PC!)))

(: debug-file (-> Integer Void))
(define (debug-file end)
  (when (< chip8/PC end)
        (debug-instr)
        (debug-file end)))

(: execute (-> Path-String Void))
(define (execute filepath)
  (chip8/load-file filepath)
  (execute-file))

(: debug (-> Path-String Void))
(define (debug filepath)
  (chip8/load-file filepath)
  (debug-file 4096))

(define (reset)
  (chip8/reset!)
  (CLS))