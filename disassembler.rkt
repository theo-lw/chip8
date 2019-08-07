#lang typed/racket

(require "structs.rkt")
(provide (prefix-out debug/ (all-defined-out)))

; Registers
(struct V ([at : Integer]) #:transparent)
(struct DT () #:transparent)
(struct ST () #:transparent)
(struct I () #:transparent)

; Events
(struct K () #:transparent)

; Opcodes
(define-type opcode (U SYS CLS RET JP CALL SE SNE LD ADD OR AND XOR SUB SHR SUBN SHL RND DRW SKP SKNP BAD))
(struct SYS ([v1 : Integer]) #:transparent)
(struct CLS () #:transparent)
(struct RET () #:transparent)
(struct JP ([v1 : (U V Integer)] [v2 : Integer]) #:transparent)
(struct CALL ([v1 : Integer]) #:transparent)
(struct SE ([v1 : V] [v2 : (U V Integer)]) #:transparent)
(struct SNE ([v1 : V] [v2 : (U V Integer)]) #:transparent)
(struct LD ([v1 : (U V DT ST I 'F 'B (Listof V))] [v2 : (U V DT I Integer K (Listof V))]) #:transparent)
(struct ADD ([v1 : (U V I)] [v2 : (U V Integer)]) #:transparent)
(struct OR ([v1 : V] [v2 : V]) #:transparent)
(struct AND ([v1 : V] [v2 : V]) #:transparent)
(struct XOR ([v1 : V] [v2 : V]) #:transparent)
(struct SUB ([v1 : V] [v2 : V]) #:transparent)
(struct SHR ([v1 : V] [v2 : V]) #:transparent)
(struct SUBN ([v1 : V] [v2 : V]) #:transparent)
(struct SHL ([v1 : V] [v2 : V]) #:transparent)
(struct RND ([v1 : V] [v2 : Integer]) #:transparent)
(struct DRW ([v1 : V] [v2 : V] [v3 : Integer]) #:transparent)
(struct SKP ([v1 : V]) #:transparent)
(struct SKNP ([v1 : V]) #:transparent)
(struct BAD ([v1 : instruction]) #:transparent)

; Disassembler

(: address (-> Integer Integer Integer Integer))
(define (address x y z) (+ z (* 16 (+ y (* 16 x)))))

(: byte (-> Integer Integer Integer))
(define (byte x y) (+ y (* 16 x)))

(: disassemble-instruction (-> instruction opcode))
(define (disassemble-instruction instr)
  (match instr
    [(instruction #x0 #x0 #xE #x0) (CLS)]
    [(instruction #x0 #x0 #xE #xE) (RET)]
    [(instruction #x0 n1 n2 n3) (SYS (address n1 n2 n3))]
    [(instruction #x1 n1 n2 n3) (JP 0 (address n1 n2 n3))]
    [(instruction #x2 n1 n2 n3) (CALL (address n1 n2 n3))]
    [(instruction #x3 x k1 k2) (SE (V x) (byte k1 k2))]
    [(instruction #x4 x k1 k2) (SNE (V x) (byte k1 k2))]
    [(instruction #x5 x y #x0) (SE (V x) (V y))]
    [(instruction #x6 x k1 k2) (LD (V x) (byte k1 k2))]
    [(instruction #x7 x k1 k2) (ADD (V x) (byte k1 k2))]
    [(instruction #x8 x y #x0) (LD (V x) (V y))]
    [(instruction #x8 x y #x1) (OR (V x) (V y))]
    [(instruction #x8 x y #x2) (AND (V x) (V y))]
    [(instruction #x8 x y #x3) (XOR (V x) (V y))]
    [(instruction #x8 x y #x4) (ADD (V x) (V y))]
    [(instruction #x8 x y #x5) (SUB (V x) (V y))]
    [(instruction #x8 x y #x6) (SHR (V x) (V y))]
    [(instruction #x8 x y #x7) (SUBN (V x) (V y))]
    [(instruction #x8 x y #xE) (SHL (V x) (V y))]
    [(instruction #x9 x y #x0) (SNE (V x) (V y))]
    [(instruction #xA n1 n2 n3) (LD (I) (address n1 n2 n3))]
    [(instruction #xB n1 n2 n3) (JP (V 0) (address n1 n2 n3))]
    [(instruction #xC x k1 k2) (RND (V x) (byte k1 k2))]
    [(instruction #xD x y n) (DRW (V x) (V y) n)]
    [(instruction #xE x #x9 #xE) (SKP (V x))]
    [(instruction #xE x #xA #x1) (SKNP (V x))]
    [(instruction #xF x #x0 #x7) (LD (V x) (DT))]
    [(instruction #xF x #x0 #xA) (LD (V x) (K))]
    [(instruction #xF x #x1 #x5) (LD (DT) (V x))]
    [(instruction #xF x #x1 #x8) (LD (ST) (V x))]
    [(instruction #xF x #x1 #xE) (ADD (I) (V x))]
    [(instruction #xF x #x2 #x9) (LD 'F (V x))]
    [(instruction #xF x #x3 #x3) (LD 'B (V x))]
    [(instruction #xF x #x5 #x5) (LD (I) (build-list (add1 x) (lambda (x) (V x))))]
    [(instruction #xF x #x6 #x5) (LD (build-list (add1 x) (lambda (x) (V x))) (I))]
    [x (BAD x)]))

; disassembles from the current input port

(: fetch-instruction (-> instruction))
(define (fetch-instruction)

  (: first-nibble (-> (U Byte EOF) Integer))
  (define (first-nibble byte) (if (eof-object? byte) (error "Malformed instruction!") (quotient byte 16)))

  (: second-nibble (-> (U Byte EOF) Integer))
  (define (second-nibble byte) (if (eof-object? byte) (error "Malformed instruction!") (modulo byte 16)))

  (define first-byte (read-byte))  
  (define second-byte (read-byte))
  (instruction (first-nibble first-byte) (second-nibble first-byte) (first-nibble second-byte) (second-nibble second-byte)))

(: disassemble-file (-> Void))
(define (disassemble-file)
  (when (not (eof-object? (peek-byte)))
      (begin
        (pretty-print (disassemble-instruction (fetch-instruction)))
        (disassemble-file))))

(: disassemble (-> Path-String Void))
(define (disassemble filepath)
  (with-input-from-file filepath disassemble-file))

; IDEA
; Use classes instead of these random structs
; Instruction classes should contain functions that contain global states
; instruction class function :: global state -> global state (will most definitely contain some side effects)
; execute :: instruction -> (global state -> global state)
; disassemble-instruction :: instruction -> instruction class
; disassemble :: '(instruction) -> '(serialized instruction classes)

