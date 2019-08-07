#lang typed/racket

(provide (struct-out instruction))

(struct instruction ([first : Integer] [second : Integer] [third : Integer] [fourth : Integer]) #:transparent)