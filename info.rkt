#lang info

;; General

(define collection "struct-set")
(define pkg-desc "Helpers for immutably updating structs.")
(define version "0.0")
(define pkg-authors '(camoy))
(define scribblings '(("scribblings/struct-set.scrbl" ())))

;; Dependencies

(define deps
  '("syntax-classes-lib"
    "base"))

(define build-deps
  '("sandbox-lib"
    "chk-lib"
    "scribble-lib"
    "racket-doc"
    "rackunit-lib"))
