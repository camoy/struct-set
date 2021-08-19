#lang info

;; General

(define collection "struct-set")
(define pkg-desc "Description Here")
(define version "0.0")
(define pkg-authors '(camoy))
(define scribblings '(("scribblings/struct-set.scrbl" ())))

;; Dependencies

(define deps
  '("syntax-classes-lib"
    "base"))

(define build-deps
  '("chk-lib"
    "scribble-lib"
    "racket-doc"
    "rackunit-lib"))
