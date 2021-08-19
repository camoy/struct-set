#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide struct/set
         struct-set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; require

(require (for-syntax racket/base
                     racket/function
                     racket/list
                     racket/sequence
                     racket/set
                     racket/struct-info
                     syntax/parse
                     syntax/parse/class/struct-id
                     syntax/strip-context)
         racket/contract
         racket/generic
         racket/unsafe/ops)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; runtime stuff

(define-generics struct-can-set
  (struct-set-procedure struct-can-set . rst))

(define missing (gensym 'missing))
(define missing-cycle (in-cycle (list missing)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helpers

(begin-for-syntax
  (define (struct-fields si)
    (define flds (reverse (struct-field-info-list si)))
    (define par-id (last (extract-struct-info si)))
    (if (identifier? par-id)
        (append (struct-fields (syntax-local-value par-id)) flds)
        flds)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros

(define-syntax (struct/set stx)
  (syntax-parse stx
    [(_ ?name:id (~optional ?parent:id) (?fld ...) ?opt ...)
     #:with ?stx stx
     #'(struct/derived
        ?stx ?name (~? ?parent) (?fld ...)
        ?opt ...
        #:methods gen:struct-can-set
        [(define-struct-set-method ?name)])]))

(define-syntax (struct-set stx)
  (syntax-parse stx
    [(_ ?s:struct-id ?inst [?fld:id ?fld-val:expr] ...)
     #:declare ?inst (expr/c #'(and/c ?s.predicate-id struct-can-set?))
     #:do [(define fields (struct-fields (syntax-local-value #'?s)))]
     #:fail-unless
     (subset? (syntax->datum #'(?fld ...)) fields)
     "field name not associated with the given structure type"
     #:do [(define update-hash
             (for/hash ([fld (in-syntax #'(?fld ...))]
                        [val (in-syntax #'(?fld-val ...))])
               (values (syntax-e fld) val)))]
     #:with (?new-fld-val ...)
     (for/list ([fld (in-list fields)])
       (hash-ref update-hash fld (const #'missing)))
     #'(struct-set-procedure ?inst.c ?new-fld-val ...)]))

(define-syntax (define-struct-set-method stx)
  (syntax-parse stx
    [(_ ?s:struct-id)
     #:fail-unless (attribute ?s.all-fields-visible?)
     "not all fields are visible"
     #:with ?num-fields (datum->syntax stx (attribute ?s.num-fields))
     #:with ?method-name (replace-context stx #'struct-set-procedure)
     #'(define (?method-name self . rst)
         (define new-flds
           (for/list ([new-fld (in-sequences rst missing-cycle)]
                      [k (in-range ?num-fields)])
             (if (eq? new-fld missing)
                 (unsafe-struct-ref self k)
                 new-fld)))
         (apply ?s.constructor-id new-flds))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tests

(module+ test
  (require chk)

  (chk
   #:do (struct/set fruit (price))
   #:do (struct/set banana fruit (ripe?))
   #:do (struct/set orange fruit (juicy?))
   #:do (define b (banana 1.90 #t))
   #:do (define b* (struct-set banana b [price 2.00]))

   ;; success
   (fruit-price b*) 2.00
   #:t (banana-ripe? b*)

   ;; failure
   #:do (struct bad (thing))
   #:x (struct-set bad (bad 42) [thing 43])
   "expected: struct-can-set?"

   #:x (struct-set bad b [thing 43])
   "expected: bad?"
   ))
