#lang racket/base

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; provide

(provide struct/set
         struct-set
         struct-update)

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
         racket/function
         racket/generic
         racket/unsafe/ops)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; runtime stuff

(define-generics struct-can-update
  (struct-update-procedure struct-can-update . rst))

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
        flds))

  (define (struct-update-parser stx set-only?)
    (syntax-parse stx
      [(_ ?s:struct-id ?inst [?fld:id ?fld-val] ...)
       #:declare ?fld-val (expr/c (if set-only? #'any/c #'(-> any/c any/c)))
       #:declare ?inst (expr/c #'(and/c ?s.predicate-id struct-can-update?))
       #:do [(define fields (struct-fields (syntax-local-value #'?s)))]
       #:fail-unless
       (subset? (syntax->datum #'(?fld ...)) fields)
       "field name not associated with the given structure type"
       #:do [(define update-hash
               (for/hash ([fld (in-syntax #'(?fld ...))]
                          [val (in-syntax #'(?fld-val.c ...))])
                 (values (syntax-e fld) val)))]
       #:with (?new-fld ...)
       (for/list ([fld (in-list fields)])
         (cond
           [(hash-has-key? update-hash fld)
            (define fld-val (hash-ref update-hash fld))
            (if set-only? #`(const #,fld-val) fld-val)]
           [else #'missing]))
       #'(struct-update-procedure ?inst.c ?new-fld ...)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; macros

(define-syntax (struct/set stx)
  (syntax-parse stx
    [(_ ?name:id (~optional ?parent:id) (?fld ...) ?opt ...)
     #:with ?stx stx
     #'(struct/derived
        ?stx ?name (~? ?parent) (?fld ...)
        ?opt ...
        #:methods gen:struct-can-update
        [(define-struct-update-method ?name)])]))

(define-syntax (define-struct-update-method stx)
  (syntax-parse stx
    [(_ ?s:struct-id)
     #:fail-unless (attribute ?s.all-fields-visible?)
     "not all fields are visible"
     #:with ?num-fields (datum->syntax stx (attribute ?s.num-fields))
     #:with ?method-name (replace-context stx #'struct-update-procedure)
     #'(define (?method-name self . rst)
         (define new-flds
           (for/list ([proc (in-sequences rst missing-cycle)]
                      [k (in-range ?num-fields)])
             (define prior (unsafe-struct-ref self k))
             (if (eq? proc missing) prior (proc prior))))
         (apply ?s.constructor-id new-flds))]))

(define-syntax (struct-set stx)
  (struct-update-parser stx #t))

(define-syntax (struct-update stx)
  (struct-update-parser stx #f))

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
   #:do (define b** (struct-update banana b [price add1]))

   ;; success
   (fruit-price b*) 2.00
   #:t (banana-ripe? b*)
   (fruit-price b**) 2.90

   ;; failure
   #:do (struct bad (thing))
   #:x (struct-set bad (bad 42) [thing 43])
   "expected: struct-can-update?"

   #:x (struct-set bad b [thing 43])
   "expected: bad?"

   #:x (struct-update banana b [price 43])
   "expected: a procedure"
   ))
