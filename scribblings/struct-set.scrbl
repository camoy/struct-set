#lang scribble/manual

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; require

@require[@for-label[racket/base
                    struct-set]
         racket/sandbox
         scribble/example]

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; example evaluator

@(define evaluator
   (make-base-eval
     '(require struct-set)))

@;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
@;; document

@title{Immutable Struct Updates}
@author{Cameron Moy}

@defmodule[struct-set]

@margin-note{
  See @hyperlink[SO-POST]{this link} for an explanation of the behavior
  of @racket[struct-copy].
}

This module defines helpers for immutably updating structs.
Notably, updating a struct using this package doesn't change its type
like @racket[struct-copy] does. Under the hood, it derives a generic method
for copying structs that is then dispatched to for updating.

@defform[(struct/set id maybe-super (field ...) struct-option ...)]{
  The same as @racket[struct], except the newly defined struct
  supports updating via @racket[struct-set].

  @examples[#:eval evaluator
    (struct/set fruit (price))
    (struct/set apple fruit (oxidized?))
    (struct/set banana fruit (too-ripe?))]
}

@defform[(struct-set id struct-expr [fld-id expr] ...)]{
  The same as @racket[struct-copy], except it will
  return the same struct instance as @racket[struct-expr],
  which may not necessarily be the same as @racket[id].

  @examples[#:eval evaluator
    (define old-apple (apple 1.50 #t))
    (define new-apple (struct-set fruit old-apple [price 1.00]))
    (fruit-price new-apple)
    (apple-oxidized? new-apple)]
}


@defform[(struct-update id struct-expr [fld-id update-expr] ...)]{
  Like @racket[struct-set], except it will apply the result
  of @racket[update-expr] to the old value of the field
  to yield the new value.

  @examples[#:eval evaluator
    (define cheap-apple (struct-update fruit old-apple [price sub1]))
    (fruit-price cheap-apple)
    (apple-oxidized? cheap-apple)]
}

@(define SO-POST
  (string-append "https://stackoverflow.com/questions/52142731/"
                 "how-do-you-get-struct-copy-to-create-a-struct-of-the-same-type-as-the-original"))
