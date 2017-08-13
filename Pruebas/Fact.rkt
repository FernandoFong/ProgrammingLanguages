#lang racket
#| This is a comment.
 | of more than one line.|#

(define (factorial n)
  (cond [(zero? n) 1]
        [else (* n (factorial (- n 1)))]))

