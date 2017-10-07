#lang plai

(require "grammars.rkt")
(require "parser.rkt")
(require "interp.rkt")

(print-only-errors)

#| Módulo para pruebas unitarias de la práctica 3 |#

;; Pruebas para  parse
(test (parse '(1)) (num 1))
(test (parse 'c) (id 'c))
(test (parse '(+ 1 2 3)) (op + (list (num 1) (num 2) (num 3))))
(test (parse '("with" ((a 2) (b 3)) (+ a b)))
      (with (list (binding 'a (num 2)) (binding 'b (num 3))) (op + (list (id 'a) (id 'b)))))
(test (parse '("with*" ((x (+ 2 2)) (y (* 3 3))) ("pow" x y)))
      (with*
       (list (binding 'x (op + (list (num 2) (num 2)))) (binding 'y (op * (list (num 3) (num 3)))))
       (op mexpt (list (id 'x) (id 'y)))))

;; Pruebas para  interp

(test (interp (parse 'foo)) "Free identifier")
(test (interp (parse '1729)) 1729)
(test (interp (parse '(+ 1 2 3))) 6)
(test (interp (parse '("with" ((x 2) (y 3)) (+ x y)))) 5)
(test (interp (parse '("with*" ((a 1) (b (+ 1 a)) (c (+ b 1)) (d (+ 1 c)) (e (+ 1 d))) (+ a b c d e)))) 15)

;; Pruebas para  subst (opcional)

(test (subst (op + (list (id 'x) (id 'x))) 'x (num 2)) (op + (list (num 2) (num 2))))
(test (subst (op * (list (id 'x) (id 'y))) 'x (num 3)) (op * (list (num 3) (id 'y))))
