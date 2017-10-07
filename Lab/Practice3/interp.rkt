#lang plai

(require "grammars.rkt")
(require "parser.rkt")

;; Función encargada de interpretar el árbol de sintaxis abstracta generado por el parser.
;; interp: WAE -> number
(define (interp expr)
  (match expr
    [(num n) n]
    [(id i) error 'interp "Free id"]
    [(op p body) (let [(int-body (map interp body))]
                   (foldr p (car int-body) (cdr int-body)))]
    [(with binds body) (interp (subst-lst binds body))]
    [(with* binds body) (interp (subst-lst (check-binds (car binds) (cdr binds)) body))]
    [else error "Not in WAE"]))

;;Función que recibe una lista de bindings para el with* que nos va a regresar
;;una lista con puros bindings interpretados.
;; check-binds: binding (listof binding) -> (listof binding)
(define (check-binds bind lst)
  (if (empty? lst)
      bind
      (match bind
        [(binding var val) (let* [(snd (car lst))
                                 (new-snd (binding (binding-name snd) (subst (binding-value snd) var val)))]
                             (cons bind (check-binds new-snd (cdr lst))))])))

;; Función que recibe una lista de bindings y una expresión de tipo WAE, a cada binding de la
;; lista, lo irá buscando en la sustitución.
;; subst-lst: (listof binding) WAE -> WAE.
(define (subst-lst binds body)
  (match binds
    [(binding var val) (subst body var val)]
    ['() body]
    [(cons (binding var val) xs)(let [(res (subst body var val))]
                                  (subst-lst xs res))]))

;; Función que implementa el algoritmo de sustitución.
;; subst: WAE symbol WAE -> WAE
(define (subst body var val)
  (match body
    [(num n) (num n)]
    [(id i) (if (equal? var i) val body)]
    [(op p lst) (op p (my-map subst lst var val))]
    [(with binds bdy) (subst (subst-lst binds bdy) var val)]
    [(with* binds bdy) (subst (subst-lst (check-binds (car binds) (cdr binds)) bdy) var val)]))

;; Función que nos ayuda para cuando tenemos un op, es un map mejorado puesto que map
;; solo nos acepta la lista y funciones de un solo parámetro, para la práctica se necesitan
;; más parámetros. Simplemente en cada término de lst irá aplicando la substitución y nos regresa una
;; lista de WAE.
;; my-map: procedure (listof WAE) string/symbol WAE -> (listof WAE)
(define (my-map subst lst var val)
  (match lst
    ['() '()]
    [(cons x xs) (cons (subst x var val) (my-map subst xs var val))]))

                           