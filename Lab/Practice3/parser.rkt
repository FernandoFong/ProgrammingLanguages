#lang plai

(require "grammars.rkt")

;;Función que nos regresa el procedure que necesita WAE.
;;Dado un símbolo o una cadena, nos regresa el procedure que representa.
;;pass-fun: ? -> procedure.
(define (pass-fun s)
  (match s
    ['+ +]
    ['- -]
    ['* *]
    ['/ /]
    ["min" min]
    ["max" max]
    ["pow" mexpt]
    [else error 'pass-fun "Not in WAE"]))

;;Función que nos regresa la exponenciación de WAE.
;;Dados dos parámetros de tipo number, nos regresa el resultado del primer parámetro
;;a la potencia del segundo parámetro.
;;mexpt: number number -> number.
(define (mexpt num ex)
  (expt num ex))

;; Analizador sintáctico para WAE.
;; Dada una s-expresión, regresa el árbol de sintaxis abstracta correspondiente, es decir, construye 
;; expresiones del tipo de dato abstracto definido en el archivo grammars.rkt
;; parse: s-expresion -> WAE.
(define (parse sexp)
  (match sexp
    [(? number?) (num sexp)]
    [(? symbol?) (id sexp)]
    [(list x) (if (number? x) (num x) (id x))]
    [(list "with" lst body)
     (match lst
       ['() (with '() (parse body))]
       [else (with (make-bindings lst) (parse body))])]
    [(list "with*" lst body)
     (match lst
       ['() (with* '() (parse body))]
       [else (with* (make-bindings lst) (parse body))])]
    [(cons oper lst)
     (match lst
       ['() error 'parse "No parameters"]
       [else (op (pass-fun oper) (map parse lst))])]
    [else "Not in WAE"]))


;;Función que nos regresa una lista de bindings del tipo WAE.
;;Dada una lista de listas de tamaño 2, de la forma (symbol, number) nos regresa una
;;lista de n bindings donde n es el tamaño de la lista de listas.
;;listof(listof(symbol number)) -> listof bindings.
(define (make-bindings lst)
  (match lst
    [(list (list var val)) (list (binding  var (parse val)))]
    [(cons (list var val) xs) (cons (binding var (parse val)) (make-bindings xs))]))