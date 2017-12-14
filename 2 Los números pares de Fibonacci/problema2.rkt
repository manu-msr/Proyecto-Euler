#lang racket

;; Tabla para almacenar los resultados previos y evitar calculos repetidos. Esta tabla permite usar
;; la técnica de memoización para optimizar la solución.
(define tabla (make-hash (list (cons 0 1) (cons 1 2))))

;; Función que obtiene en n-ésimo elemento de la sucesión de Fibonacci.
;; fibonacci: number -> number
(define (fibonacci n)
   (fibonacci-memo n))

;; Versión memoizada de la función de Fibonacci. Primero revisa, buscando en la tabla, si el 
;; resultado fue calculado anteriormente.
;; fibonacci: number -> number
(define (fibonacci-memo n)
   (let ([busqueda (hash-ref tabla n 'ninguno)])
      (cond
         [(equal? busqueda 'ninguno)
            (define resultado (+ (fibonacci-memo (- n 1)) (fibonacci-memo (- n 2))))
            (hash-set! tabla n resultado)
            resultado]
         [else busqueda])))

;; Suma los números pares de la sucesión de Fibonacci menores a cuatro millones.
;; solucion: number
(define (solucion)
   (letrec (
      [genera-siguiente (lambda (fib)
         (let ([res (fibonacci-memo fib)])
            (cond
               [(>= res 4000000) 0]
               [(even? res) (+ res (genera-siguiente (add1 fib)))]
               [else (genera-siguiente (add1 fib))])))])
      (genera-siguiente 0)))