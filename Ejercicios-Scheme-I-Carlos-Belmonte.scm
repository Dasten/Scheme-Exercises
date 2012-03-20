;; Ejercicios Scheme I - Carlos Belmonte Ceniza

;; 1 - Algoritmos con listas


;;Ejercicio 1
(define (list-ref-right l k)
  (cond
   ((null? l) '())
   ((eq? k 0)
    (car l))
   (else
    (list-ref-right (cdr l) (- k 1)))))

(pp (list-ref-right '(a b c d e f g) 3))

;;Ejercicio 2
;;En este caso usamos la funcion list-ref del R5RS-1, combinada con la funcion
;;reverse,  de esta manera...

(pp (list-ref (reverse '(a b c d e f g h)) 2))


;;Ejercicio 3
