;; Ejercicios Scheme I - Carlos Belmonte Ceniza

;;Funciones complementarias usadas para los ejercicios
(define (length l)
  (let recur ((list l))
    (cond 
     ((null? list) 0)
     (else
      (+ 1 (recur (cdr list)))))))

(define (element-append l argl)
  (let recur ((list l))
    (cond
     ((null? list) (cons argl '()))
     (else
      (cons (car list) (recur (cdr list)))))))

;;-1 - Algoritmos con listas

;;Ejercicio 1
;;Usamos la funcion length definida arriba
(define (list-ref-right l k)
  (let recur ((list l)
              (counter (- (length l) k)))
    (cond
     ((null? list) '())
     ((eq? counter 0)
      (car list))
     (else
      (recur (cdr list) (- counter 1))))))
#;;
(define (list-ref-right l k)
  (cons '() (car (take-right l k))))

(pp (list-ref-right '(a b c d Z Y f g) 3))

;;Ejercicio 2
;;Usamos la funcion list-ref del R5RS-1 + la funcion reverse

(pp (list-ref (reverse '(a b c d e f g h)) 2))

;;Ejercicio 3
(define (list-ref-circular l k)
  (let recur ((list l) (counter 
                        (- (length l)
                           (cond
                            ((> k (length l))
                             (modulo k (length l)))
                            (else
                             k)))))
    (cond
     ((null? list) '())
     ((eq? counter 0)
      (car list))
     (else
      (recur (cdr list) (- counter 1))))))

(pp (list-ref-circular '(a b c d Z Y f) 16))

;;Ejercicio 4
(define (palindrome? l)
  (equal? (reverse l) l))

(pp (palindrome? '(a b b a)))

;;Ejercicio 5
;;Usamos la funcion element-append definida arriba
(define (rotate-left l k)
  (let recur ((list l) 
              (counter k))
    (cond
     ((eq? counter 0)
      list)
     (else
      (recur (element-append (cdr list) (car list)) (- counter 1))))))

(pp (rotate-left '(a b c d e f g h) 3))

;;Ejercicio 6
(define (rotate-right l k)
  (let recur ((list l)
              (counter k))
    (cond
     ((eq? counter 0)
      list)
     (else
      (recur (TO DO) (- counter 1))))))

(pp (rotate-right '(a b c d e f g h) 3))