;; Ejercicios Scheme I - Carlos Belmonte Ceniza

(load "/data/projects/scheme-srfi/src/srfi-1.scm")

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
;;Usamos la funcion take-right y drop-right del srfi-1
(define (rotate-right l k)
  (let recur ((list l)
              (counter k))
    (cond
     ((eq? counter 0)
      list)
     (else
      (recur 
       (drop-right (cons (car (take-right list 1)) list) 1)
       (- counter 1))))))

(pp (rotate-right '(a b c d e f g h) 3))

;;Ejercicio 7
(define (insert-at new k l) 
  (let recur ((list l)
              (counter k))
    (cond
     ((eq? counter 0)
      (cons new (cdr list)))
     (else
      (cons (car list) (recur (cdr list) (- counter 1)))))))

(pp (insert-at 'N 2 '(a b c d e f)))

;;Ejercicio 8
(define (insert-left new e l)
  (let recur ((list l))
    (cond 
     ((null? list) '())
     ((eq? (car list) e)
      (cons new (cons e (recur (cdr list)))))
     (else
      (cons (car list) (recur (cdr list)))))))

(pp (insert-left 'N 'c '(a b c d e c f)))

;;Exercicio 9
;;TO DO

;;Ejercicio 10
(define (insert-right new e l)
  (let recur ((list l))
    (cond
     ((null? list) '())
     ((eq? (car list) e)
      (cons e (cons new (recur (cdr list)))))
     (else
      (cons (car list) (recur (cdr list)))))))

(pp (insert-right 'N 'c '(a b c d e c f)))

;;Ejercicio 11
;;TO DO

;;Ejercicio 12
(define (remove-at k l)
  (let recur ((list l)
              (counter k))
    (cond
     ((eq? counter 0)
      (cdr list))
     (else
      (cons (car list) (recur (cdr list) (- counter 1)))))))

(pp (remove-at 3 '(a b c d e f)))