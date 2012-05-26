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

(define (maximum l)
  (let recur ((list (cdr l))
              (n (car l)))
    (cond
     ((null? list) n)
     ((< n (car list)) (recur (cdr list) (car list)))
     ((> n (car list)) (recur (cdr list) n)))))

(define (minimum l)
  (let recur ((list (cdr l))
             (n (car l)))
    (cond
     ((null? list) n)
     ((> n (car list)) (recur (cdr list) (car list)))
     ((< n (car list)) (recur (cdr list) n)))))

;; 1 - Algoritmos con listas
(pp "Algoritmos con Listas")

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

;;Ejercicio 13
;;TO-DO


;;Ejercicio 14
;;TO-DO

#;;
(define (pack l)
  (let recur ((list l))
    (cond
     ((null? list) '())
     ((equal? (car list) (cadr list))
      (cons (cons (car list) (recur (cddr list))) '()))
     (else
      (cons (car list) (recur (cdr list)))))))
  
;;Ejercicio 15
;;Ejercicio 16
;;Ejercicio 17
;;Ejercicio 18
;;TO DO

;; 2 - Algoritmos matematicos
(pp "Algoritmos matematicos")

;;Ejercicio 1
(define (mean l)
  (/ (fold + 0 l) (length l)))

(pp (mean '(1 2 3 4 5)))

;;Ejercicio 2
;;Ejercicio 3

;;Ejercicio 4
(define (geometric-mean l)
  (exp (/ (log (fold * 1 l)) (length l))))

(pp (geometric-mean '(1 2 3 4 5)))

;;Ejercicio 5
(define (harmonic-mean l)
  (/ (+ (length l) 0.0)  (fold + 0 
          (map (lambda (n) (/ 1 n))
               l))))

(pp (harmonic-mean '(1 2 3 4 5)))

;;Ejercicio 6
(define (arithmetic-mean l)
  (mean l))

(pp (arithmetic-mean '(1 2 3 4 5)))

;;Ejercicio 7
(define (weighted-mean l-pairs)
  (/ 
   (fold + 0 (map (lambda (l) (* (car l) (cadr l)))
                  l-pairs))
   (fold + 0 (map cadr l-pairs))))

(pp (weighted-mean '((1 0.5) (4 0.8) (3 0.4))))

;;Ejercicio 8
(define (quadratic-mean l)
  (sqrt 
   (/ 
    (fold + 0 
          (map (lambda (n) (expt n 2))
               l))
    (length l))))

(pp (quadratic-mean '(1 2 3 4 5)))

;;Ejercicio 9

;;Ejercicio 10
(define (mid-range l-pairs)
  (/ (+ (maximum l-pairs) (minimum l-pairs)) 2.0))

(pp (mid-range '(1 4 2 3 6 5)))




  
