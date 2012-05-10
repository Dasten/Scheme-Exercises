;;Scheme exercises for practice 1 - Carlos Belmonte

;;Exercise 1 - (list-operation '(a b c d e f g h) 'c) -> '(a b d e f g h)
(define (list-operation l argl)
  (cond
   ((null? l) '())
   ((eq? (car l) argl) 
    (list-operation (cdr l) argl))
   (else
    (cons (car l) (list-operation (cdr l) argl)))))

(pp (list-operation '(a b c d e f) 'b))

;;Exercise 2 - (insert-right '(a b c d e f g h) 'c 'M) -> '(a b c M d e f g h)
(define (insert-right l argl1 argl2)
(cond 
   ((null? l)  '())
   ((eq? (car l) argl1)
    (cons argl1 (cons argl2 (insert-right (cdr l) argl1 argl2))))
   (else
    (cons (car l) (insert-right (cdr l) argl1 argl2)))))

(pp (insert-right '(a b c d e f) 'b 'Z))

;;Exercise 3 - (insert-left '(a b c d e f g h) 'c 'M) -> '(a b M c d e f g h)
(define (insert-left l argl1 argl2)
  (cond
   ((null? l) '())
   ((eq? (car l) argl1) 
    (cons argl2 (cons argl1 (insert-left (cdr l) argl1 argl2))))
   (else
    (cons (car l) (insert-left (cdr l) argl1 argl2)))))

(pp (insert-left '(a b c d e f) 'b 'Z))

;;Exercise 4 - (sust '(a b c a e f a h) 'a 'M) -> '(M b c a e f a h)
(define (sust l argl1 argl2)
  (cond
   ((null? l) '())
   ((eq? (car l) argl1)
    (cons argl2 (cdr l)))
   (else
    (cons (car l) (sust (cdr l) argl1 argl2)))))

(pp (sust '(a b c d b f b) 'b 'Z))

;;Exercise 5 - (element-append '(a b c d e f g h) 'M) -> '(a b c d e f g h M) 
(define (element-append l argl)
  (cond
   ((null? l) (cons argl '()))  
   (else
    (cons (car l) (element-append (cdr l) argl)))))

(pp (element-append '(a b c d e f g) 'H))

;;Exercise 6 - (msust '(a b c a e f a h) 'a 'M) -> '(M b c M e f M h)
(define (msust l argl1 argl2)
  (cond 
   ((null? l) '())
   ((eq? (car l) argl1)
    (cons argl2 (msust (cdr l) argl1 argl2)))
   (else
    (cons (car l) (msust (cdr l) argl1 argl2)))))

(pp (msust '( a b a c a d a) 'a 'Z))

;;Exercise 7 - (mrember '(a b c a e f a h) 'a) -> '(b c e f h)
(define (mrember l argl)
  (cond
   ((null? l) '())
   ((eq? (car l) argl)
    (mrember (cdr l) argl))
   (else
    (cons (car l) (mrember (cdr l) argl)))))

(pp (mrember '(a b a c a d a) 'a))

;;Exercise 8 - (nth-element '(a b c d e f g h) 3) -> 'd
(define (nth-element l argl)
  (cond
   ((null? l) '())
   ((eq? 0 argl)
    (car l))
   (else
    (nth-element (cdr l) (- argl 1)))))

(pp (nth-element '(a b c d e f g) 4))

;;Exercise 9 - (nth-rember '(a b c d e f g h) 3) -> '(a b c e f g h)
(define (nth-rember l argl)
  (cond
   ((null? l) '())
   ((eq? 0 argl)
    (cdr l))
   (else
    (cons (car l) (nth-rember (cdr l) (- argl 1))))))

(pp (nth-rember '(a b c d e f g) 4))

;;Exercise 10 - (element-occur '(a b c a e f a h) 'a) -> 3
(define (element-occur l argl)
  (cond
   ((null? l) 0)
   ((eq? (car l) argl)
    (+ 1 (element-occur (cdr l) argl)))
   (else
    (element-occur (cdr l) argl))))

(pp (element-occur '(a b a c a d a) 'a))

;; 11- (length '(a b c d e f g h)) -> 8
(define (length l)
  (cond
   ((null? l) 0)
   ((eq? (car l) '())
    (cdr l))
   (else
    (+ 1 (length (cdr l))))))

(pp (length '(a b c d e f g)))