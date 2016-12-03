 ;;; Scheme Interpreter Extension written in Scheme
 ;Clayton Samson
 ;CSC 4101 Project 3
 
 (define (+ . l)
   (if (null? l) 0
       (b+ (car l) (apply + (cdr l)))))
 
 ; Test incorrect return value
 (define (caar x) (car (cdr x)))
 
 ; Test wrong number of arguments
 (define (list l) l)
 
 ;; Binary and function
 (define (and x y) (if x y x))
 
 ;; Binary map function
 (define (map f l)
   (if (null? l) '()
       (cons (f (car l)) (map f (cdr l)))))
 
;;comparison operations
;eqv?
(define (eqv? x y . more)
  (define (eqv x y)
    (cond ((and (string? x) (string? y)) (eq? x y))
          ((and (boolean? x) (boolean? y)) (eq? x y))
          ((and (symbol? x) (symbol? y)) (eq? x y))
          ((and (number? x) (number? y)) (= x y))
          ((and (null? x) (null? y)) #t)
          (else #f)))
  (if (b> (length more) 0)
    (if (eqv x y)
      (apply eqv? (cons y more))
      #f)
    (eqv x y)))

;equal?
(define (equal? x y . more)
  (define (equal x y)
    (cond (and (pair? x) (pair? y))
            (and (equal? (car x) (car y))
                 (equal? (cdr x) (cdr y)))
          (else (eqv? x y))))
  (if (b> (length more) 0)
    (if (equal x y)
      (apply equal? (cons y more))
      #f)
    (equal x y)))

;;The n-ary integer comparison operations
;=
(define (= x y . more)
  (if (b> (length more) 0)
    (if (b= x y)
      (apply = (cons y more))
      #f)
    (b= x y)))

;<
(define (< x y . more)
  (if (b< (length more) 0)
    (if (b< x y)
      (apply < (cons y more))
      #f)
    (b< x y)))

;>
(define (> x y . more)
  (if (b> (length more) 0)
    (if (b> x y)
      (apply > (cons y more))
      #f)
    (b> x y)))

;<=
(define (<= x y . more)
  (define (lte x y) (or (eq? x y) (b< x y)))
  (if (<= (length more) 0)
    (if (lte x y)
      (apply <= (cons y more))
      #f)
    (lte x y)))

;>=
(define (>= x y . more)
  (define (gte x y) (or (eq? x y) (b> x y)))
  (if (>= (length more) 0)
    (if (gte x y)
      (apply >= (cons y more))
      #f)
    (gte x y)))

;;The test predicates
;zero?
(define (zero? x)
	(b= x 0))

;positive?
(define (positive? x)
  (b> x 0))

;negative?
(define (negative? x)
  (b< x 0))

;odd?
(define (odd? x)
  (not (even? x)))

;even?
(define (even? x)
  (if (b= x 0) #t
    (if (b= (b- x 1) 0) #f
      (even? (b- x 2)))))

;;The n-ary arithmetic operations
; max
(define (max x . args)
  (if (null? args)
    x
    (apply max (cons (if (b> x (car args)) x (car args))
                     (cdr args)))))

;min
(define (min x . args)
  (if (null? args)
    x
    (apply min (cons (if (b< x (car args)) x (car args))
                     (cdr args)))))

;reduce
(define (reduce fn start coll)
  (if (null? coll)
    start
    (fn (car coll)
        (reduce fn start (cdr coll)))))

;+
(define (+ . args)
  (reduce b+ 0 args))

;-
(define (- x . args)
  (if (null? args)
    (b- (b* x 2) x)
    (reduce b- 0 (cons x args))))

;*
(define (* . args)
  (reduce b* 1 args))

;;The boolean operations
;not
(define (not x)
  (if x #f #t))

;and
(define (and . args)
   (if (null? (cdr args)) (car args)
      (if (eq? (car args) #f)
        #f
        (apply and (cdr args)))))

;filter
(define (filter pred coll)
  (if (null? coll)
    coll
    (if (pred (car coll))
      (cons (car coll)
            (filter pred (cdr coll)))
      (filter pred (cdr coll)))))

;compact
(define (compact coll)
  (filter (lambda (x) (if x x #f)) coll))

;or
(define (or . args)
  (let ((truthy-args (compact args)))
    (if (null? truthy-args)
      #f
      (car truthy-args))))

;;The list functions
;caar
(define caar (lambda (x) (car (car x))))
;cadr
(define cadr (lambda (x) (car (cdr x))))
;cdar
(define cdar (lambda (x) (cdr (car x))))
;cddr
(define cddr (lambda (x) (cdr (cdr x))))
;caaar
(define caaar (lambda (x) (car (car (car x)))))
;caadr
(define caadr (lambda (x) (car (car (cdr x)))))
;cadar
(define cadar (lambda (x) (car (cdr (car x)))))
;caddr
(define caddr (lambda (x) (car (cdr (cdr x)))))
;cdaar
(define cdaar (lambda (x) (cdr (car (car x)))))
;cdadr
(define cdadr (lambda (x) (cdr (car (cdr x)))))
;cddar
(define cddar (lambda (x) (cdr (cdr (car x)))))
;cdddr
(define cdddr (lambda (x) (cdr (cdr (cdr x)))))
;caaaar
(define caaaar (lambda (x) (car (car (car (car x))))))
;caaadr
(define caaadr (lambda (x) (car (car (car (cdr x))))))
;caadar
(define caadar (lambda (x) (car (car (cdr (car x))))))
;caaddr
(define caaddr (lambda (x) (car (car (cdr (cdr x))))))
;cadaar
(define cadaar (lambda (x) (car (cdr (car (car x))))))
;cadadr
(define cadadr (lambda (x) (car (cdr (car (cdr x))))))
;caddar
(define caddar (lambda (x) (car (cdr (cdr (car x))))))
;cadddr
(define cadddr (lambda (x) (car (cdr (cdr (cdr x))))))
;cdaaar
(define cdaaar (lambda (x) (cdr (car (car (car x))))))
;cdaadr
(define cdaadr (lambda (x) (cdr (car (car (cdr x))))))
;cdadar
(define cdadar (lambda (x) (cdr (car (cdr (car x))))))
;cdaddr
(define cdaddr (lambda (x) (cdr (car (cdr (cdr x))))))
;cddaar
(define cddaar (lambda (x) (cdr (cdr (car (car x))))))
;cddadr
(define cddadr (lambda (x) (cdr (cdr (car (cdr x))))))
;cdddar
(define cdddar (lambda (x) (cdr (cdr (cdr (car x))))))
;cddddr
(define cddddr (lambda (x) (cdr (cdr (cdr (cdr x))))))

;list
(define (list . args)
	args)

;lenght
(define (length coll)
  (if (null? coll)
    0
    (b+ 1 (length (cdr coll)))))

;append
(define (append coll1 coll2)
  (if (null? coll1)
    coll2
    (cons (car coll1)
          (append (cdr coll1) coll2))))

;reverse
(define (reverse coll)
  (if (null? coll) coll
     (append (reverse (cdr coll)) (list (car coll)))))

;;The set and association list operations
;generic-member
(define (generic-member cmp obj lst)
  (cond
    ((null? lst) #f)
    ((cmp obj (car lst)) lst)
    (else (generic-member cmp obj (cdr lst)))))

;memq
(define (memq obj lst)
  (generic-member eq? obj lst))

;memv
(define (memv obj lst)
  (generic-member eqv? obj lst))

;member
(define (member obj lst)
  (generic-member equal? obj lst))

;generic-association
(define (generic-association cmp obj alst)
  (cond
    ((null? alst) #f)
    ((cmp obj (caar alst)) (car alst))
    (else (generic-association cmp obj (cdr alst)))))

;assq
(define (assq obj alst)
  (generic-association eq? obj alst))

;assv
(define (assv obj alst)
  (generic-association eqv? obj alst))

;assoc
(define (assoc obj alst)
  (generic-association equal? obj alst))

;;The higher-order functions
;generic-map
(define (generic-map fn coll)
  (if (null? coll)
    coll
    (cons (fn (car coll))
          (generic-map fn (cdr coll)))))

;map
(define (map fn . lists)
  (if (apply and (generic-map null? lists))
    '()
    (let ((cars (generic-map car lists))
          (cdrs (generic-map cdr lists)))
      (cons (apply fn cars)
            (apply map (cons fn cdrs))))))

;for-each
(define (for-each f coll)
  (if (null? coll) coll
    (begin (f (car coll))
           (for-each f (cdr coll)))))