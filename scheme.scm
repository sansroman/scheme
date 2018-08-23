(define (1+ x)(+ x 1))
(define pi (* 4 (atan 1.0)))
(define (radian deg)
  (* deg (/ pi 180.0)))
(define (fact n)
  (if (= n 1)
      1
      (* n (fact (- n 1)))))
(define (fact-tail n)(fact-rec n n))
(define (fact-rec n p)
  (if (= n 1)
      p
      (let ((m (- n 1)))
      (fact-rec m (* m p)))))
(define (fact-letrec n)
  (letrec ((iter (lambda (n1 p)
                   (if (= n1 1)
                       p
                       (let ((m (- n1 1)))
                         (iter m (* m p)))))))
    (iter n n)))

(define (member-if proc ls)
  (cond
   ((null? ls) #f)
   ((proc (car ls)) ls)
   (else (member-if proc (cdr ls)))))
(define (read-file file-name)
  (let ((p (open-input-file file-name)))
    (let loop((ls1 '()) (c (read-char p)))
      (if (eof-object? c)
          (begin
            (close-input-port p)
            (list->string (reverse ls1)))
          (loop (cons c ls1) (read-char p))))))

(define wc '((hi . 3) (everybody . 5) (nice . 3) (to . 10) (meet . 4) (you . 8)))

(define (kfact n k)
  (if (= n 1)
      (k 1)
      (kfact (- n 1) (lambda (x) (k (* n x))))))
(define atom?
  (lambda (x)
  (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
  (cond
    ((null? l) #t)
    ((atom? (car l))(lat? (cdr l)))
    (else #f))))

(define member?
  (lambda  (a lat)
  (cond
    ((null? lat) #f)
    (else (or (eq? (car lat) a)
              (member? a (cdr a)))))))

(define rember
  (lambda  (a lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat) a)(cdr lat))
    (else (cons (car lat) (rember a (cdr lat)))))))
(define firsts
  (lambda (l)
  (cond
    ((null? l) '())
    (else (cons (car (car l))(firsts (cdr l)))))))

(define insertR
  (lambda (new old lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat) old) (cons (car lat) (cons new (cdr lat))))
    (else (cons (car lat) (insertR new old (cdr lat)))))))

(define insertL
  (lambda (new old lat)
  (cond 
    ((null? lat) '())
    ((eq? (car lat) old) (cons new lat))
    (else (cons (car lat) (insertL new old (cdr lat)))))))

(define subst
  (lambda (new old lat)
  (cond
    ((null? lat) '())
    ((eq? (car lat) old) (cons new (cdr lat)))
    (else (cons (car lat) (subst new old (cdr lat)))))))

(define subst2
  (lambda (new o1 o2 lat)
  (cond
    ((null? lat) '())
    ((or (eq? (car lat) o1)(eq? (car lat) o2)) (cons new (cdr lat)))
    (else (cons (car lat)(subst2 new o1 o2 (cdr lat)))))))

  (define multirember
    (lambda (a lat)
    (cond
      ((null? lat) '())
      (else
        (cond
          ((eq? (car lat) a) (multirember a (cdr lat)))
          (else (cons (car lat) (multirember a (cdr lat)))))))))

(define multiinsertR
  (lambda (new old lat)
  (cond
    ((null? lat) '())
    (else
      (cond
        ((eq? (car lat) old)(cons  (car lat) (cons new (multiinsertR new old (cdr lat)))))
        (else (cons (car lat)(multiinsertR new old (cdr lat)))))))))

(define multisubst
  (lambda (new old lat)
  (cond
    ((null? lat) '())
    (else
      (cond
        ((eq? (car lat) old)(cons new (multiinsertR new old (cdr lat))))
        (else (cons (car lat)(multisubst new old (cdr lat)))))))))
(define add1
  (lambda (a)
  (+ a 1)))
(define sub1
  (lambda (a)
  (- a 1)))
(define zero?
  (lambda (a)
  (= a 0)))

(define o+
  (lambda (n m)
  (cond 
    ((zero? m)n)
    (else (add1 (o+ n (sub1 m)))))))

(define o-
  (lambda (n m)
  (cond 
    ((zero? m)n)
    (else (sub1 (o- n(sub1 m)))))))

(define addtup
  (lambda (tup)
  (cond
    ((null? tup) 0)
    (else (o+ (car tup) (addtup (cdr tup)))))))

(define x
  (lambda (n m)
  (cond
    ((zero? m) 0)
    (else (o+ n (x n (sub1 m)))))))

(define tup+
  (lambda (tup1 tup2)
  (cond 
    ((null? tup1) tup2) 
    ((null? tup2) tup1)
    (else (cons (o+ (car tup1) (car tup2))
                (tup+ (cdr tup1) (cdr tup2)))))))

(define expt
  (lambda (n m)
  (cond
    (zero? m) ))