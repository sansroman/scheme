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
    ((null? lat)#f)
    (else (or (equal? (car lat) a)
              (member? a (cdr lat)))))))

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

(define o*
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


(define quotient
  (lambda (n m)
  (cond
    ((< n m)0)
    (else (add1 (quotient (o- n m)m))))))
(define length
  (lambda (lat)
  (cond
    ((null? lat) 0)
    (else (add1 (length (cdr lat)))))))
    
(define pick
      (lambda (n lat)
      (cond
        ((zero? (sub1 n))(car lat))
        (else (pick (sub1 n)(cdr lat))))))
(define rempick
  (lambda (n lat)
  (cond
    ((zero? (sub1 n))(cdr lat))
    (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(define no-nums
  (lambda (lat)
  (cond
    ((null? lat) '())
    ((number? (car lat)) (no-nums (cdr lat)))
    (else (cons (car lat) (no-nums (cdr lat)))))))

(define all-nums
  (lambda (lat)
  (cond
    ((null? lat) '())
    ((number? (car lat)) (cons (car lat) (all-nums(cdr lat))))
    (else (all-nums (cdr lat))))))

(define eqan?
  (lambda (n1 n2)
  (cond
    ((and (number? n1) (number? n2)) (= n1 n2))
    ((or (number? n1) (number? n2)) #f)
    (else (eq? n1 n2)))))

(define occur
  (lambda (a lat)
  (cond
    ((null? lat) 0)
    ((eqan? (car lat) a) (add1 (occur a (cdr lat))))
    (else (occur a (cdr lat))))))
(define one?
  (lambda (n)
  (= n 1)))

(define rember*
  (lambda (a lat)
  (cond
    ((null? l) '())
    ((atom? (car l)) 
      (cond
        ((eq? (car l) a) (rember* a (cdr l)))
        (else (cons (car l)(rember* a (cdr l))))))
    (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(define insertR*
  (lambda (new old l)
  (cond
    ((null? l) '())
    ((atom? (car l)) 
      (cond
        ((eq? (car l) old) (cons (car l) (cons new (insertR* new old (cdr l)))))
        (else (cons (car l) (insertR* new old (cdr l))))))
    (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(define occur*
  (lambda (a l)
  (cond
    ((null? l) 0)
    ((atom? (car l)) 
      (cond
        ((eq? (car l) a) (add1 (occur* a (cdr l))))
        (else (occur* a (cdr l)))))
    (else (o+ (occur* a (car l)) (occur* a (cdr l)))))))

(define subst*
  (lambda (new old l)
  (cond
    ((null? l) '())
    ((atom? (car l)) 
      (cond
        ((eq? (car l) old) (cons new (subst* new old (cdr l))))
        (else (cons (car l) (subst* new old (cdr l))))))
    (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(define insertL*
  (lambda (new old l)
  (cond 
    ((null? l) '())
    ((atom? (car l))
      (cond
        ((eq? (car l) old) (cons new l))
        (else (cons (car l) (insertL* new old (cdr l))))))
    (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(define member*
  (lambda (a l)
  (cond
    ((null? l) #f)
    ((atom? (car l))
      (cond
        ((eq? (car l) a) #t)
        (else (member* a (cdr l)))))
    (else (or (member* a (car l)) (member* a (cdr l)))))))
  
(define leftmost
  (lambda (l)
  (cond
    ((atom? (car l)) (car l))
    (else (leftmost (car l))))))

(define eqlist?
  (lambda (l1 l2)
  (cond 
    ((and (null? l1) (null? l2)) #t)
    ((or (null? l1) (null? l2)) #f)
    ((and (atom? (car l1)) (atom? (car l2))) (and (eq? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
    ((or (atom? (car l1)) (atom? (car l2))) #f)
    (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))

(define equal?
  (lambda (s1 s2)
  (cond
    ((and (atom? s1)(atom? s2)) (eq? s1 s2))
    ((or (atom? s1) (atom? s2)) #f)
    (else (eqlist? s1 s2)))))
(define numbered?
  (lambda (aexp)
  (cond
    ((atom? aexp)(number? aexp))
    (else
      (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))
(define value
  (lambda (nexp)
  (cond
    ((atom? nexp) nexp)
    ((eq? (car (cdr nexp)) 'o+) o+ (value (car nexp)))
    ((eq? (car (cdr nexp)) 'o*) o* (value (car nexp)))
    ((eq? (car (cdr nexp)) 'o^) o^ (value (car nexp)))
    )))
  
(define lst-sub-exp
  (lambda (aexp) (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp) (car (cdr (cdr aexp)))))

(define operator
  (lambda (aexp) (car aexp)))

(define set?
  (lambda (lat)
  (cond
    ((null? lat) #t)
    ((member? (car lat) (cdr lat)) #f)
    (else (set? (cdr lat))))))

(define makeset
  (lambda (lat)
  (cond
    ((null? lat) '())
    ((member? (car lat) (cdr lat)) (makeset(cdr lat)))
    (else (cons (car lat) (makeset (cdr lat)))))))


(define subset?
  (lambda (set1 set2)
  (cond
    ((null? set1) #t)
    ((member? (car set1) set2) (subset? (cdr set1) set2))
  (else #f))))

; Their implementation
(define eqset?
  (lambda (set1 set2)
  (cond
    ((and (null? set1) (null? set2)) #t)
    ((or (null? set1) (null? set2)) #f)
    (else (eqset? (cdr set1) (multirember (car set1) set2))))))

(define eqset?
  (lambda (set1 set2)
  (and (subset? set1 set2) (subset? set2 set1))))

(define intersect?
  (lambda (set1 set2)
  (cond
    ((null? set1) #f)
    ((member? (car set1) set2) #t)
    (else (intersect? (cdr set1) set2)))))

(define intersect
  (lambda (set1 set2)
  (cond
    ((null? set1) '())
    ((member? (car set1) set2) (cons (car set1) (intersect (cdr set1) set2)))
    (else (intersect (cdr set1) set2)))))

(define union
  (lambda (set1 set2)
  (cond
    ((null? set2) set1)
    ((member? (car set2) set1) (union set1 (cdr set2)))
    (else (cons (car set2) (union set1 (cdr set2)))))))

(define intersectall
  (lambda (l-set)
  (cond
    ((null? (cdr l-set)) (car l-set))
    (else (intersect (car l-set) (intersectall (cdr l-set)))))))

(define a-pair?
  (lambda (x)
  (cond
    ((atom? x)#f)
    ((null? x)#f)
    ((null? (cdr x)) #f)
    ((null? (cdr (cdr x))) #t)
    (else #f))))

(define first
  (lambda (p)
  (cond
    (else (car p)))))

(define second
  (lambda (p)
  (cond
    (else (car (cdr p))))))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))

(define build
  (lambda (s1 s2)
  (cond 
    (else (cons s1 (cons s2 '()))))))

(define revrel
  (lambda (rel)
  (cond
    ((null? rel) '())
    (else (cons (build (second (car rel)) (first (car rel))) (revrel (cdr rel)))))))

(define revpair
  (lambda (pair)
  (build (second pair) (first pair))))

(define seconds
  (lambda (rel)
  (cond
    ((null? rel) '())
    (else (cons  (car (cdr (car rel))) (seconds (cdr rel)))))))

(define eq?-c
  (lambda (a)
  (lambda (x)
  (eq? x a))))

(define rember-f
  (lambda (test? a l)
  (cond
    ((null? l) '())
    (else (cond
      ((test? (car l) a) (cdr l))
      (else (cons (car l) (rember-f test? a (cdr l)))))))))
    
(define insertL-f
  (lambda (test?)
  (lambda (new old l)
  (cond
    ((null? l) '())
    ((test? (car l) old)
      (cons new (cons old (cdr l))))
    (else (cons (car l)
      ((insertL-f test?) new old (cdr l))))))))

(define multirember-f
  (lambda (test?)
  (lambda (a lat)
  (cond
    ((null? lat) '())
    ((test? a (car lat)) ((multirember-f test?) a (cdr lat)))
    (else (cons (car lat) ((multirember-f test?) a (cdr lat))))))))

(define eq?-tuna
  (eq?-c 'tuna))

(define multiremberT
  (lambda (test? lat)
  (cond
    ((null? lat) '())
    ((test? (car lat)) (multiremberT test? (cdr lat)))
    (else (cons (car lat) (multiremberT test? (cdr lat)))))))


(define multiinsertLR
    (lambda (new oldL oldR lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) oldL) (cons new (cons oldL (multiinsertLR new oldL oldR (cdr lat)))))
      ((eq? (car lat) oldR) (cons oldR (cons new (multiinsertLR new oldL oldR (cdr lat)))))
      (else (cons (car lat) (multiinsertLR new oldL oldR (cdr lat)))))))


(define keep-looking
  (lambda (a sorn lat)
  (cond
    ((number? sorn)
    (keep-looking a (pick sorn lat) lat))
    (else (eq? sorn a)))))

(define A
  (lambda (n m)
  (cond
    ((zero? n) (add1 m))
    ((zero? m) (A (sub1 n) 1))
    (else (A (sub1 n) (A n (sub1 m)))))))

(define eternity
  (lambda (x)
  (eternity x)))

(define length
  (lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 (length (cdr l)))))))

(((lambda (mk-length)
  (mk-length mk-length))
  (lambda (mk-length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 ((mk-length eternity) (cdr l)))))))) '(apples))


(lambda (l)
  (cond
    ((null? l) 0)
    (else (add1 (eternity (cdr l))))))

((lambda (length)
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l))))))) eternity)
(define Y
  (lambda (le)
   ((lambda (f) (f f))
    (lambda (f)
      (le (lambda (x) ((f f) x)))))))

  (define new-entry build)


  (define lookup-in-entry
    (lambda (name entry entry-f)
      (lookup-in-entry-help name
        (first entry)
        (second entry)
        (entry-f))))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond
      ((null? names) (entry-f name))
      ((eq? (car names) name) (car values))
      (else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))

(define extend-table cons)


(define lookup-in-table
  (lambda (name table table-f)
  (cond
    ((null? table) (table-f name))
    (else (lookup-in-entry name (car table) (lambda (name)
        (lookup-in-table name (cdr table) table-f)))))))



(define atom-to-action
  (lambda (e)
    (cond
      ((number? e) *const)
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e 'cons) *const)
      ((eq? e 'car) *const)
      ((eq? e 'cdr) *const)
      ((eq? e 'null?) *const)
      ((eq? e 'eq?) *const)
      ((eq? e 'atom?) *const)
      ((eq? e 'zero?) *const)
      ((eq? e 'add1) *const)
      ((eq? e 'sub1) *const)
      ((eq? e 'number?) *const)
      (else *identifier))))


(define list-to-action
  (lambda (e)
  (cond
    ((atom? (car e)) 
      (cond
        ((eq? (car e ) 'quote) *quote)
        ((eq? (car e) 'lambda) *lambda)
        ((eq? (car e) 'cond) *cond)
        (else *application)))
  (else *application))))


(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define value
  (lambda (e)
    (meaning e '())))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build ('primitive) e)))))

(define *quote
  (lambda (e table)
  (text-of e)))

(define text-of second)

(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define initial-table
  (lambda (name)
  (car '())))


(define *lambda
  (lambda (e table)
    (build 'non-primitive (cons table (cdr e)))))


(define table-of first)

(define formals-of second)

(define body-of third)

(define evcon
  (lambda (lines table)
  (cond
    ((else? (question-of (car lines))) (meaning (answer-of (car lines)) table))
    ((meaning (question-of (car lines)) table) (meaning (answer-of (car lines )) table))
     (else (evcon (cdr lines) table)))
))


(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? x 'else))
      (else #f))))
(define question-of first)

(define answer-of second)

(define *cond
  (lambda (e table)
  (evcon (cond-lines-of e) table)))
(define cond-lines-of cdr)

(define evils
  (lambda (args table)
    (cond
      ((null? args) '())
      (else (cons (meaning (car args) table) (evils (cdr args) table))))))


(define *application
  (lambda (e table)
    (apply
      (meaning (function-of e) table)
      (evils (arguments-of e) table))))

(define function-of car)

(define arguments-of cdr)

(define primitive?
  (lambda (l)
  (eq? (first l) 'primitive)))

(define non-primitive?
  (lambda (l)
  (eq? (first l) 'non-primitive)))

(define apply
  (lambda (fun vals)
  (cond
    ((primitive? fun) (apply-primitive (second fun) vals))
    ((non-primitive? fun) (apply-closure (second fun) vals))
)))