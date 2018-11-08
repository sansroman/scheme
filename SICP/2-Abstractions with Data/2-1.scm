(define (gdc a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(define (numer x) (car x))
(define (denom x) (cdr x))
(
 define (abs n)
  (if (< n 0)
      (- 0 n)
      n))

(define (make-rat n d)
  (let ((g ((if (< d 0) - +)(gcd n d))))
    (cons (/ n  g) (/ d g))))




