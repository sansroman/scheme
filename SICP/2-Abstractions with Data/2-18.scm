(define (reverse-test list)
  (if (null? (cdr list)
             (car list)
             (cons (reverse (cdr list)) (car list)))))
