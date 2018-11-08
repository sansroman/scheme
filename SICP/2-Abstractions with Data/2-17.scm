(define (last-pair l)
  (let (rest (cdr l))
    (if (null? rest)
        l
        (last-pair rest))))

