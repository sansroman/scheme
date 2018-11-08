(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define empty-board '())


(define (enumerate-interval low high)

  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))


(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
                 s)))

(define (adjoin-position row col rest)
  (cons (list row col) rest))

(define (safe? k positions)
  (let ((trial (car positions))
        (trial-row (caar positions))
        (trial-col (cadar positions))
        (rest (cdr positions)))
    (accumulate (lambda (pos result)
                  (let ((row (car pos))
                        (col (cadr pos)))
                    (and (not (= (- trial-row trial-col)
                                 (- row col)))
                         (not (= (+ trial-row trial-col)
                                 (+ row col)))
                         (not (= trial-row row))
                         result)))
                true
                rest)))


(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

