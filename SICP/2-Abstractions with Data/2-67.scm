(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))


(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))




(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
  (define (encode-symbol-1 result tree)
    (cond
     ((null? tree) '())
     ((leaf? tree)
      (if (eq? (symbol-leaf tree) symbol)
          result
          '()))
     (else
      (let ((left-iter (encode-symbol-1 (cons 0 result) (left-branch tree)))
            (right-iter (encode-symbol-1 (cons 1 result) (right-branch tree))))
        (cond ((and (not (null? left-iter)) (not (null? right-iter))) (error "malformed tree -- ENCODE-SYMBOL" tree))
              ((null? left-iter) right-iter)
              ((null? right-iter) left-iter)
              (else '()))))))
  (let ((encoded-symbol (reverse (encode-symbol-1 '() tree))))
    (if (null? encoded-symbol) (error "symbol not found" symbol)
        encoded-symbol)))

;; test data
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define sample-symbols '(A D A B B C A))
