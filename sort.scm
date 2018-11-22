(define (make-node left-node right-node integral total color)
  (list left-node right-node integral total color))

(define (left-node node) (car node))

(define (right-node node) (cadr node))

(define (node-integral node) (caddr node))

(define (node-total node) (cadddr node))

(define (node-color node) (car (cddddr node)))

(define (red? node)
  (if (null? node) #f
      (car (cddddr node))))

(define (add-total node)
  (make-node (left-node node) (right-node node) (node-integral node) (+ 1 (node-total node)) (node-color node)))

(define (toggleColor node)
  (make-node (left-node node) (right-node node) (node-integral node) (node-total node) (not (node-color node))))

(define (toRed node)
  (make-node (left-node node) (right-node node) (node-integral node) (node-total node) #t))

(define (toBlack node)
  (make-node (left-node node) (right-node node) (node-integral node) (node-total node) #f))

(define (rotateRight node)
  (let ((left (left-node node)))
    (make-node (left-node left)
               (make-node (right-node left)
                          (right-node node)
                          (node-integral node)
                          (node-total node)
                          (node-color node))
               (node-integral left)
               (node-total left)
               (node-color left))))

(define (rotateLeft node)
  (let ((right (right-node node)))
    (make-node (make-node (left-node node)
                          (left-node right)
                          (node-integral node)
                          (node-total node)
                          (node-color node))
               (right-node right)
               (node-integral right)
               (node-total right)
               (node-color right))))

(define (flipColors node)
  (let ((left (left-node node))
        (right (right-node node)))
    (toggleColor (make-node (toggleColor left)
                            (toggleColor right)
                            (node-integral node)
                            (node-total node)
                            (node-color node)))))

;; (define (delete node integral))

(define (put root integral)
  (define (insert node)
    (cond
     ((null? node) (make-node '() '() integral 1 #t))
     ((eq? (node-integral node) integral) (add-total node))
     ((< (node-integral node) integral) (repair (make-node (left-node node)
                                                           (insert (right-node node))
                                                           (node-integral node)
                                                           (node-total node)
                                                           (node-color node))))
     (else (repair (make-node (insert (left-node node))
                              (left-node node)
                              (node-integral node)
                              (node-total node)
                              (node-color node))))))
  (define (repair node)
    (let ((left (left-node node))
          (right (right-node node)))
      (cond
       ((and (red? left) (red? right)) (toggleColor node))
       ((and (red? right) (not (red? left))) (rotateLeft node))
       ((and (not (red? right)) (red? left)) (rotateRight node))
       (else node))))
  (toBlack (insert root)))


(define (find node integral)
  (cond
   ((null? node) (error "integral not found" integral))
   ((eq? (node-integral node) integral) node)
   ((< (node-integral node) integral) (find (right-node node) integral))
   (else (find (left-node node) integral))))



(define (sample-data n)
  (define (gen ti result)
    (if (> ti n) result
        (gen (+ ti 1) (put result (random 1000)))))
  (gen 0 '()))

(display (current-date))
(sample-data 300000)
(display (current-date))
