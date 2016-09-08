(define (for-each proc items)
   (cond ((null? items) '())
         (else (proc (car items))
               (for-each proc (cdr items)))))


(define (enumerate-tree tree)
        (cond ((null? tree) '())
              ((not (pair? tree))(list tree))
              (else (append (enumerate-tree (car tree))
                            (enumerate-tree (cdr tree))))))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

;;; exercise 2.30
(define (square-tree tree) 
    (cond ((null? tree) '())
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree-higher tree)
    (map (lambda (sub-tree) 
        (if (pair? sub-tree)
            (square-tree-higher sub-tree)
            (* sub-tree sub-tree)))
    tree))
    

;;; exercise 2.31
(define (tree-map op seq)
    (map (lambda (sub-tree)
        (if (pair? sub-tree)
            (tree-map sub-tree)
            (op sub-tree)))
    seq))





;;;exercise 2.33
(define (map p seq)
    (accumulate (lambda (x y) (cons (p y) x )) '() seq))


(define (append seq1 seq2)
    (accumulate cons seq2 seq1))

(define (length seq)
    (accumulate (lambda (x y) (+ 1 y)) 0 seq))


(define (count-leaves x)
    (cond ((null? x) 0)
          ((not (pair? x)) 1)
          (else (+ (count-leaves (car x))
            (count-leaves (cdr x))))))

;;;exercise 2.35

(define (count-leaves x) 
    (accumulate + 0 (map (lambda (sub-tree) 1)
                    (enumerate-tree x))))

;;;exercise 2.36
(define (accumulate-n op init seqs)
    (if (null? (car seqs))
        '()
        (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))



(define (flatmap proc seq)
    (accumulate append '() (map proc seq)))

(define (permutations s)
    (if (null? s)
        (list '())
        (flatmap (lambda (x)
            (map (lambda (p) (cons x p))
                (permutations (remove x s))))
        s)))


(define (remove item seq)
    (filter (lambda (x) (not (= x item)))
        seq))

(define (maap n) 
    (map (lambda (i)
        (map (lambda (j) (list i j))
            (enumerate 0 (- i 1))))
    (enumerate 0 n)))


(define (enumerate low high)
    (if (> low high)
        '()
        (cons low (enumerate (+ low 1) high))))



(define (fold-left op initial sequence)
    (define (iter result rest)
        (if (null? rest)
            result
            (iter (op result (car rest)) ;;; fold-left
                (cdr rest))))
    (iter initial sequence))


(define fold-right accumulate)



;;Exercise 3.2

(define (make-monitored func) 
    (let ((count 0))
        (define (get) count)
        (define (reset) (set! count 0))
        (define (dispatch m)
            (cond ((eq? m 'how) (get))
                  ((eq? m 'reset) (reset))
                  (else (begin (set! count (+ count 1)) 
                               (func m)))))
            dispatch ))




;;Exercise 3.3 

(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (reject any)
    "Incorrect password")
  (define (dispatch p m)
    (if (eq? p password)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT" m)))
        reject))
  dispatch)



;;Exercise 3.5

;; Monte Carlo
(define (monte-carlo trials experiment) 
   (define (iter trials-remaining trials-passed) 
     (cond ((= trials-remaining 0) 
            (/ trials-passed trials)) 
           ((experiment) 
            (iter (- trials-remaining 1) (+ trials-passed 1))) 
           (else 
            (iter (- trials-remaining 1) trials-passed)))) 
   (iter trials 0)) 

; random gen
(define (random-in-range low high) 
   (let ((range (- high low))) 
     (+ low (random range)))) 


(define (est-integral predicate x1 x2 y1 y2 trials) 
        (* (* (- x2 x1)
              (- y2 y1))
            (monte-carlo trials predicate )))


(define (within)

    (<= (+ (square (- (random-in-range -1 1) 1))
           (square (- (random-in-range -1 1) 1)))
        (square 2)
    )
 )


;; Exercise 3.7
(define (make-join acc oldpass newpass)

  (define (dispatch p m)
    (if (eq? p newpass) (acc oldpass m)
        (lambda (any) "Incorrect password")))
  dispatch)


;; Exercise 3.17

(define (count-pairs x) 
   (let ((encountered '())) 
     (define (helper x) 
       (if (or (not (pair? x)) (memq x encountered)) 
         0 
         (begin 
           (set! encountered (cons x encountered)) 
           (+ (helper (car x)) 
              (helper (cdr x)) 
              1)))) 
   (helper x))) 


;; Exercise 3.18

(define (has-cycle? xs)
  (define seen null)
  (define (cycle-aux ys)
    (cond ((null? ys) #f)
          ((memq (car ys) seen) #t)
          (else (set! seen (cons (car ys) seen))
                (cycle-aux (cdr ys)))))
  (cycle-aux xs))


















