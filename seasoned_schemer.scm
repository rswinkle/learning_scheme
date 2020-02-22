

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))



(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))

; Chapter 11 

(define is-first?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (eq? (car lat) a)))))

(define is-first-b?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (two-in-a-row? lat))))))

(define two-in-a-row-old?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else
        (or
          (is-first? (car lat) (cdr lat))
          (two-in-a-row? (cdr lat)))))))

(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else
          (is-first-b? (car lat) (cdr lat))))))


(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) preceding)
                (two-in-a-row-b? (car lat) (cdr lat)))))))

(define two-in-a-row-final?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (two-in-a-row-b? (car lat) (cdr lat))))))


; sum is sum of numbers seen so far, they name it sonssf...
(define sum-of-prefixes-b
  (lambda (sum tup)
    (cond
      ((null? tup) '())
      (else (cons (+ sum (car tup))
                  (sum-of-prefixes-b
                    (+ sum (car tup)) (cdr tup)))))))



(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-b 0 tup)))



; return nth s-expr in l (1 based indexing)
(define pick
  (lambda (n l)
    (cond
      ((= n 1) (car l))
      (else (pick (sub1 n) (cdr l))))))


; rev-pre = reversed prefix
(define scramble-b
  (lambda (tup rev-pre)
    (cond
      ((null? tup) '())
      (else
        (cons (pick (car tup) (cons (car tup) rev-pre))
              (scramble-b (cdr tup) (cons (car tup) rev-pre)))))))

(define scramble
  (lambda (tup)
    (scramble-b tup '())))



; Chapter 12
(define multirember
  (lambda (a lat)
    (letrec
      ((mr (lambda (lat)
             (cond
               ((null? lat) '())
               ((eq? a (car lat))
                (mr (cdr lat)))
               (else
                 (cons (car lat)
                       (mr (cdr lat))))))))
      (mr lat))))


(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? (car lat) a)
         ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat)
                    ((multirember-f test?) a (cdr lat))))))))




