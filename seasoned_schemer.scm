

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

; What difference does it make whether you put the lambda at the top
; or in the value section of the letrec?  Seems like no real difference
; to me, W? aka two-in-a-row-b? is already hidden/protected
(define two-in-a-row-final?
  (lambda (lat)
    (letrec
      ((W? (lambda (preceding lat)
        (cond
          ((null? lat) #f)
          (else (or (eq? (car lat) preceding)
                    (W? (car lat) (cdr lat))))))))
      (cond
        ((null? lat) #f)
        (else (W? (car lat) (cdr lat)))))))


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
    (letrec
      ((m-f
        (lambda (a lat)
          (cond
            ((null? lat) '())
            ((test? (car lat) a)
             (m-f a (cdr lat)))
            (else (cons (car lat)
                        (m-f a (cdr lat))))))))
      m-f)))



(define member?
  (lambda (a lat)
    (letrec
      ((yes? (lambda (l)
               (cond
                 ((null? l) #f)
                 ((eq? (car l) a) #t)
                 (else (yes? (cdr l)))))))
      (yes? lat))))


(define union
  (lambda (s1 s2)
    (cond
      ((null? s1) s2)
      ((member? (car s1) s2) (union (cdr s1) s2))
      (else (cons (car s1) (union (cdr s1) s2))))))

(define union
  (lambda (set1 set2)
    (letrec
      ((U (lambda (set)
            (cond
              ((null? set) set2)
              ((member? (car set) set2) (U (cdr set)))
              (else (cons (car set) (U (cdr set)))))))
       (M?
         (lambda (a lat)
           (letrec
             ((N? (lambda (lat)
               (cond
                 ((null? lat) #f)
                 ((eq? (car lat) a) #t)
                 (else (N? a (cdr lat)))))))
             (N? lat)))))
      (U set1))))





