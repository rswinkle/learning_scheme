
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
    (letrec
      ((S (lambda (sum, tup)
            (cond
              ((null? tup) '())
              (else (cons (+ sum (car tup))
                          (S (+ sum (car tup)) (cdr tup))))))))
    (S 0 tup))))



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
    (letrec
      ((P (lambda (tup rev-pre)
            (cond
              ((null? tup) '())
              (else
                (let ((rp (cons (car tup) rev-pre)))
                  (cons (pick (car tup) rp)
                        (P (cdr tup) rp))))))))
    (P tup '()))))



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




; Chapter 13


(define intersect
  (lambda (set1 set2)
    (letrec
      ((intersect-b (lambda (set)
        (cond
          ((null? set) '())
          ((member? (car set) set2)
           (cons (car set) (intersect-b (cdr set))))
          (else (intersect-b (cdr set)))))))
      (cond
        ((null? set2) '())
        (else (intersect-b set1))))))



; racket has
; call-with-current-continuation (call/cc is an alias)
;
; let/cc is simpler wrapper like the book's letcc
; (let/cc k body ...) is equivalent to (call/cc (lambda (k) body ...)

(define intersectall
  (lambda (lset)
    (let/cc hop
      (letrec
        ((intersectall-b
           (lambda (lset)
            (cond
              ((null? (car lset)) (hop '()))
              ((null? (cdr lset)) (car lset))
              (else (intersect (car lset) (intersectall-b (cdr lset)))))))
         (intersect
           (lambda (set1 set2)
            (letrec
              ((I (lambda (set)
                (cond
                  ((null? set) '())
                  ((member? (car set) set2)
                   (cons (car set) (I (cdr set))))
                  (else (I (cdr set)))))))
              (cond
                ((null? set2) (hop '()))
                (else (I set1)))))))
        (cond
          ((null? lset) '())
          (else (intersectall-b lset)))))))


(define rember
  (lambda (a lat)
    (letrec
      ((R (lambda (lat)
            (cond
              ((null? lat) '())
              ((eq? (car lat) a) (cdr lat))
              (else (cons (car lat) (R (cdr lat))))))))
      (R lat))))


(define rember-beyond-first
  (lambda (a lat)
    (letrec
      ((R (lambda (lat)
            (cond
              ((null? lat) '())
              ((eq? (car lat) a) '())
              (else (cons (car lat) (R (cdr lat))))))))
      (R lat))))


(define rember-upto-last
  (lambda (a lat)
    (let/cc skip
      (letrec
        ((R (lambda (lat)
              (cond
                ((null? lat) '())
                ((eq? (car lat) a) (skip (R (cdr lat))))
                (else (cons (car lat) (R (cdr lat))))))))
        (R lat)))))


; Chapter 14

; (let ((x1 a1) ... (xn an)) B) is shortcut for
; ((lambda (x1 ... xn) B) a1...an)


; (let () a...b) same as (begin a...b)

(define leftmost
  (lambda (l)
    (let/cc skip
      (letrec
        ((lm (lambda (l)
               (cond
                 ((null? l) '())
                 ((atom? (car l)) ((skip (car l))))
                 (else
                   (let ()
                     (lm (car l))
                     (lm (cdr l))))))))
      (lm l)))))


(define eqan?
  (lambda (a b)
    (cond
      ((and (number? a) (number? b)) (= a b))
      ((or (number? a) (number? b)) #f)
      (else (eq? a b)))))


; Is eqan? really necessary?
; eq? seems to work fine for numbers and other atoms
(define eqlist?
  (lambda (l1 l2)
    (cond
      ((null? l1) (null? l2))
      ((null? l2) #f)
      ((atom? (car l1))
       (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
      ((atom? (car l2)) #f)
      (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))



(define rember1*
  (lambda (a l)
    (letrec
      ((R (lambda (l)
            (cond
              ((null? l) '())
              ((atom? (car l))
               (cond
                 ((eq? (car l) a) (cdr l))
                 (else (cons (car l) (R (cdr l))))))
              (else
                (let ((av (R (car l))))
                  (cond
                    ((eqlist? (car l) av)
                     (cons (car l) (R (cdr l))))
                    (else (cons av (cdr l))))))))))
      (R l))))



; https://github.com/viswanathgs/The-Seasoned-Schemer
;(define-syntax letcc
;  (syntax-rules ()
;    ((letcc var body ...)
;     (call-with-current-continuation
;       (lambda (var)  body ... )))))


(define-syntax try
  (syntax-rules ()
    ((try var a . b)
     (let/cc success
       (let/cc var (success a)) . b))))



(define rm
  (lambda (a l oh)
    (cond
      ((null? l) (oh 'no))
      ((atom? (car l))
       (if (eq? (car l) a)
           (cdr l)
           (cons (car l) (rm a (cdr l) oh))))
      (else
        (try oh2
          (cons (rm a (car l) oh2) (cdr l))
          (cons (car l) (rm a (cdr l) oh)))))))


; (try x a B)

; equivalent to

; (let/cc success
;   (let/cc x
;     (success a))
;    B)
;
(define rember1*
  (lambda (a l)
    (try oh (rm a l oh) l)))


(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth* (cdr l)))
      (else (max
              (add1 (depth* (car l)))
              (depth* (cdr l)))))))



; Chapter 15

(define x (cons 'chicago (cons 'pizza '())))

(set! x 'gone)

(define gourmet
  (lambda (food)
    (cons food (cons x '()))))

(define gourmand
  (lambda (food)
    (set! x food)
    (cons food (cons x '()))))


(define dinerR
  (lambda (food)
    (set! x food)
    (cons 'milkshake (cons food '()))))


(define omnivore
  (let ((x 'minestrone))
    (lambda (food)
      (set! x food)
      (cons food (cons x '())))))

(define gobbler
  (let ((x 'minestrone))
    (lambda (food)
      (set! x food)
      (cons food (cons x '())))))

(define food 'none)

(define glutton
  (lambda (x)
    (set! food x)
    (cons 'more (cons x (cons 'more (cons x '()))))))

(define chez-nous
  (lambda ()
    (let ((a food))
      (set! food x)
      (set! x a))))


; Chapter 16

(define sweet-tooth
  (lambda (food)
    (cons food (cons 'cake '()))))

(define last 'angelfood)

(define sweet-toothL
  (lambda (food)
    (set! last food)
    (cons food (cons 'cake '()))))

(define ingredients '())

(define sweet-toothR
  (lambda (food)
    (set! ingredients (cons food ingredients))
    (cons food (cons 'cake '()))))


(define deep
  (lambda (m)
    (cond
      ((zero? m) 'pizza)
      (else (cons (deepM (sub1 m)) '())))))

(define Rs '())
(define Ns '())

(define deepR
  (lambda (n)
    (let ((result (deep n)))
    (set! Rs (cons result Rs))
    (set! Ns (cons n Ns))
    result)))

(define find
  (lambda (n Ns Rs)
    (letrec
      ((A (lambda (ns rs)
          (cond
            ((null? ns) #f)
            ((= (car ns) n) (car rs))
            (else
              (A (cdr ns) (cdr rs)))))))
      (A Ns Rs))))




(define deepM
  (let ((Rs '()) (Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
            (let ((result (deep n)))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists)))))


;(define length
;  (lambda (l)
;    (cond
;      ((null? l) 0)
;      (else (add1 (length (cdr l)))))))


(define len
  (let ((h (lambda (l) 0)))
    (set! h
      (lambda (arg)
        (cond
          ((null? arg) 0)
          (else (add1 (h (cdr arg)))))))
    h))

