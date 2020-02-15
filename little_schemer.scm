


(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


; is list of atoms
(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))


; is 'a member of lat
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
              (member? a (cdr lat)))))))

; remove first instance of a in lat
(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))


; return list of first s-exprs of each sublist of l
(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (firsts (cdr l)))))))


(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))


(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))


; replace first occurance of old with new
(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))


; replace first occurance of either o1 or o2 with new
(define subst2
  (lambda (new o1 o2 lat)
    (cond
      ((null? lat) '())
      ((or (eq? (car lat) o1) (eq? (car lat) o2)) (cons new (cdr lat)))
      (else (cons (car lat) (subst2 new o1 o2 (cdr lat)))))))



(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) a) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))



(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons old (cons new (multiinsertR? new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR? new old (cdr lat)))))))


(define multiinsertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (cons old (multiinsertL new old (cdr lat)))))
      (else (cons (car lat) (multiinsertL new old (cdr lat)))))))


(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))





; define basic math on the natural numbers [0, inf) based on add1 and sub1


(define add1
  (lambda (n)
    (+ n 1)))

(define sub1
  (lambda (n)
    (- n 1)))


; define our on '+' using add1 and sub1 (only defined for m and n >= 0
(define o+
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (add1 (o+ n (sub1 m)))))))


; '-' using sub1, again only defined for n and m >= 0 and n >= m
(define o-
  (lambda (n m)
    (cond
      ((zero? m) n)
      (else (o- (sub1 n) (sub1 m))))))


; books version ... I get it and it's easier to visualize recursively
; (sub1 (sub1 (sub1 ... m times ...(sub1 n))))
; but the above is what I wrote before I looked at the answer and it also
; works and makes sense
;(define o-
;  (lambda (n m)
;    (cond
;      ((zero? m) n)
;      (else (sub1 (o- n (sub1 m))))))


(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (o+ (car tup) (addtup (cdr tup)))))))



(define o*
  (lambda (n m)
    (cond
      ((zero? m) 0)
      (else (o+ n (o* n (sub1 m)))))))


(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (o+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))



(define greater?
  (lambda (n m)
    (cond
      ;((zero? m) (not (zero? n)))   why does scheme not allow this?
      ((zero? n) #f)
      ((zero? m) #t)
      (else (greater? (sub1 n) (sub1 m))))))


(define less?
  (lambda (n m)
    (cond
      ((zero? m) #f)
      ((zero? n) #t)
      (else (less? (sub1 n) (sub1 m))))))

; equal? is part of scheme already... hmm
(define equal?
  (lambda (n m)
    (cond
      ((zero? m) (zero? n))
      ((zero? n) #f)
      (else (equal? (sub1 n) (sub1 m))))))

; these types of comments only in R6RS
; also #; comments a full s-expression so could do
; #; (define blah
; and all of blah definition is commented
#|
(define equal2?
  (lambda (n m)
    (cond
      ((greater? n m) #f)
      ((less? n m) #f)
      (else #t))))

|#


(define higher
  (lambda (n m)
    (cond
      ((greater? n m) n)
      (else m))))

(define lower
  (lambda (n m)
    (cond
      ((less? n m) n)
      (else m))))

(define pow
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (o* n (pow n (sub1 m)))))))

(define div
  (lambda (n m)
    (cond
      ((less? n m) 0)
      (else (add1 (div (o- n m) m))))))



(define len
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (len (cdr l)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; return nth s-expr in l (1 based indexing)
(define pick
  (lambda (n l)
    (cond
      ((zero? (sub1 n)) (car l))
      (else (pick (sub1 n) (cdr l))))))


; same as above but 0 based indexing
;(define pick
;  (lambda (n l)
;    (cond
;      ((zero? n) (car l))
;      (else (pick (sub1 n) (cdr l))))))


; remove nth s-expr from l (0 based)
(define rempick
  (lambda (n l)
    (cond
      ((null? l) '())
      ((zero? n) (cdr l))
      (else (cons (car l) (rempick (sub1 n) (cdr l)))))))



(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))


(define all-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((not (number? (car lat))) (all-nums (cdr lat)))
      (else (cons (car lat) (all-nums (cdr lat)))))))


(define eqan?
  (lambda (a b)
    (cond
      ((and (number? a) (number? b)) (= a b))
      ((or (number? a) (number? b)) #f)
      (else (eq? a b)))))


(define occur
  (lambda (a lat)
    (cond
      ((null? lat) 0)
      ((eqan? (car lat) a) (add1 (occur a (cdr lat))))
      (else (occur a (cdr lat))))))


(define one?
  (lambda (n)
    (= n 1)))


; 1 based
(define rempick
  (lambda (n l)
    (cond
      ((null? l) '())
      ((one? n) (cdr l))
      (else (cons (car l) (rempick (sub1 n) (cdr l)))))))


; removes all matching s-exprs s from l

(define rember*
  (lambda (s l)
    (cond
      ((null? l) '())
      ((equal? (car l) s) (rember* s (cdr l)))
      (else (cons (car l) (rember* s (cdr l)))))))


(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons old (cons new (cdr l))))
         (else (cons (car l) (insertR* new old (cdr l))))))
      (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))



(define occur*
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((atom? (car l))
       (cond
         ((eq? (car l) a) (add1 (occur* a (cdr l))))
         (else (occur* a (cdr l)))))
      (else (o+ (occur* a (car l)) (occur* a (cdr l)))))))


(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons new (subst* new old (cdr l))))
         (else (cons (car l) (subst* new old (cdr l))))))
      (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))



(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) old) (cons new (cons old (insertL* new old (cdr l)))))
         (else (cons (car l) (insertL* new old (cdr l))))))
      (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))


(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((atom? (car l))
       (or (eq? (car l) a) (member* a (cdr l))))
      (else (or (member* a (car l)) (member* a (cdr l)))))))


(define leftmost
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))


(define eqlist?
  (lambda (l1 l2)
    (cond
      ((null? l1) (null? l2))
      ((null? l2) #f)
      ((atom? (car l1))
       (and (eqan? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2))))
      ((atom? (car l2)) #f)
      (else (and (eqlist? (car l1) (car l2)) (eqlist? (cdr l1) (cdr l2)))))))


(define equal?
  (lambda (s1 s2)
    (cond
      ((and (atom? s1) (atom? s2)) (eqan? s1 s2))
      ((or (atom? s1) (atom? s2)) #f)
      (else (eqlist? s1 s2)))))


(define eqlist?
  (lambda (l1 l2)
    (cond
      ((null? l1) (null? l2))
      ((null? l2) #f)
      (else
        (and
          (equal? (car l1) (car l2))
          (eqlist? (cdr l1) (cdr l2)))))))


(define numbered?
  (lambda (aexp)
    (cond
      ((atom? aexp) (number? aexp))
      (else
        (and (numbered? (car aexp))
             (numbered? (car (cdr (cdr aexp)))))))))


; return value of a numbered arithmetic expr

; change these 3 to change between pre/post/infix representations
(define operand1
  (lambda (aexp)
    (cadr aexp)))

(define operand2
  (lambda (aexp)
    (caddr aexp)))

(define operator
  (lambda (aexp)
    (car aexp)))

(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      ((eq? (operator nexp) 'o+)
       (o+ (value (operand1 nexp)) (value (operand2 nexp))))
      ((eq? (operator nexp) 'o*)
       (o* (value (operand1 nexp)) (value (operand2 nexp))))
      (else
       (pow (value (operand1 nexp)) (value (operand2 nexp)))))))



; alternative math primitives
; '() = 0 '(()) = 1 '(() ()) = 2 etc.

(define sero?
  (lambda (n)
    (null? n)))

(define edd1
  (lambda (n)
    (cons '() n)))

(define zub1
  (lambda (n)
    (cdr n)))

(define plus
  (lambda (n m)
    (cond
      ((sero? m) n)
      (elso (edd1 (plus n (zub1 m)))))))



(define set?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((member? (car l) (cdr l)) #f)
      (else (set? (cdr l))))))


; Came up with this without looking at the book
; more efficient than recursing till (null? l)
; when you don't have to
(define makeset
  (lambda (l)
    (cond
      ((set? l) l)
      (else (cons (car l) (makeset (multirember (car l) (cdr l))))))))


(define subset?
  (lambda (s1 s2)
    (cond
      ((null? s1) #t)
      (else
        (and (member? (car s1) s2) (subset? (cdr s1) s2))))))


(define eqset?
  (lambda (s1 s2)
    ((and (subset? s1 s2) (subset? s2 s1)))))


(define intersect?
  (lambda (s1 s2)
    (cond
      ((null? s1) #f)
      (else (or (member? (car s1) s2) (intersect? (cdr s1) s2))))))


(define intersect
  (lambda (s1 s2)
    (cond
      ((null? s1) '())
      ((member? (car s1) s2)
       (cons (car s1) (intersect (cdr s1) s2)))
      (else (intersect (cdr s1) s2)))))



; note both of these only work if input lists are valid sets

; what I came up with
(define union
  (lambda (s1 s2)
    (cond 
      ((null? s1) s2)
      ((null? s2) s1)
      (else (cons (car s1) (union (cdr s1) (rember (car s1) s2)))))))

; book's version, probably better
(define union
  (lambda (s1 s2)
    (cond
      ((null? s1) s2)
      ((member? (car s1) s2) (union (cdr s1) s2))
      (else (cons (car s1) (union (cdr s1) (multirember (car s1) s2)))))))


(define difference
  (lambda (s1 s2)
    (cond
      ((null? s1) '())
      ((member? (car s1) s2) (difference (cdr s1) s2))
      (else (cons (car s1) (difference (cdr s1) s2))))))


(define intersectall
  (lambda (l-set)
    (cond
      ((null? (cdr l-set)) (car l-set))
      (else (intersect (car l-set) (intersectall (cdr l-set)))))))


(define a-pair?
  (lambda (x)
    (and (list? x) (eq? (len x) 2))))

(define a-pair?
  (lambda (x)
    (cond
      ((atom? x) #f)
      ((null? x) #f)
      ((null? (cdr x)) #f)
      ((null? (cddr x)) #t)
      (else #f))))


(define first
  (lambda (p)
    (car p)))

(define second
  (lambda (p)
    (cadr p)))

(define third
  (lambda (p)
    (caddr p)))

(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))


; is a function (set of pairs (x, y) all x are unique)
; same definition from algebra, no overlap in domain,
; vertical line test
(define fun?
  (lambda (rel)
    (set? (firsts rel))))


(define revpair
  (lambda (pair)
    (build (second pair) (first pair))))


(define revrel
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons
              (revpair (car rel))
              (revrel (cdr rel)))))))


; assumes fun is a function ie (fun? fun) -> #t
(define one-to-one?
  (lambda (fun)
    (fun? (revrel fun))))



(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) '())
      ((test? (car l) a) (cdr l))
      (else (cons (car l) (rember-f test? a (cdr l)))))))



(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(define seqS
  (lambda (new old l)
    (cons new l)))

(define seqrem
  (lambda (new old l) l))

(define insert-g
  (lambda (seq)
    (lambda (new old l)
      (cond
        ((null? l) '())
        ((eq? (car l) old) (seq new old (cdr l)))
        (else (cons (car l) ((insert-g seq) new old (cdr l))))))))


(define insertR (insert-g seqR))

(define insertL (insert-g seqL))

(define subst (insert-g seqS))

(define rember
  (lambda (a l)
    ((insert-g seqrem) #f a l)))


(define atom-to-function
  (lambda (x)
    (cond
      ((eq? x 'o+) o+)
      ((eq? x 'o*) o*)
      (else pow))))


(define value
  (lambda (nexp)
    (cond
      ((atom? nexp) nexp)
      (else
        ((atom-to-function (operator nexp))
         (value (operand1)) (value (operand2)))))))



(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
        ((null? lat) '())
        ((test? (car lat) a)
         ((multirember-f test?) a (cdr lat)))
        (else (cons (car lat) ((multirember-f test?) a (cdr lat))))))))


(define multirember-eq
  (multirember-f eq?))

(define print-hello
  (lambda (n)
    (cond
      ((= n 0) (printf "\n"))
      (else (and (printf "hello\n") (print-hello (- n 1)))))))


(define keep-looking
  (lambda (a next lat)
    (cond
      ((eq? next a) #t)
      ((not (integer? next)) #f)
      (else (keep-looking a (pick next lat) lat)))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))


(define shift
  (lambda (pair)
    (build (first (first pair))
           (build (second (first pair))
                  (second pair)))))


(define align
  (lambda (pora)
    (cond
      ((atom? pora) pora)
      ((a-pair? (first pora))
       (align (shift pora)))
      (else (build (first pora)
                   (align (second pora)))))))


(define collatz
  (lambda (n)
    (print n)
    (display " ")
    (cond
      ((one? n) 1)
      ((even? n) (collatz (/ n 2)))
      (else (collatz (add1 (* 3 n)))))))


(define ackermann
  (lambda (n m)
    (cond
      ((zero? n) (add1 m))
      ((zero? m) (ackermann (sub1 n) 1))
      (else (ackermann (sub1 n)
                       (ackermann n (sub1 m)))))))







