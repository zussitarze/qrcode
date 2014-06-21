#lang racket/base

;; For Finite fields GF(2^8)

(require racket/string
         racket/list)

(provide gf+ gf- gf* gf/
         gflog gfexp
         
         gfpoly+
         gfpoly*
         gfpoly-remainder
         gfpoly-degree
         gfpoly-shift

	 gfpoly->string
	 gf->poly-over-gf2-string)

(define primative-poly 285) ; x^8 + x^4 + x^3 + x^2 + 1

(define-values (gflog gfexp)
  (let ([log-tbl (make-vector 256)]
        [exp-tbl (make-vector 511)])
    (vector-set! exp-tbl 0 1)
    (for/fold ([a 2]) ([i (in-range 1 256)])
      (let ([a (if (bitwise-bit-set? a 8)
                   (bitwise-xor a primative-poly)
                   a)])
        (vector-set! log-tbl a i)
        (vector-set! exp-tbl i a)
        (vector-set! exp-tbl (+ i 255) a)
        (arithmetic-shift a 1)))
    (values (λ (x)
               (vector-ref log-tbl x))
            (λ (x)
               (vector-ref exp-tbl x)))))

(define (gf+ x y)
  (bitwise-xor x y))

(define (gf- x y)
  (gf+ x y))

(define (gf* x y)
  (if (or (= 0 x) (= 0 y))
      0
      (gfexp (+ (gflog x) (gflog y)))))

(define (gf/ x y)
  (cond [(= 0 y) (error "Divide by zero")]
        [(= 0 x) 0]
        [else (gfexp (+ (gflog x) (- 255 (gflog y))))]))


;; Polynomials are represented internally as lists, in order of decreasing
;; term degree.

(define (add-lists l1 l2)
 (cond [(null? l1) l2]
       [(null? l2) l1]
       [else (cons (gf+ (car l1) (car l2))
                   (add-lists (cdr l1) (cdr l2)))]))

(define (gfpoly+ a b) 
 (dropf (reverse (add-lists (reverse a) (reverse b))) zero?))
                   
(define (gfpoly-remainder dividend divisor)
  (define (gfpoly+left a b)
    (dropf (add-lists a b) zero?))
  (define p (gfpoly-degree divisor))
  (let div ([dividend dividend])
    (if (< (gfpoly-degree dividend) p)
        dividend
        (div (gfpoly+left (gfpoly-scale divisor (car dividend))
                          dividend)))))

(define (gfpoly* a b)
  (define r (make-vector (+ (gfpoly-degree a) (gfpoly-degree b) 1) 0))
  (for ([ei a] [i (in-naturals)])
    (for ([ej b] [j (in-naturals)])
      (vector-set! r (+ i j) (gf+ (vector-ref r (+ i j))
                                  (gf* ei ej)))))
  (dropf (vector->list r) zero?))

;; multiply each term of the polynomial by x in GF(Q)
(define (gfpoly-scale p x)
  (dropf (map (lambda (t) (gf* t x)) p) zero?))

(define (gfpoly-degree p)
  (- (length p) 1))

(define (gfpoly-shift p n)
  (append p (make-list n 0)))

;; Convert a polynomial to string representation
(define (gfpoly->string p)
  (if (empty? p)
      "0"
      (string-join (for/list ([elem p]
                              [deg (in-range (gfpoly-degree p) -1 -1)]
                              #:unless (= 0 elem))
                     (cond [(= 0 deg) (gf->power-string elem)]
                           [(= 1 deg) (if (= elem 1)
                                          "x"
                                          (format "~ax" (gf->power-string elem)))]
                           [else (if (= elem 1)
                                     (format "x^~a" deg)
                                     (format "~ax^~a" (gf->power-string elem) deg))]))
                   " + ")))

;; Convert a field element to power representation
(define (gf->power-string x)
  (cond [(> x 2) (format "α^~a" (gflog x))]
        [(= x 0) "0"]
        [(= x 1) "1"]
        [(= x 2) "α"]))

;; Convert a field element to string a representation of polynomial over GF(2)
(define (gf->poly-over-gf2-string x)
  (if (= 0 x) 
      "0"
      (let loop ([x x] [i 0] [terms '()])
        (cond 
         [(= x 0) (string-join terms " + ")]
         [else (loop (arithmetic-shift x -1) 
                     (+ i 1)
                     (if (bitwise-bit-set? x 0)
                         (let ([t (cond [(> i 1) (format "x^~s" i)]
                                        [(= 0 i) "1"]
                                        [(= 1 i) "x"])])
                           (cons t terms))
                         terms))]))))
