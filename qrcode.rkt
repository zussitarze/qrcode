#lang racket/base

(require racket/draw
         racket/match
         racket/class
         racket/list
         "bitarray.rkt"
         "coding.rkt"
         "specs.rkt")

(provide main
         qrcode-encode
         qrcode-render)

(define (main out err msg [scale "4"])
  (define qr (qrcode-encode (string->bytes/utf-8 msg) 
                            (string->symbol (string-upcase err))))
  (send (qrcode-render qr (string->number scale)) 
        save-file out 'png))

(define (qrcode-encode msg err #:version [ver 'auto])
  (let* ([ver (if (eq? ver 'auto)
                  (or (bestfit msg err 'byte)
                      (error "Message exceeds QR code capacity"))
                  (if (canhold? msg ver err 'byte)
                      ver
                      (error "Message exceeds version capacity")))]
         [qr (make-bitarray (version->dimension ver))]
         [cws (interleave-ec-codewords 
               (msg-codewords (bytes->list msg) ver err)
               ver err)])
    (place-timing-patterns qr)
    (place-finder-patterns qr)
    (place-alignment-patterns ver qr)
    (place-data-modules ver qr cws)
    (match (car (sort (for/list ([f mask-generators]
                                 [num (in-naturals 0)])
                        (let ([qrc (bitarray-clone qr)])
                          (apply-mask ver qrc f)
                          (list (score-qrcode qrc) num qrc)))
                      <
                      #:key car))
      [(list score mask-num qr)
       (place-format-modules qr (format-information err mask-num))
       (when (>= ver 7) (place-version-patterns ver qr))
       (printf "Ver: ~a Err: ~a~n" ver err)
       qr])))       

(define (qrcode-render qr scale)
  (let* ([dim (bitarray-dimension qr)]
         [bitmap (make-bitmap (* scale (+ 8 dim))
                              (* scale (+ 8 dim))
                              #f)]
         [dc (new bitmap-dc% [bitmap bitmap])])
    (send dc set-scale scale scale)
    (send dc set-smoothing 'unsmoothed)
    (send dc draw-bitmap (make-monochrome-bitmap dim dim (bitarray-data qr)) 4 4)
    bitmap))

(define (msg-codewords msg-bs ver err)
  (let* ([num-codes (spec-msg-codes (lookup-spec ver err))]
         [mode (byte->bits #b0100 4)]
         [count-bits (character-count-bits ver)]
         [chars (byte->bits (length msg-bs) count-bits)]
         [msg-bits (bytes->bits msg-bs)]
         [terminator (byte->bits 0 (min 4 (- (* num-codes 8) 4 count-bits (length msg-bits))))]
         [data (bits->bytes (append mode chars msg-bits terminator))])
    (append data (build-list (- num-codes (length data))
                             (λ (i)                               
                               (if (even? i)
                                   #b11101100
                                   #b00010001))))))

(define (interleave-ec-codewords msgcodes ver err)
  (let* ([s (lookup-spec ver err)]
         [ec1 (spec-ec1 s)]
         [ec2 (spec-ec2 s)]
         [p (/ (spec-ec-codes s) 
               (+ ec1 ec2))]
         [k (/ (- (spec-msg-codes s) ec2)
               (+ ec1 ec2))]
         [gen (reed-solomon-generator p)])
    (let* ([blks (let*-values ([(blks1 restcodes) (chunk-list msgcodes k ec1)]
                               [(blks2 _) (chunk-list restcodes (+ k 1) ec2)]) 
                   (append blks1 blks2))]
           [ec-blks (map (λ (m) (reed-solomon-encode m gen #f)) blks)])
      (append (interleave-lists blks)
              (interleave-lists ec-blks)))))

(define (format-information err mask-num)
  (let* ([errbits (case err
                    [(L) '(0 1)]
                    [(M) '(0 0)]
                    [(Q) '(1 1)]
                    [(H) '(1 0)])]
         [maskbits (map (λ (x) (if x 1 0)) (byte->bits mask-num 3))])
    (map (λ (x) (= 1 x))
         (bch-encode-QR (append errbits maskbits)))))

(define (place-timing-patterns qr)
  (define s (bitarray-dimension qr))
  (for ([i (in-range 8 (- s 8))])
    (bitarray-set qr i 6 (even? i))
    (bitarray-set qr 6 i (even? i))))

(define (place-finder-patterns qr)
  (let ([dim (bitarray-dimension qr)])
    (bitarray-blit qr finder-pattern 0 0)  
    (bitarray-blit qr finder-pattern (- dim 7) 0)
    (bitarray-blit qr finder-pattern 0 (- dim 7))))

(define (place-alignment-patterns ver qr)
  (for ([c (in-list (alignment-coords ver))])
    (bitarray-blit qr alignment-pattern (car c) (cdr c))))

(define (place-version-patterns ver qr)
  (let ([vbits (golay-encode-QR (map (λ (x) (if x 1 0)) (byte->bits ver 6)))]
        [offset (- (bitarray-dimension qr) 11)])
    (for*/fold ([bs vbits])
      ([j (in-range 5 -1 -1)]
       [i (in-range 2 -1 -1)])
      (when (= 1 (car bs))
        (bitarray-set qr j (+ i offset))
        (bitarray-set qr (+ i offset) j))
      (cdr bs))))

(define (place-data-modules ver qr bs)
  (define exclusions (encoding-region-exclusions ver qr))
  (define dim (bitarray-dimension qr))
  (let loop ([bits (bytes->bits bs)] 
             [col (- dim 1)] [right? #t]
             [row (- dim 1)] [dir -1])
    (unless (null? bits)
      (cond 
        [(< row 0) 
         (loop bits (- col 2) right? 0 1)]
        [(>= row dim) 
         (loop bits (- col 2) right? (- dim 1) -1)]
        [(= col 6)
         (loop bits (- col 1) right? row dir)]
        [(< col 0)
         (error "Column out of bounds" col)]
        [else 
         (let* ([x (if (or right? (= col 0)) 
                       col 
                       (- col 1))]
                [rest-bits (if (can-write? x row exclusions)
                               (begin (bitarray-set qr x row (car bits))
                                      (cdr bits))
                               bits)])
           (if (and right? (> col 0))
               (loop rest-bits col #f row dir)
               (loop rest-bits col #t (+ row dir) dir)))]))))

(define (place-format-modules qr bits)
  (define dim (bitarray-dimension qr))
  (define-values (hbs lbs) (split-at bits 7))
  ;; Left corner
  (for ([j (in-list '(0 1 2 3 4 5 7))]
        [b (in-list hbs)])
    (bitarray-set qr j 8 b))
  (for ([i (in-list '(8 7 5 4 3 2 1 0))]
        [b (in-list lbs)])
    (bitarray-set qr 8 i b))
  ;; dark
  (bitarray-set qr 8 (- dim 8) #t)
  ;; Split corners
  (for ([i (in-range 1 9)]
        [b (in-list hbs)])
    (bitarray-set qr 8 (- dim i) b))  
  (for ([j (in-range 8 0 -1)]
        [b (in-list lbs)])
    (bitarray-set qr (- dim j) 8 b)))

(define (apply-mask ver qr maskfn)
  (define d (bitarray-dimension qr))
  (define excs (encoding-region-exclusions ver qr))
  (for* ([i (in-range 0 d)]
         [j (in-range 0 d)])
    (when (and (maskfn i j)
               (can-write? j i excs))
      (bitarray-toggle qr j i))))

;; returns a list of constraints, where each constraint (x1 x2 y1 y2) demarcates
;; an exclusion zone.
(define (encoding-region-exclusions ver qr)
  (let ([d (bitarray-dimension qr)])
    `( ;; Finder patterns + 1 wide separator + format strips.
      (0 8 0 8)
      (,(- d 8) ,(- d 1) 0 8)
      (0 8 ,(- d 8) ,(- d 1))
      ;; Timing patterns
      (0 ,(- d 1) 6 6)
      (6 6 0 ,(- d 1))
      ;; Version information
      ,@(if (< ver 7)
            '()
            `((0 5 ,(- d 11) ,(- d 9))
              (,(- d 11) ,(- d 9) 0 5)))
      ;; Alignment patterns
      ,@(for/list ([c (in-list (alignment-coords ver))])
          (let ([x (car c)]
                [y (cdr c)])
            (list x (+ x 4) y (+ y 4)))))))

(define (can-write? x y excs)
  (for/and ([exc (in-list excs)])
    (let ([cx1 (car exc)] [cx2 (cadr exc)]
	  [cy1 (caddr exc)] [cy2 (cadddr exc)])
      (or (< x cx1) (> x cx2)
	  (< y cy1) (> y cy2)))))

(define (score-qrcode qr)
  (define dim (bitarray-dimension qr))
  (define (count-adj fetch)
    (let scan ([i 0] [acc 0] [prev #f] [counts '()])
      (if (>= i dim)
          (cons acc counts)
          (let ([cur (fetch i)])
            (if (eq? cur prev)
                (scan (+ i 1) (+ acc 1) cur counts)
                (scan (+ i 1) 0 cur (cons acc counts)))))))
  (define (seek-pattern bit-list)
    (let ([pat1 '(#t #f #t #t #t #f #t #f #f #f #f)]
          [pat2 '(#f #f #f #f #t #f #t #t #t #f #t)])
      (if (or (contains-sublist? bit-list pat1)
              (contains-sublist? bit-list pat2))
          1
          0)))
  (let ([adjs (for/sum ([z (in-range dim)])
                (foldl (λ (c acc)
			  (if (> c 5)
			      (+ acc (- 5 c) 3)
			      acc))  
                       0
                       (append (count-adj (λ (col) 
					     (bitarray-get qr col z)))
                               (count-adj (λ (row) 
					     (bitarray-get qr z row))))))]
        [blocks (for*/sum ([i (in-range (- dim 1))]
                           [j (in-range (- dim 1))])
                  (if (same? (bitarray-get qr j i)
                             (bitarray-get qr (+ j 1) i)
                             (bitarray-get qr j (+ i 1)) 
                             (bitarray-get qr (+ j 1) (+ i 1)))
                      3
                      0))]
        [pattern (for/sum ([z (in-range dim)])
                   (* 40 (+ (seek-pattern (for/list ([j (in-range dim)])
                                            (bitarray-get qr j z)))
                            (seek-pattern (for/list ([i (in-range dim)])
                                            (bitarray-get qr z i))))))]
        [deviation (let* ([blacks (for*/sum ([i (in-range dim)]
                                             [j (in-range dim)])
                                    (if (bitarray-get qr j i) 1 0))]
                          [pcnt (* 100 (/ blacks (* dim dim)))])
                     (* 10 (abs (round (/ (- pcnt 50) 5)))))])
    (+ adjs blocks pattern deviation)))


(define (prefix? l p)
  (or (null? p)
      (and (not (null? l))
           (eq? (car l) (car p))
           (prefix? (cdr l) (cdr p)))))

(define (contains-sublist? l s)
  (and (not (null? l))
       (or (prefix? l s)
           (contains-sublist? (cdr l) s))))

(define (same? x . xs)
  (andmap (λ (y) 
            (eq? x y))
          xs))  

(define (chunk-list lst size count)
  (let loop ([lst lst] [count count] [acc '()]) 
    (if (or (null? lst) (= count 0)) 
        (values (reverse acc) lst)
        (let-values ([(c r) (split-at lst size)])
          (loop r (- count 1) (cons c acc))))))

(define (interleave-lists lol)
  (let ([lol (filter (λ (l) (not (null? l))) lol)])
    (if (null? lol)
        '()
        (append (map car lol)
                (interleave-lists (map cdr lol))))))

(define (byte->bits b [n 8])
  (let loop ([b b] [n n] [acc '()])
    (if (= n 0)
        acc
        (loop (arithmetic-shift b -1)
              (- n 1)
              (cons (bitwise-bit-set? b 0) acc)))))

(define (bytes->bits bs)
  (append-map (λ (b) (byte->bits b 8)) bs))

;; Produces a list of bytes from given bit list, where leftmost bits are 
;; taken as most significant.
(define (bits->bytes bits)
  (let loop ([bits bits] [pos 7] [b 0] [bs '()])
    (cond [(< pos 0) (loop bits 7 0 (cons b bs))]
          [(null? bits) (reverse (if (> b 0)
                                     (cons b bs)
                                     bs))]
          [else (loop (cdr bits)
                      (- pos 1)
                      (if (car bits) 
                          (bitwise-ior b (arithmetic-shift 1 pos))
                          b)
                      bs)])))

(define finder-pattern 
  (list #b01111111
        #b01000001
        #b01011101
        #b01011101
        #b01011101
        #b01000001
        #b01111111))

(define alignment-pattern 
  (list #b00011111
        #b00010001
        #b00010101
        #b00010001
        #b00011111))

(define mask-generators 
  (list (λ (i j) (= 0 (modulo (+ i j) 2)))
        (λ (i j) (= 0 (modulo i 2)))
        (λ (i j) (= 0 (modulo j 3)))
        (λ (i j) (= 0 (modulo (+ i j) 3)))
        (λ (i j) (= 0 (modulo (+ (quotient i 2) (quotient j 3)) 2)))
        (λ (i j) (= 0 (+ (modulo (* i j) 2) (modulo (* i j) 3))))
        (λ (i j) (= 0 (modulo (+ (modulo (* i j) 2) (modulo (* i j) 3)) 2)))
        (λ (i j) (= 0 (modulo (+ (modulo (+ i j) 2) (modulo (* i j) 3)) 2)))))
