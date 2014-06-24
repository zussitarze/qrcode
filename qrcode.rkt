#lang racket/base

(require racket/draw
         racket/match
         racket/class
         racket/list
         racket/contract
         "bitarray.rkt"
         "coding.rkt")

(module+ test
  (require rackunit))

(define error-level/c (one-of/c 'L 'M 'Q 'H))
(define version/c (integer-in 1 40))
(define mode/c (one-of/c 'numeric 'alphanumeric 'byte))

(provide 
 (struct-out configuration)
 (contract-out 
  [make-qrcode (->* (string? error-level/c)
                    (#:scale exact-positive-integer?
		     #:filename string?)
                    (or/c (is-a?/c bitmap%) boolean?))]
  [qrcode-encode (-> string? error-level/c 
                     (values bitarray? configuration?))]
  [qrcode-render (-> bitarray? exact-positive-integer? 
                     (is-a?/c bitmap%))]))

(module+ main
  (require racket/cmdline)
  (require racket/string)
  (define error-level (make-parameter 'M))
  (define scale (make-parameter 4))  
  (command-line 
   #:program "qrcode"
   #:once-each 
   [("-l" "--errorlevel") 
    lvl 
    "Error correction level: (L)ow, (M)edium, (Q)uartile, (H)igh (default M)"
    (error-level (string->symbol (string-upcase lvl)))]
   [("-s" "--scale") 
    s
    "Pixel scale factor (default 4)"
    (scale (string->number s))]    
   #:args (filename . message)
   (let*-values ([(message) (string-join message)]
                 [(qr config) (qrcode-encode message (error-level))])
     (send (qrcode-render qr (scale)) save-file filename 'png)
     config)))

(define (make-qrcode msg err #:scale [scale 4] #:filename [filename #f])
  (define-values (qr _) (qrcode-encode msg err))
  (define bitmap (qrcode-render qr scale))
  (if filename
      (send bitmap save-file filename 'png)
      bitmap))

(define (qrcode-encode msg err)
  (let* ([config (make-configuration msg err)]
         [ver (configuration-version config)]
         [qr (make-bitarray (version->dimension ver))]
         [mcws (msg-codewords msg config)]
         [ecws (interleave-ec-codewords mcws config)])
    (place-timing-patterns qr)
    (place-finder-patterns qr)
    (place-alignment-patterns ver qr)
    (place-data-modules ver qr ecws)
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
       (values qr config)])))       

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

(struct configuration (version error mode) #:transparent)

(define (make-configuration msg err)
  (let* ([mode (cond [(regexp-match-exact? #rx"[0-9]+" msg) 'numeric]
                     [(regexp-match-exact? #rx"[0-9A-Z $%*+-./:]+" msg) 'alphanumeric]		    
                     [else 'byte])]
         [ver (fit-version (string-length msg) err mode)])
    (if ver
        (configuration ver err mode)
        (raise-user-error 
         'Error "Data length ~a exceeds QR code capacity. (mode: ~a) (error-level: ~a)"
         (string-length msg) mode err))))

(define (encode-message msg mode)
  (case mode
    [(numeric) 
     (let*-values 
         ([(q r) (quotient/remainder (string-length msg) 3)]
          [(trips rest) (chunk-list (string->list msg) 3 q)]
          [(f) (λ (b) 
                 (λ (l) (number->bits (string->number (list->string l)) b)))])
       (append (append-map (f 10) trips)
               (case r
                 [(0) '()]
                 [(1) ((f 4) rest)]
                 [(2) ((f 7) rest)])))]
    [(alphanumeric) 
     (let*-values 
         ([(q r) (quotient/remainder (string-length msg) 2)]
          [(pairs lone) (chunk-list (string->list msg) 2 q)]
          [(code) (λ (x) (hash-ref alphanumeric-tbl x))])
       (append (append-map 
                (λ (l) 
                  (number->bits (+ (* 45 (code (car l))) (code (cadr l))) 11))
                pairs)
               (if (= r 1) 
                   (number->bits (code (car lone)) 6)
                   '())))]
    [(byte) 
     (bytes->bits (bytes->list (string->bytes/latin-1 msg)))]))

(module+ test
  (check-equal? (encode-message "01234567" 'numeric)
                '(#f #f #f #f #f #f #t #t #f #f #f #t #f #t #f #t #t #f #f #t #t #f #f #f #f #t #t))
  (check-equal? (encode-message "AC-42" 'alphanumeric)
                '(#f #f #t #t #t #f #f #t #t #t #f #t #t #t #f #f #t #t #t #f #f #t #f #f #f #f #t #f)))

(define (msg-codewords msg config)
  (match-let* ([(configuration ver err mode) config]
               [ncodes (spec-msg-codes (lookup-spec ver err))]
               [data-bits (append (number->bits (case mode
                                                  [(numeric)      #b0001]
                                                  [(alphanumeric) #b0010]
                                                  [(byte)         #b0100])
                                                4)
                                  (number->bits (string-length msg) (character-count-bits ver mode))
                                  (encode-message msg mode)
                                  (number->bits 0 4))]
               [data (bits->bytes data-bits)]
               [data (take data (min ncodes (length data)))])
    (append data
            (build-list (- ncodes (length data))
                        (λ (i)                               
                          (if (even? i)
                              #b11101100
                              #b00010001))))))


(module+ test
  (check-equal? (msg-codewords "01234567" (configuration 1 'M 'numeric))
                '(#b00010000 #b00100000 #b00001100 #b01010110 #b01100001 #b10000000 #b11101100 #b00010001 #b11101100
		 #b00010001 #b11101100 #b00010001 #b11101100 #b00010001 #b11101100 #b00010001)))

(define (character-count-bits ver mode)
  (cdr (assq mode (cond 
                    [(<= ver 9)  '((numeric . 10) (alphanumeric . 9)  (byte . 8)  (kanji . 8))]
                    [(<= ver 26) '((numeric . 12) (alphanumeric . 11) (byte . 16) (kanji . 10))]
                    [(<= ver 40) '((numeric . 14) (alphanumeric . 13) (byte . 16) (kanji . 12))]))))

(define (fit-version len err mode)
  (let ([sel (case mode 
               [(byte) spec-maxbyte]
               [(alphanumeric) spec-maxalphanumeric]
               [(numeric) spec-maxnumeric])]) 
    (for/or ([v (in-range 1 41)])
      (and (>= (sel (lookup-spec v err)) len)
           v))))

(module+ test
  (check-eqv? (fit-version 3 'L 'byte) 1)
  (check-eqv? (fit-version 1024 'Q 'byte) 31)
  (check-eqv? (fit-version 2954 'L 'byte) #f))

(define (interleave-ec-codewords msgcodes config)
  (let* ([s (lookup-spec (configuration-version config) (configuration-error config))]
         [ec1 (spec-ec1 s)]
         [ec2 (spec-ec2 s)]
         [p (/ (spec-ec-codes s) 
               (+ ec1 ec2))]
         [k (/ (- (spec-msg-codes s) ec2)
               (+ ec1 ec2))]
         [gen (reed-solomon-generator p)]
         [blks (let*-values ([(blks1 restcodes) (chunk-list msgcodes k ec1)]
                             [(blks2 _) (chunk-list restcodes (+ k 1) ec2)])
                 (append blks1 blks2))]
         [ec-blks (map (λ (m) (reed-solomon-encode m gen #f)) blks)])
    (append (interleave-lists blks)
            (interleave-lists ec-blks))))

(module+ test
  (let ([cfg (configuration 1 'M 'numeric)])
    (check-equal? (interleave-ec-codewords (msg-codewords "01234567" cfg) cfg)
                  '(#b00010000 #b00100000 #b00001100 #b01010110 #b01100001 #b10000000 #b11101100 #b00010001 #b11101100 
		    #b00010001 #b11101100 #b00010001 #b11101100 #b00010001 #b11101100 #b00010001 #b10100101 #b00100100 
                    #b11010100 #b11000001 #b11101101 #b00110110 #b11000111 #b10000111 #b00101100 #b01010101))))

(define (format-information err mask-num)
  (let* ([errbits (case err
                    [(L) '(0 1)]
                    [(M) '(0 0)]
                    [(Q) '(1 1)]
                    [(H) '(1 0)])]
         [maskbits (map (λ (x) (if x 1 0)) (number->bits mask-num 3))])
    (map (λ (x) (= 1 x))
         (bch-encode-QR (append errbits maskbits)))))

(module+ test
  (check-equal? (format-information 'M #b010)
                '(#t #f #t #t #t #t #f #f #t #t #t #t #t #f #f)))

(define (place-timing-patterns qr)
  (define s (bitarray-dimension qr))
  (for ([i (in-range 8 (- s 8))])
    (bitarray-set! qr i 6 (even? i))
    (bitarray-set! qr 6 i (even? i))))

(define (place-finder-patterns qr)
  (let ([dim (bitarray-dimension qr)])
    (bitarray-blit qr finder-pattern 0 0)  
    (bitarray-blit qr finder-pattern (- dim 7) 0)
    (bitarray-blit qr finder-pattern 0 (- dim 7))))

(define (place-alignment-patterns ver qr)
  (for ([c (in-list (alignment-coords ver))])
    (bitarray-blit qr alignment-pattern (car c) (cdr c))))

(define (place-version-patterns ver qr)
  (let ([vbits (golay-encode-QR (map (λ (x) (if x 1 0)) (number->bits ver 6)))]
        [offset (- (bitarray-dimension qr) 11)])
    (for*/fold ([bs vbits])
      ([j (in-range 5 -1 -1)]
       [i (in-range 2 -1 -1)])
      (when (= 1 (car bs))
        (bitarray-set! qr j (+ i offset))
        (bitarray-set! qr (+ i offset) j))
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
                               (begin (bitarray-set! qr x row (car bits))
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
    (bitarray-set! qr j 8 b))
  (for ([i (in-list '(8 7 5 4 3 2 1 0))]
        [b (in-list lbs)])
    (bitarray-set! qr 8 i b))
  ;; dark
  (bitarray-set! qr 8 (- dim 8) #t)
  ;; Split corners
  (for ([i (in-range 1 9)]
        [b (in-list hbs)])
    (bitarray-set! qr 8 (- dim i) b))  
  (for ([j (in-range 8 0 -1)]
        [b (in-list lbs)])
    (bitarray-set! qr (- dim j) 8 b)))

(define (apply-mask ver qr maskfn)
  (define d (bitarray-dimension qr))
  (define excs (encoding-region-exclusions ver qr))
  (for* ([i (in-range 0 d)]
         [j (in-range 0 d)])
    (when (and (maskfn i j)
               (can-write? j i excs))
      (bitarray-toggle! qr j i))))

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

(define (number->bits b [n 8])
  (let loop ([b b] [n n] [acc '()])
    (if (= n 0)
        acc
        (loop (arithmetic-shift b -1)
              (- n 1)
              (cons (bitwise-bit-set? b 0) acc)))))

(define (bytes->bits bs)
  (append-map (λ (b) (number->bits b 8)) bs))

;; Produces a list of bytes from given bit list, where leftmost bits are 
;; taken as most significant.
(define (bits->bytes bits)
  (let loop ([bits bits] [pos 7] [b 0] [bs '()])
    (cond [(null? bits) (reverse (cons b bs))]
          [(< pos 0) (loop bits 7 0 (cons b bs))]
          [else (loop (cdr bits)
                      (- pos 1)
                      (if (car bits) 
                          (bitwise-ior b (arithmetic-shift 1 pos))
                          b)
                      bs)])))

(module+ test
  (check-equal? (bits->bytes '(#t #t #t #t #t #t #t #t)) '(255))
  (check-equal? (bits->bytes '(#t #t #t #t #t #t #t #t #f)) '(255 0)))

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

(define (version->dimension v)
  (+ 17 (* 4 v)))

(define (lookup-spec ver err)
  (apply spec (cdr (assq err (vector-ref spec-tbl (- ver 1))))))

(define (alignment-coords v)
  (if (= v 1)
      '()
      (let* ([rcs (vector-ref alignment-tbl (- v 1))]
             [f (car rcs)]
             [l (last rcs)])
        (for*/list ([i (in-list rcs)]
                    [j (in-list rcs)]
                    #:unless (or (and (= i f) (= j f))
                                 (and (= i f) (= j l))
                                 (and (= j f) (= i l))))
          (cons (- j 2) (- i 2))))))

(define alphanumeric-tbl 
  #hasheq((#\0 . 0)  (#\1 . 1)  (#\2 . 2)  (#\3 . 3)  (#\4 . 4)      (#\5 . 5)  (#\6 . 6)  (#\7 . 7)  
          (#\8 . 8)  (#\9 . 9)  (#\A . 10) (#\B . 11) (#\C . 12)     (#\D . 13) (#\E . 14) (#\F . 15) 
          (#\G . 16) (#\H . 17) (#\I . 18) (#\J . 19) (#\K . 20)     (#\L . 21) (#\M . 22) (#\N . 23) 
          (#\O . 24) (#\P . 25) (#\Q . 26) (#\R . 27) (#\S . 28)     (#\T . 29) (#\U . 30) (#\V . 31) 
          (#\W . 32) (#\X . 33) (#\Y . 34) (#\Z . 35) (#\space . 36) (#\$ . 37) (#\% . 38) (#\* . 39) 
          (#\+ . 40) (#\- . 41) (#\. . 42) (#\/ . 43) (#\: . 44)))

(struct spec (msg-codes ec-codes ec1 ec2 maxnumeric maxalphanumeric maxbyte maxjanji))
(define spec-tbl
  ;;       msg-codes    ec-codes        ec1     ec2     num     alph    bytes   kanji  
  #(((L . (19		7               1	0  	41      25      17      10))
     (M . (16   	10		1	0  	34      20      14      8))
     (Q . (13   	13		1	0  	27      16      11      7))
     (H . (9    	17		1	0  	17      10      7       4)))
    ((L . (34   	10		1	0  	77      47      32      20))
     (M . (28   	16		1	0  	63      38      26      16))
     (Q . (22   	22		1	0  	48      29      20      12))
     (H . (16   	28		1	0  	34      20      14      8)))
    ((L . (55   	15		1	0  	127     77      53      32))
     (M . (44   	26		1	0  	101     61      42      26))
     (Q . (34   	36		2	0  	77      47      32      20))
     (H . (26   	44		2	0  	58      35      24      15)))
    ((L . (80   	20		1	0  	187     114     78      48))
     (M . (64   	36		2	0  	149     90      62      38))
     (Q . (48   	52		2	0  	111     67      46      28))
     (H . (36   	64		4	0  	82      50      34      21)))
    ((L . (108   	26		1	0  	255     154     106     65))
     (M . (86   	48		2	0  	202     122     84      52))
     (Q . (62   	72		2	2  	144     87      60      37))
     (H . (46   	88		2	2  	106     64      44      27)))
    ((L . (136  	36		2	0  	322     195     134     82))
     (M . (108   	64		4	0  	255     154     106     65))
     (Q . (76   	96		4	0  	178     108     74      45))
     (H . (60   	112		4	0  	139     84      58      36)))
    ((L . (156  	40		2	0  	370     224     154     95))
     (M . (124   	72		4	0  	293     178     122     75))
     (Q . (88   	108		2	4  	207     125     86      53))
     (H . (66   	130		4	1  	154     93      64      39)))
    ((L . (194  	48		2	0  	461     279     192     118))
     (M . (154  	88		2	2  	365     221     152     93))
     (Q . (110   	132		4	2  	259     157     108     66))
     (H . (86   	156		4	2  	202     122     84      52)))
    ((L . (232  	60		2	0  	552     335     230     141))
     (M . (182  	110		3	2  	432     262     180     111))
     (Q . (132  	160		4	4  	312     189     130     80))
     (H . (100   	192		4	4  	235     143     98      60)))
    ((L . (274  	72		2	2  	652     395     271     167))
     (M . (216  	130		4	1  	513     311     213     131))
     (Q . (154  	192		6	2  	364     221     151     93))
     (H . (122   	224		6	2  	288     174     119     74)))
    ((L . (324  	80		4	0  	772     468     321     198))
     (M . (254  	150		1	4  	604     366     251     155))
     (Q . (180  	224		4	4  	427     259     177     109))
     (H . (140  	264		3	8  	331     200     137     85)))
    ((L . (370  	96		2	2  	883     535     367     226))
     (M . (290  	176		6	2  	691     419     287     177))
     (Q . (206  	260		4	6  	489     296     203     125))
     (H . (158  	308		7	4  	374     227     155     96)))
    ((L . (428  	104		4	0  	1022    619     425     262))
     (M . (334  	198		8	1  	796     483     331     204))
     (Q . (244  	288		8	4  	580     352     241     149))
     (H . (180  	352		12	4  	427     259     177     109)))
    ((L . (461  	120		3	1  	1101    667     458     282))
     (M . (365  	216		4	5  	871     528     362     223))
     (Q . (261  	320		11	5  	621     376     258     159))
     (H . (197  	384		11	5  	468     283     194     120)))
    ((L . (523  	132		5	1  	1250    758     520     320))
     (M . (415  	240		5	5  	991     600     412     254))
     (Q . (295  	360		5	7  	703     426     292     180))
     (H . (223  	432		11	7  	530     321     220     136)))
    ((L . (589  	144		5	1  	1408    854     586     361))
     (M . (453  	280		7	3  	1082    656     450     277))
     (Q . (325  	408		15	2  	775     470     322     198))
     (H . (253  	480		3	13 	602     365     250     154)))
    ((L . (647  	168		1	5  	1548    938     644     397))
     (M . (507  	308		10	1  	1212    734     504     310))
     (Q . (367  	448		1	15 	876     531     364     224))
     (H . (283  	532		2	17 	674     408     280     173)))
    ((L . (721  	180		5	1  	1725    1046    718     442))
     (M . (563  	338		9	4  	1346    816     560     345))
     (Q . (397  	504		17	1  	948     574     394     243))
     (H . (313  	588		2	19 	746     452     310     191)))
    ((L . (795  	196		3	4  	1903    1153    792     488))
     (M . (627  	364		3	11 	1500    909     624     384))
     (Q . (445  	546		17	4  	1063    644     442     272))
     (H . (341  	650		9	16 	813     493     338     208)))
    ((L . (861  	224		3	5  	2061    1249    858     528))
     (M . (669  	416		3	13 	1600    970     666     410))
     (Q . (485  	600		15	5  	1159    702     482     297))
     (H . (385  	700		15	10 	919     557     382     235)))
    ((L . (932  	224		4	4  	2232    1352    929     572))
     (M . (714  	442		17	0  	1708    1035    711     438))
     (Q . (512  	644		17	6  	1224    742     509     314))
     (H . (406  	750		19	6  	969     587     403     248)))
    ((L . (1006  	252		2	7  	2409    1460    1003    618))
     (M . (782  	476		17	0  	1872    1134    779     480))
     (Q . (568  	690		7	16 	1358    823     565     348))
     (H . (442  	816		34	0  	1056    640     439     270)))
    ((L . (1094  	270		4	5  	2620    1588    1091    672))
     (M . (860  	504		4	14 	2059    1248    857     528))
     (Q . (614  	750		11	14 	1468    890     611     376))
     (H . (464  	900		16	14 	1108    672     461     284)))
    ((L . (1174  	300		6	4  	2812    1704    1171    721))
     (M . (914  	560		6	14 	2188    1326    911     561))
     (Q . (664  	810		11	16 	1588    963     661     407))
     (H . (514  	960		30	2  	1228    744     511     315)))
    ((L . (1276 	312		8	4  	3057    1853    1273    784))
     (M . (1000  	588		8	13 	2395    1451    997     614))
     (Q . (718  	870		7	22 	1718    1041    715     440))
     (H . (538  	1050		22	13 	1286    779     535     330)))
    ((L . (1370 	336		10	2  	3283    1990    1367    842))
     (M . (1062  	644		19	4  	2544    1542    1059    652))
     (Q . (754  	952		28	6  	1804    1094    751     462))
     (H . (596  	1110		33	4  	1425    864     593     365)))
    ((L . (1468 	360		8	4  	3517    2132    1465    902))
     (M . (1128  	700		22	3  	2701    1637    1125    692))
     (Q . (808  	1020		8	26 	1933    1172    805     496))
     (H . (628  	1200		12	28 	1501    910     625     385)))
    ((L . (1531 	390		3	10 	3669    2223    1528    940))
     (M . (1193  	728		3	23 	2857    1732    1190    732))
     (Q . (871  	1050		4	31 	2085    1263    868     534))
     (H . (661  	1260		11	31 	1581    958     658     405)))
    ((L . (1631 	420		7	7  	3909    2369    1628    1002))
     (M . (1267 	784		21	7  	3035    1839    1264    778))
     (Q . (911  	1140		1	37 	2181    1322    908     559))
     (H . (701  	1350		19	26 	1677    1016    698     430)))
    ((L . (1735 	450		5	10 	4158    2520    1732    1066))
     (M . (1373 	812		19	10 	3289    1994    1370    843))
     (Q . (985  	1200		15	25 	2358    1429    982     604))
     (H . (745  	1440		23	25 	1782    1080    742     457)))
    ((L . (1843 	480		13	3  	4417    2677    1840    1132))
     (M . (1455 	868		2	29 	3486    2113    1452    894))
     (Q . (1033  	1290		42	1  	2473    1499    1030    634))
     (H . (793  	1530		23	28 	1897    1150    790     486)))
    ((L . (1955 	510		17	0  	4686    2840    1952    1201))
     (M . (1541 	924		10	23 	3693    2238    1538    947))
     (Q . (1115  	1350		10	35 	2670    1618    1112    684))
     (H . (845  	1620		19	35 	2022    1226    842     518)))
    ((L . (2071 	540		17	1  	4965    3009    2068    1273))
     (M . (1631 	980		14	21 	3909    2369    1628    1002))
     (Q . (1171  	1440		29	19 	2805    1700    1168    719))
     (H . (901  	1710		11	46 	2157    1307    898     553)))
    ((L . (2191 	570		13	6  	5253    3183    2188    1347))
     (M . (1725 	1036		14	23 	4134    2506    1722    1060))
     (Q . (1231  	1530		44	7  	2949    1787    1228    756))
     (H . (961  	1800		59	1  	2301    1394    958     590)))
    ((L . (2306 	570		12	7  	5529    3351    2303    1417))
     (M . (1812 	1064		12	26 	4343    2632    1809    1113))
     (Q . (1286 	1590		39	14 	3081    1867    1283    790))
     (H . (986  	1890		22	41 	2361    1431    983     605)))
    ((L . (2434 	600		6	14 	5836    3537    2431    1496))
     (M . (1914 	1120		6	34 	4588    2780    1911    1176))
     (Q . (1354 	1680		46	10 	3244    1966    1351    832))
     (H . (1054  	1980		2	64 	2524    1530    1051    647)))
    ((L . (2566 	630		17	4  	6153    3729    2563    1577))
     (M . (1992 	1204		29	14 	4775    2894    1989    1224))
     (Q . (1426 	1770		49	10 	3417    2071    1423    876))
     (H . (1096  	2100		24	46 	2625    1591    1093    673)))
    ((L . (2702 	660		4	18 	6479    3927    2699    1661))
     (M . (2102 	1260		13	32 	5039    3054    2099    1292))
     (Q . (1502 	1860		48	14 	3599    2181    1499    923))
     (H . (1142  	2220		42	32 	2735    1658    1139    701)))
    ((L . (2812 	720		20	4  	6743    4087    2809    1729))
     (M . (2216 	1316		40	7  	5313    3220    2213    1362))
     (Q . (1582 	1950		43	22 	3791    2298    1579    972))
     (H . (1222  	2310		10	67 	2927    1774    1219    750)))
    ((L . (2956 	750		19	6  	7089    4296    2953    1817))
     (M . (2334 	1372		18	31 	5596    3391    2331    1435))
     (Q . (1666 	2040		34	34 	3993    2420    1663    1024))
     (H . (1276 	2430		20	61 	3057    1852    1273    784)))))

(define alignment-tbl
  #(()
    (6  18)      
    (6  22)
    (6  26)
    (6  30)
    (6  34)
    (6  22  38)
    (6  24  42)
    (6  26  46)
    (6  28  50)
    (6  30  54)
    (6  32  58)
    (6  34  62)
    (6  26  46  66)
    (6  26  48  70)
    (6  26  50  74)
    (6  30  54  78)
    (6  30  56  82)
    (6  30  58  86)
    (6  34  62  90)
    (6  28  50  72  94)
    (6  26  50  74  98)
    (6  30  54  78  102)
    (6  28  54  80  106)
    (6  32  58  84  110)
    (6  30  58  86  114)
    (6  34  62  90  118)
    (6  26  50  74  98  122)
    (6  30  54  78  102  126)
    (6  26  52  78  104  130)
    (6  30  56  82  108  134)
    (6  34  60  86  112  138)
    (6  30  58  86  114  142)
    (6  34  62  90  118  146)
    (6  30  54  78  102  126  150)
    (6  24  50  76  102  128  154)
    (6  28  54  80  106  132  158)
    (6  32  58  84  110  136  162)
    (6  26  54  82  110  138  166)
    (6  30  58  86  114  142  170)))
