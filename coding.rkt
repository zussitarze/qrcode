#lang racket/base

(require racket/contract
         racket/list
         "galois.rkt")

(provide 
 (contract-out 
  [reed-solomon-generator (-> exact-positive-integer? list?)]
  [reed-solomon-encode (-> list? list? boolean? list?)]
  [bch-encode-QR (-> list? list?)]
  [golay-encode-QR (-> list? list?)]))

(define (reed-solomon-generator p) 
  (for/fold ([acc '(1)])
    ([i (in-range 0 p)])
    (gfpoly* acc (list 1 (gfexp i)))))

;; Encodes message with num-parity bytes.  Parity check symbols are
;; formed by X^p * u(x) mod g(x), where p = num-parity.  Since g(x) is
;; monic, we can take the leading coefficient of each dividend as the
;; next quotient term.
(define (reed-solomon-encode msg gen systemic?)
  (let* ([p (gfpoly-degree gen)]
         [shifted-msg (gfpoly-shift msg p)]
         [parity (ensure-length p (gfpoly-monic-remainder shifted-msg gen))])
    (if systemic?
        (gfpoly+ shifted-msg parity)
        parity)))

;; Performs BCH (15,5) encoding with mask for QR format bits.
(define (bch-encode-QR msg)
  (let* ([gen '(1 0 1 0 0 1 1 0 1 1 1)]
         [shifted-msg (gfpoly-shift msg 10)]
         [parity (gfpoly-monic-remainder shifted-msg gen)])
    (ensure-length 15 
                   (gfpoly+ shifted-msg 
                            parity
                            '(1 0 1 0 1 0 0 0 0 0 1 0 0 1 0)))))

;; Performs Golay (18,6) encoding for QR version bits
(define (golay-encode-QR msg)
  (let* ([gen '(1 1 1 1 1 0 0 1 0 0 1 0 1)]
         [shifted-msg (gfpoly-shift msg 12)]
         [parity (gfpoly-monic-remainder shifted-msg gen)])
    (ensure-length 18 (gfpoly+ shifted-msg parity))))

;; left pads lists with 0s as necessary
(define (ensure-length p l)
  (if (< (length l) p)
      (append (make-list (- p (length l)) 0) l)
      l))


