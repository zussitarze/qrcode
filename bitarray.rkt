#lang racket/base

(provide 
  (struct-out bitarray)
  make-bitarray
  bitarray-clone
  bitarray-blit
  bitarray-set
  bitarray-get
  bitarray-toggle)


;; bits are written in low to high order for combatability with monochrome bitmaps,
;; so bits are blitted in reverse order.

(struct bitarray (dimension row-width data))  

(define (make-bitarray dimension)
  (let* ([row-width (ceiling (/ dimension 8))]
         [len (* row-width dimension)])
    (bitarray dimension row-width (make-bytes len 0))))

(define (bitarray-clone ba)
  (struct-copy bitarray ba [data (bytes-copy (bitarray-data ba))]))

(define (bitarray-indexes ba x y)
  (define-values (col-idx bit-offset) (quotient/remainder x 8))
  (values (+ col-idx (* y (bitarray-row-width ba))) bit-offset))

(define (bitarray-blit dest src-bs x y)
  (define-values (byte-idx bit-offset) (bitarray-indexes dest x y))
  (for/fold ([i byte-idx]) ([s (in-list src-bs)])
    (let ([d (bytes-ref (bitarray-data dest) i)])
      (bytes-set! (bitarray-data dest) i 
                  (bitwise-ior d (bitwise-and #xff (arithmetic-shift s bit-offset)))))
    (unless (= 0 bit-offset)
      (let ([d (bytes-ref (bitarray-data dest) (+ i 1))])
        (bytes-set! (bitarray-data dest) (+ i 1)
                    (bitwise-ior d (arithmetic-shift s (+ -8 bit-offset))))))
    (+ i (bitarray-row-width dest))))

(define (bitarray-set dest x y [val #t])
  (define-values (byte-idx bit-offset) (bitarray-indexes dest x y))
  (define target (bytes-ref (bitarray-data dest) byte-idx))  
  (bytes-set! (bitarray-data dest) byte-idx
              (if val
                  (bitwise-ior (arithmetic-shift 1 bit-offset) target)
                  (bitwise-and (bitwise-not (arithmetic-shift 1 bit-offset)) target))))

(define (bitarray-get dest x y)
  (define-values (byte-idx bit-offset) (bitarray-indexes dest x y))
  (bitwise-bit-set? (bytes-ref (bitarray-data dest) byte-idx) bit-offset))

(define (bitarray-toggle dest x y)
  (define-values (byte-idx bit-offset) (bitarray-indexes dest x y))
  (define target (bytes-ref (bitarray-data dest) byte-idx))  
  (bytes-set! (bitarray-data dest) byte-idx
              (bitwise-xor (arithmetic-shift 1 bit-offset) target)))
              