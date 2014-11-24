#lang racket/base
(require racket/match
         racket/contract/base
         charterm
         lux/chaos)

(struct *charterm (depth-box ct)
        #:methods gen:chaos
        [(define (chaos-yield c e)
           (sync e))
         (define (chaos-event c)
           (wrap-evt (*charterm-ct c)
                     (λ (ct)
                       (charterm-read-key #:charterm ct))))
         (define (chaos-output! c o)
           (when o
             (parameterize ([current-charterm (*charterm-ct c)])
               (o))))
         (define (chaos-label! c l)
           (charterm-title #:charterm (*charterm-ct c) l))
         (define (chaos-swap! c t)
           (define db (*charterm-depth-box c))
           (define og (unbox db))
           (set-box! db (add1 og))
           (begin0 (t)
             (if (zero? og)
                 (close-charterm #:charterm (*charterm-ct c))
                 (set-box! db og))))])

(define (make-charterm)
  (define ct (open-charterm #:current? #f))
  (define depth-box (box 0))

  (*charterm depth-box ct))

(provide
 (contract-out
  [make-charterm
   (-> chaos?)]))
