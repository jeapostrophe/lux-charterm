#lang racket/base
(require racket/match
         racket/contract/base
         charterm
         lux/chaos)

(struct *charterm (ct)
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
         (define (chaos-stop! c)
           (close-charterm #:charterm (*charterm-ct c)))])

(define (make-charterm)
  (define ct (open-charterm #:current? #f))

  (*charterm ct))

(provide
 (contract-out
  [make-charterm
   (-> chaos?)]))
