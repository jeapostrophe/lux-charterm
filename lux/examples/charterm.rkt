#lang racket/base
(require racket/match
         racket/runtime-path
         racket/file
         charterm
         lux
         lux/chaos/charterm)

(struct zip (before after))
(define (zip->list z)
  (append (reverse (zip-before z))
          (zip-after z)))

(struct demo (text)
        #:methods gen:word
        [(define (word-fps w)
           0.0)
         (define (word-label s ft)
           (lux-standard-label "Demo" ft))
         (define (word-event w e)
           (match-define (demo z) w)
           (match e
             ['escape
              #f]
             ['return
              w]
             ['up
              (define nz
                (match z
                  [(zip (cons lc lsb) lsa)
                   (zip lsb (cons lc lsa))]
                  [_
                   z]))
              (demo nz)]
             ['down
              (define nz
                (match z
                  [(zip lsb (cons lc lsa))
                   (zip (cons lc lsb) lsa)]
                  [_
                   z]))
              (demo nz)]
             ['left
              (match-define (zip lsb (cons lc lsa)) z)
              (define nlc
                (match lc
                  [(zip (cons cc lb) la)
                   (zip lb (cons cc la))]
                  [_
                   lc]))
              (define nz (zip lsb (cons nlc lsa)))
              (demo nz)]
             ['right
              (match-define (zip lsb (cons lc lsa)) z)
              (define nlc
                (match lc
                  [(zip lb (cons cc la))
                   (zip (cons cc lb) la)]
                  [_
                   lc]))
              (define nz (zip lsb (cons nlc lsa)))
              (demo nz)]
             ['backspace
              (match-define (zip lsb (cons lc lsa)) z)
              (define nlc
                (match lc
                  [(zip (cons cc lb) la)
                   (zip lb la)]
                  [_
                   lc]))
              (define nz (zip lsb (cons nlc lsa)))
              (demo nz)]
             [(or (? char-graphic? k)
                  (? char-whitespace? k))
              (match-define (zip lsb (cons lc lsa)) z)
              (match-define (zip lb la) lc)
              (define nlc (zip (cons k lb) la))
              (define nz (zip lsb (cons nlc lsa)))
              (demo nz)]))
         (define (word-output w)
           (match-define (demo z) w)
           (lambda (width height)
             (charterm-clear-screen)
             (define cur-x
               (match z
                 [(zip _ (cons (zip b _) _))
                  (add1 (length b))]
                 [(zip _ '())
                  0]))
             (define file-y
               (match z
                 [(zip b _)
                  (add1 (length b))]))
             (define cur-y
               (+ 1 (quotient height 2)))
             (define status-y (- height 1))
             (charterm-cursor 1 status-y)
             (charterm-inverse)
             (charterm-display #:width width
                               (format "(~a,~a) ~a"
                                       cur-x file-y
                                       (bytes->string/utf-8 (path->bytes me))))
             (charterm-normal)
             (define (display-line y l)
               (for ([c (zip->list l)]
                     [x (in-naturals 1)])
                 (charterm-cursor x y)
                 (charterm-display (string c))))
             (for ([l (zip-before z)]
                   [y (in-range (quotient height 2) 0 -1)])
               (display-line y l))
             (for ([l (zip-after z)]
                   [y (in-range cur-y status-y)])
               (display-line y l))
             (charterm-cursor cur-x cur-y)))
         (define (word-tick w)
           (match-define (demo z) w)
           w)])

(define-runtime-path me "charterm.rkt")
(define (demo-it)
  (define d
    (demo
     (zip '()
          (for/list ([l (in-list (file->lines me))])
            (zip '() (string->list l))))))
  (fiat-lux d))

(module+ main
  (call-with-chaos
   (make-charterm)
   (λ ()
     (demo-it))))
