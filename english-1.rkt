#lang racket
(require racket/trace)
(define english-1
  '((Initial (1))
    (Final (9))
    (From 1 to 3 by NP)
    (From 1 to 2 by DET)
    (From 2 to 3 by N)
    (From 3 to 4 by BV)
    (From 4 to 5 by ADV)
    (From 4 to 5 by |#|)
    (From 5 to 6 by DET)
    (From 5 to 7 by DET)
    (From 5 to 8 by |#|)
    (From 6 to 7 by ADJ)    
    (From 6 to 6 by MOD)
    (From 7 to 9 by N)
    (From 8 to 8 by MOD)
    (From 8 to 9 by ADJ)
    (From 9 to 4 by CNJ)
    (From 9 to 1 by CNJ)))

(define (initial-nodes network)
  (cadar
   (filter
    (lambda (x)(eq? 'Initial (car x)))
    network)))

(define (final-nodes network)
  (cadar
   (filter
    (lambda (x)(eq? 'Final (car x)))
    network)))

(define (transitions network)
  (cddr network))

(define (trans-node transition)
  (cadr transition))

(define (trans-newnode transition)
  (cadddr transition))

(define (trans-label transition)
  (last transition))

(define abbreviations
  '((NP kim sandy lee)
    (DET a the her)
    (N consumer man woman)

    (BV is was)
    (CNJ and or)
    (ADJ happy stupid)
    (MOD very)
    (ADV often always sometimes)
    (|#|)))

(define (recognize network tape)
    (for ((initialnode (initial-nodes network)))
            (recognize-next initialnode tape network)))

(define (recognize-next node tape network)
  (if (and (empty? tape) (member node (final-nodes network)))
      1
      (for ((transition (transitions network)))
        ;#:final 
              (if (eq? node (trans-node transition))
                  (for ((newtape (recognize-move (trans-label transition) tape)))
                    #:final (and (empty? newtape) (member (trans-newnode transition) (final-nodes network)))
                    (recognize-next (trans-newnode transition) newtape network))
                  false))))

(define (recognize-move label tape)
  (if (or (eq? label (car tape))
          (member (car tape) (assoc label abbreviations)))
      (list (cdr tape))
      (if (eq? label '|#|)
          (list tape)
          null)))

;(trace recognize)
(trace recognize-next)
;(trace recognize-move)