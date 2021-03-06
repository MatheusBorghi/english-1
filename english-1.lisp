
(defparameter english-1
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

  (defun initial-nodes (network)
    (nth 1 (assoc 'Initial network)))

  (defun final-nodes (network)
    (nth 1 (assoc 'Final network)))

  (defun transitions (network)
    (cddr network))

  (defun trans-node (transition)
    (getf transition 'From))

  (defun trans-newnode (transition)
    (getf transition 'to))

  (defun trans-label (transition)
    (getf transition 'by))


  (defparameter abbreviations
    '((NP kim sandy lee)
      (DET a the her)
      (N consumer man woman)
      (BV is was)
      (CNJ and or)
      (ADJ happy stupid)
      (MOD very)
      (ADV often always sometimes))) 


  (defun recognize (network tape)
    (catch 'stop
      (dolist (initialnode (initial-nodes network))
        (recognize-next initialnode tape network))
      nil))

  (defun recognize-next (node tape network)
    (if (and (null tape) (member node (final-nodes network)))
        (throw 'stop t)
        (dolist (transition (transitions network))
          (if (equal node (trans-node transition))
              (dolist (newtape (recognize-move (trans-label transition) tape))
                (recognize-next (trans-newnode transition) newtape network))))))

  (defun recognize-move (label tape)
    (if (or (equal label (car tape))
            (member (car tape) (assoc label abbreviations)))
        (list (cdr tape))
        (if (equal label '|#|)
            (list tape)
            nil)))

  (defun generate-move (label tape)
    "...")

  (defun generate-next (node tape network)
    "...")

  (defun generate (network)
    (dolist (initialnode (initial-nodes network))
      (generate-next initialnode nil network)))
