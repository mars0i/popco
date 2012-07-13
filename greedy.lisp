; File: greedy.lisp
; Purpose: greedy algorithm for maximizing coherence;
; idea due to Toby Donaldson based on GSAT
; algorithm of Selman et al., Proc. AAAI-92.
; Programmer: Paul Thagard, 2-96

; GREEDY is an algorithm for approximating the most
; coherent solution. It starts
; with a randomly or otherwise generated solution,
; i.e. assignment
; to A and R. Then it repeatedly flips an element from
; A to R or R to A, based on a calculation of which
; flip will most increase the coherence score. It
; stops either when a maximum number of flips have
; taken place, or when flipping has ceased to increase
; the coherence score.



(defun greedy (max-flips)
  (do ((solution (greedy-start))
       (count-flips 1)
       (best-weight-so-far 0)
       )
      ((or (= count-flips max-flips) ; enough tries
           (= best-weight-so-far (second solution))
           ; no progress in increasing coherence
           )
       (setq *greedy-solution* solution) ; return best solution
       )
    ; repeat:
    (my-print "Best solution at " count-flips " is " solution)
    (setq best-weight-so-far (second solution))
    (setq solution (best-flip solution))
    (setq count-flips (1+ count-flips))
    )
  )

; GREEDY-START produces an initial solution.
; If the coherence mode is pure, this is randomly
; generated, even for the special element. If the
; mode is tempered, special is put into A, but is
; not flippable (see below). If the mode is
; tempered+, then all the favored elements are put
; into A, although they can still be flipped out. If the
; mode is foundational, then all the favored elements are
; put into A and cannot be flipped out.
; Greedy-start returns a solution of the form
; (1 %weight %number accepted-list rejected-list)

(defun greedy-start ()
  (do ((elements (if (or (equal *eval-mode* 'foundational)
                         (equal *eval-mode* 'tempered+)
                         )
                     (set-difference (get *the-person* 'all-units) (get-favored))
                   ; else
                   (get *the-person* 'all-units) ; everything random
                   )
                 (cdr elements)
                 )
       (accepted (cond ((equal *eval-mode* 'tempered)
                        (list 'special)
                        )
                       ((or (equal *eval-mode* 'foundational)
                            (equal *eval-mode* 'tempered+)
                            )
                        (cons 'special (get-favored))
                        )
                       (t nil) ; pure mode
                       )
                 )
       (rejected nil)
       )
      ((null elements) ; return
       (cons 1 (append (coh-score (list 1 accepted rejected))
                       (list accepted rejected)
                       )
             )
       )
    
    ; repeat: Do random assignment of remaining.
    (if (random-yes) (push (car elements) accepted)
      (push (car elements) rejected)
      )
    )
  )

; RANDOM-YES yields t or nil randomly.

(defun random-yes ()
  (if (> (random 2) 0) 't
    nil
    )
  )

; BEST-FLIP figures out what element can best be flipped,
; i.e. moved from A to R or vice versa in a way that
; increases coherence more than flipping other elements.
; What elements are candidates for flipping depends on the
; mode. In case of ties, selection is random.
; It returns a solution. best-flips-so-far is a list
; of (element %weight)



(defun best-flip (solution)
  (do ((flippables (flip-candidates) (cdr flippables))
       (best-flips-so-far (list solution))
       (new-candidate nil)
       (to-flip nil)
       )
      ((null flippables) ; return
       (setq to-flip (caar (randomize best-flips-so-far)))
       (my-print "Flipping " to-flip)
       (flip to-flip solution)
       )
    ; repeat
    (setq new-candidate (flip-short (car flippables) solution))
    (cond ((equal (better-solution new-candidate
                                   (car best-flips-so-far)
                                   )
                  'yes ; new one is better
                  )
           (setq best-flips-so-far (list new-candidate)); replace
           )
          ((equal (better-solution new-candidate
                                   (car best-flips-so-far)
                                   )
                  'tie ; no difference
                  )
           (push new-candidate best-flips-so-far) ; add to list
           )
          ) ; otherwise no change
    )
  )

; FLIP-CANDIDATES provides a list of candidates for
; flipping based on the mode. If coherence is
; foundational, favored elements are not flippable.
; If coherence is tempered or tempered+, then special is not flippable.
; If coherence is pure, everything is flippable.

(defun flip-candidates ()
  (cond ((equal *eval-mode* 'pure) (get *the-person* 'all-units)) ;special?
        ((or (equal *eval-mode* 'tempered)
             (equal *eval-mode* 'tempered+)
             )
         (remove 'special (get *the-person* 'all-units))
         )
        ((equal *eval-mode* 'foundational)
         (set-difference (get *the-person* 'all-units) (get-favored))
         )
        )
  )

; BETTER-SOLUTION determines whether a solution has
; a higher weight than another, reporting yes, no, or tie.

(defun better-solution (solution1 solution2)
  (cond ((> (second solution1) (second solution2))
         'yes
         )
        ((< (second solution1) (second solution2))
         'no
         )
        (t 'tie)
        )
  )

; FLIP produces a new solution by taking an element and
; moving it from accepted to rejected or vice versa.

(defun flip (element solution)
  (let (number accepted rejected)
    (setq number (1+ (car solution)))
    (cond ((member element (fourth solution)) ; accepted
           (setq accepted (remove element (fourth solution)))
           (setq rejected (cons element (fifth solution)))
           )
          (t (setq accepted (cons element (fourth solution))) ; rejected
             (setq rejected (remove element (fifth solution)))
             )
          )
    (cons number (append (coh-score (list number accepted rejected))
                         (list accepted rejected) ; return
                         )
          )
    )
  )
; FLIP-SHORT does not produce a whole solution, just a list of
; the element flipped and its %weight score.

(defun flip-short (element solution)
  (let (number accepted rejected)
    (setq number (1+ (car solution)))
    (cond ((member element (fourth solution)) ; accepted
           (setq accepted (remove element (fourth solution)))
           (setq rejected (cons element (fifth solution)))
           )
          (t (setq accepted (cons element (fourth solution))) ; rejected
             (setq rejected (remove element (fifth solution)))
             )
          )
    (cons element (coh-score (list number accepted rejected)))
    )
  )
