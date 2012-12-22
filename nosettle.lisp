; make sure nothing is ever considered settled:

; By using both defvar and setf, this can be run either before or
; after loading all of popco, and it should have the same effect.
; The first defvar overrides all later ones, which are ignored.
; setfs that come after the defvar are not ignored.
(defvar *min-pop-ticks-to-settle* nil)        ; for master branch in github repository (descendant of seproadback in assembla repository)
(setf *min-pop-ticks-to-settle* nil)        ; for master branch in github repository (descendant of seproadback in assembla repository)

;(setf *min-settle* (* *max-times* 1000000000)) ; for git branch master in 11/2012 up to at least early 12/2012 OBSOLETE

