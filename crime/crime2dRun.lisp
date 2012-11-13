(unlock-package 'common-lisp)
(defvar *data-dir* "../data/crime2d") ; second call to defvar *data-dir* in popco.lisp will be ignored
(load "start")
(load "crime/crime2d")
(quit)
