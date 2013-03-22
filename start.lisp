;---------------------------------------------------------------------
; FILE : POPCO's start.lisp (modified version of PT's COHERE-START.lisp)
; PROGRAMMER: Paul Thagard; modified by Marshall Abrams
; CREATED : 5-1-1995
; NOTES: POPCO v.2 2013
; Based on COHERE:
; Version 3.0 with HOTCO, 10-97
; Ported to G3 PowerPC, 4-98.
; HOTCO 2, 6-2000.
; WEB version available 3-2001.
;Copyright(c) Paul Thagard
; University of Waterloo. 1995, 1997, 2000, 2001.
; With modifications by Marshall Abrams 2011, 2012

; *********************************************************************

(declaim #+sbcl(sb-ext:muffle-conditions style-warning)) ; turn off annoying style warnings in SBCL -MA

(defconstant +progloc+ "./") ; added by M.Abrams
(defun myload (filename)
  (load (concatenate 'string +progloc+ filename))
  (format t "~S " filename))

(print "POPCO")

(print "Welcome to COHERE, incorporating ACME, ECHO, DECO, IMP, HOTCO etc.")
(print "Version 4.0. June, 2000")
(print "This program is copyright (c) Paul Thagard 1996, 1997, 2000.")
(print "Permission is granted for use for research purposes only.")

(print "Modifications, additional code for POPCO by Marshall Abrams and Kristen Hammack copyright (c) Marshall Abrams 2012, 2013.")

(load "~/quicklisp/setup")
(ql:quickload "cl-ppcre")  ; perl-compatible regular expressions used in popco-fmt-utils.lisp
;(use-package 'cl-ppcre) ; for regex-replace
; Maybe personal-to-generic-sym, etc. should be rewritten using this now.

;; These files started as Thagard's COHERE files, though they've been modified:
(myload "variables") 
(myload "utilities")
(myload "network") ; i.e. neural networks internal to persons
(myload "echo")
(myload "imp") ; NOTE: See note below [it *should* be below] in the (load "popco") paragraph.
(myload "acme")
(myload "cohere") ; redef of pls is OK--I like this one better
(myload "acme-infer")
; consensus.lisp now replaced by persons.lisp, loaded below.

(myload "social-net")
(myload "persons")
(myload "natsel")
(myload "popco-macros")
(myload "popco-utils")

(myload "popco-fmt-utils")
(myload "popco-fmt-csv")
(myload "popco-fmt-netlogo")
(myload "popco-fmt-guessML-general")
(myload "popco-fmt-guessML-specific1")
(myload "popco-model-run")

#+sbcl (progn
        (print "SBCL-specific code:")
        (myload "sbcl-sockets")
        (myload "popco-fmt-guessCmds"))

;; NOTE: Do NOT move the popco.lisp load before the load of imp.lisp, 
;; unless you know what you're doing.  Last time I checked, I was
;; overwriting the defmacro of normalize-degree in imp.lisp with a
;; different one in popco.lisp.
;(print "Loading POPCO")
;(myload "logistic")
(myload "popco")
(format t "Main POPCO routines loaded (popco.lisp).~%")
(setq *asymptote* .0001)
(myload "HT1989params")
(format t "~%Network and ACME parameters have been set to values specified in Holyoak and Thagard 1989 'Analog mapping by constraint satisfaction'.~%")

(setf *silent-run?* t)

;(print-parameters)
(format t "COHERE+POPCO ready.~%")
