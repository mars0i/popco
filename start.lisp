;---------------------------------------------------------------------
; FILE : POPCO-START.lisp (modified version of PT's COHERE-START.lisp)
; PURPOSE : Load all coherence files
; PROGRAMMER: Paul Thagard; modified by Marshall Abrams
; CREATED : 5-1-1995
; NOTES: POPCO v.1 2011
; Based on:
; Version 3.0 with HOTCO, 10-97
; Ported to G3 PowerPC, 4-98.
; HOTCO 2, 6-2000.
; WEB version available 3-2001.
;
;Copyright(c) Paul Thagard
; University of Waterloo. 1995, 1997, 2000, 2001.
; With modifications by Marshall Abrams 2011, 2012

;---------------------------------------------------------------------
; Load all the files necessary to operate COHERE:
;---------------------------------------------------------------------

; *********************************************************************

(declaim #+sbcl(sb-ext:muffle-conditions style-warning)) ; turn off annoying style warnings in SBCL -MA

(defconstant +progloc+ "./") ; added by M.Abrams
(defun myload (filename)
  (load (concatenate 'string +progloc+ filename)))

(print "Welcome to COHERE, incorporating ACME, ECHO, DECO, IMP, HOTCO etc.")
(print "Version 4.0. June, 2000")
(print "This program is copyright (c) Paul Thagard 1996, 1997, 2000.")
(print "Permission is granted for use for research purposes only.")

(print "Modifications, additional code for POPCO copyright (c) Marshall Abrams 2012.")

;Lisp code for initializing global variables.  
;(print "Loading global variables.")
(myload "variables") 
(print "Global variables and constants loaded.")

; Lisp code for utility functions.
;(print "Loading utility functions.")
(myload "utilities")
(print "Utility functions loaded.")

; Lisp code for creating and running networks.
;(print "Loading constraint network functions.")
(myload "network") ; overwrite of macro plist from utilities is ok--identical code
(print "Constraint network functions loaded.")

; Lisp code for ECHO
;(print "Loading ECHO.")
(myload "echo")
(print "ECHO functions loaded.")

; Lisp code for DECO
;;(print "Loading DECO.")
;;(myload "deco")
;;(print "DECO functions loaded.")

;; NOTE: See note below [it *should* be below] in the (load "popco") paragraph.
; Lisp code for IMP
;(print "Loading IMP.")
(myload "imp")
(print "IMP functions loaded.")  ; used by acme-infer.lisp

; Lisp code for ACME
;(print "Loading ACME.")
(myload "acme")
(print "ACME functions loaded.")

;; contains various utility functions
;(print "Loading cohere")
(myload "cohere") ; redef of pls is OK--I like this one better
(print "Basic COHERE utility functions loaded.")

; Lisp code for non-connectionist algorithms.
;;(print "Loading non-connectionist coherence algorithms.")
;(myload "greedy")

; Lisp code for emotional coherence.
;(print "Loading HOTCO emotional coherence algorithms.")
(myload "hotco")
(print "HOTCO functions loaded.")

; added by Marshall
;(print "Loading acme-infer")
(myload "acme-infer")
(print "ACME-INFER functions loaded.")

; Lisp code for graphics.
;(print "Loading graphics.")
;(myload "graphics")

;(print "Loading consensus")
(myload "consensus")
(print "Consensus functions loaded.")

(print "All original COHERE files needed (modified for POPCO) have been loaded.")

;(print "Loading additional POPCO utility functions.")
(myload "popco-macros")
(myload "popco-utils")
(print "Additional POPCO utility functions loaded (popco-utils.lisp).")
(terpri)

(print "Loading POPCO data formatting functions:")
(myload "popco-fmt-utils") (print "popco-fmt-utils loaded")
(myload "popco-fmt-csv") (print "popco-fmt-csv loaded")
(myload "popco-fmt-netlogo") (print "popco-fmt-netlogo loaded")
(myload "popco-fmt-guessML-general") (print "popco-fmt-guessML-general loaded")
(myload "popco-fmt-guessML-specific1") (print "popco-fmt-guessML-specific1 loaded")
(myload "popco-model-run") (print "popco-model-run loaded")

#+sbcl (progn
        (myload "sbcl-sockets") (print "sbcl-sockets SBCL socket-handling code loaded.")
        (myload "popco-fmt-guessCmds") (print "popco-fmt-guessCmds loaded"))

;; NOTE: Do NOT move the popco.lisp load before the load of imp.lisp, 
;; unless you know what you're doing.  Last time I checked, I was
;; overwriting the defmacro of normalize-degree in imp.lisp with a
;; different one in popco.lisp.
;(print "Loading POPCO")
;(myload "logistic")
(myload "popco")
(print "Main POPCO routines loaded (popco.lisp).")

(setq *asymptote* .0001)
(load "HT1989params")
(format t "Network and ACME parameters set to values specified in Holyoak and Thagard 1989 'Analog retrieval by constraint satisfaction'.~%~%")

;(print-parameters)

(setf *silent-run?* t)

(format t "~%COHERE+POPCO ready.~%")
