;; parenting18analogsAsBiases1h.lisp
;; uses/applies code in parenting18analogsAsBiases1.lisp (q.v. for outline of purpose, etc.)

; We use new output filenames for each session to preserve data:
;(defvar *basename* (concatenate 'string "../data/parenting18analogsAsBiases1f" *run-id*))
;(setf *netlogo-basename* *basename*)
; We have to construct the netlogo output file by hand below, because popco.lisp has already constructed its version.
; But we have to set *netlogo-basename* to what we want so that report-persons-just-at-t-for-netlogo,
; which runs later, will be able to construct the name that we want.

; Make a pop of people who only talk about lifestyle propns (any of them)
; and whose origin propns are only sky propns, where one person is the only
; initial perceiver (of all lifestyle propns) who then communicates them to
; others.  I'm hoping this will allow the origin analogs to affect community
; beliefs by in effect reducing the effect of perceptions by making them
; filter from one person.  QUESTION: IS EFFECT OF UTTERANCE PARAMETER SET
; OPTIMALLY?  REMEMBER THAT UTTERANCES IN EFFECT CREATE PERCEPTIONS.

(load "sanday/parenting18analogsAsBiases1")

(defvar *my-pop-size* 10)
;(setf *netlogo-output-name* (concatenate 'string *basename* ".nlogdat")) ; file to write propn activn data in csv format
;(setf *propns-csv-output-name* (concatenate 'string *basename* ".csv")) ; file to write propn activn data in csv format
;(setf *random-state-file* (concatenate 'string *basename* ".lisp")) ; file to write propn activn data in csv format

; don't move graph around in telguess:
(setf *guess-layout-commands* "")
(setf *extra-meta-commands* "")
(setf *do-converse* t)

(make-earth-biased-lifestyle-talker 'temp-person)
(n-persons-with-name 'temp-person 's (1- *my-pop-size*)) ; "s" for sky-based
(rem-elt-from-property 'temp-person 'folks 'members)
(make-earth-biased-lifestyle-talker 'sp lifestyle-propns) ; "sp" for sky-based perceiver

(init-pop)
(print (get 'folks 'members))

(setf *max-pop-ticks* 100)
(popco)
