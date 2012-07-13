;; popco-alt-loop.lisp
;; Copyright (c) 2012 by Marshall Abrams
;; Experimental alterations of main loop to allow a more interactive
;; relationship with NetLogo.

(defun run-population (population &key cont-prev-sess)
  (unwind-protect ; allows us to ensure that files will be closed
    (progn
      ; what's inside this progn is the real work we want done

      ; If user doesn't request continue prev session, store current random seed/state into a file that can recreate it later:
      (unless cont-prev-sess
        (with-open-file (random-state-file-stream *random-state-file* :direction :output :if-exists :rename :if-does-not-exist :create)
          (format random-state-file-stream "(format t \"~%Restoring previous random state from file.~%\")~%(setf *random-state* ~S)" *random-state*)))

      ; Now open files to record data as we go, if user specifies that we should.
      ; Whether we open the files for appending or for recreating depends on whether cont-prev-sess was specified as t, or is nil.

      (when *do-report-to-netlogo*
        (cond (cont-prev-sess
                (setf *netlogo-outstream* (open *netlogo-output-name* :direction :output :if-exists :append :if-does-not-exist :error)))
              (t
                (format t "Recreating output file for NetLogo ~S~%" *netlogo-output-name*)
                (setf *netlogo-outstream* (open *netlogo-output-name* :direction :output :if-exists :rename :if-does-not-exist :create))
                (report-persons-initially-for-netlogo population))))

      (when *do-report-propns-to-csv*
        (cond (cont-prev-sess
                (setf *propns-csv-outstream* (open *propns-csv-output-name* :direction :output :if-exists :append :if-does-not-exist :error)))
              (t
                (format t "Recreating output file csv file ~S~%" *propns-csv-output-name*)
                (setf *propns-csv-outstream* (open *propns-csv-output-name* :direction :output :if-exists :rename :if-does-not-exist :create))
                (report-persons-initially-for-csv population))))


      (do ()
          ((time-to-stop) population) ; keep looping until (time-to-stop) returns true
        (when *sleep-delay* (sleep *sleep-delay*))
        (run-population-once population)
        (when (and *write-person-graphs-at-pop-ticks* (= *pop-tick* (car *write-person-graphs-at-pop-ticks*)))
          (setf *write-person-graphs-at-pop-ticks* (cdr *write-person-graphs-at-pop-ticks*))
          (write-person-graphs (format nil "~A/~A/" *person-graphs-basename* *pop-tick*))))

     ) ; end of progn for main code in unwind-protect

    ; cleanup routines for unwind-protect
    (when *netlogo-outstream* (close *netlogo-outstream*))
    (when *propns-csv-outstream* (close *propns-csv-outstream*))
   ) ; end of unwind-protect
 t)
; end of run-population


;; CHANGES:
;; moved update-* to beginning rather than end.  This means that these will be called
;; unnecessarily after init-pop, which calls them as well (and then does some other stuff),
;; but it separates the function into two sections delimited by the report stages.

(defun run-population-once-v2 (population)  ; [input->output] for each stage is indicated in comments.
  (incf *pop-tick*)
  (report-conversations                      ; optionally report conversations to external gui, etc. [(conversations-plus . pop)->pop]
    (transmit-environments                   ; like transmit utterances, but "utterances" of the external world, or other "cognitively spontaneous" emphasis
      (transmit-utterances                   ; add propositions uttered to listeners [(conversations . pop)->(conversations-plus . pop)]
        (choose-utterances                   ; choose propositions spoken [(converser-pairs . pop)->(conversations . pop)]
          (choose-conversers                 ; choose who talks to whom [pop->(converser-pairs . pop)]
            (report-persons                  ; optionally report state of persons to external gui, etc. [pop->pop]
              (settle-nets                   ; settle nets, at least partially [pop->pop] +
                (update-proposition-nets     ; update proposition network links based on activations of proposition-map-units [pop->pop]
                  (update-analogy-nets population)))))))))  ; update internal analogy nets to reflect newly added propositions [pop->pop]
    (report-progress-to-console) ; brief report on what tick [etc.] just occurred
    (finish-output))


