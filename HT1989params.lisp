; HT1989params.lisp
; Settings for emulation within POPCO of results in H&T's 1989 
; "Analog mapping by constraint satisfaction"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Parameters specified by H&T 1989 "Analog retrieval as constraint satisfaction"

(setf *min-activation* -1)
(setf *max-activation* 1)
; These are the default values of .99 and -.99, as per p. 313 bottom,
; which says 1 and -1 are max and min.  So don't really have to set
; them here.  Note however comment in footnote  on p. 315: 
; "Performance is improved for some examples if the minimum
; and maximum activation values are made asymmetric (min = -.3), 
; as advocated by Grossberg (1978)."

; *grossberg?*
(gross-on) ; p. 313
; Specifies that we use Grossberg's network updating algorithm rather than
; McClellan and Rumelhart's.  (gross-off) turns sets this variable to nil.
; 12/2012: THIS IS NOW IGNORED--WE USE GROSSBERG NO MATTER WHAT.

; *output-threshold*
(output 0)
; Footnote to p. 315:
; "For both rules it proved important to impose a zero threshold on
; the outputs of units, so that units with negative activations do
; not influence the units to which they are connected.  Without this
; restriction, two units with negative activation levels that have
; an inhibitory weight on the connection between them will excite
; each other, yielding counterintuitive results for some examples.

; *decay-amount*
(decay 1/10)   ; p. 348 [but "Decay rates form .001 to .2 work equally well...."]

; *excit-weight*
(excit 1/10)   ; p. 348 ["values ranging from .01 to .12 work"]

; *inhib-weight*
(inhib -2/10)  ; p. 348 ["Inhibition values can range all the way to -.9 
             ;         without causing problems, except that high
             ;         inhibition can prevent ACME from ovecoming
             ;         an initially promising but incorrect mapping."]
; p. 348 "Although it is not crucial to the functioning of the networks,
; it was found desirable to have inhibition higher than excitation,
; because that produces greater separation in the asymptotic values of
; best and second-best units."

(setf *prag-weight* 3/10) ; This is the weight for links created by function PRESUMED.
; "In order to have a large impact, the p2 parameter for weights
; from the pragmatic unit for PRESUMED mappings works best at around
; .3; smaller values produce less differentiation while larger values
; do not produce significantly more differentiation"
