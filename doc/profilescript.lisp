
(in-package :cl-user)
(require :sb-sprof)
(declaim (optimize (speed 3) (safety 0) (space 0)))
(load "start")

; load a script
; then e.g.:
; (sb-sprof:with-profiling (:max-samples 10000 :report :flat :loop nil) (popco))

