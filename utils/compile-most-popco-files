#!/bin/sh
ls -1 *.lisp  | grep -v start.lisp | sed 's/.*/(compile-file "&")/' | sbcl --eval '(declaim (optimize (speed 3) (space 0) (debug 0)))' --load start
