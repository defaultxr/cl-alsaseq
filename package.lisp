;;;; package.lisp

(defpackage :cl-alsaseq
  (:use :cl :cffi :calispel :optima :optima.extra)
  (:import-from :let-over-lambda
                :g!-symbol-p
                :defmacro/g!
                :o!-symbol-p
                :o!-symbol-to-g!-symbol
                :defmacro!)
  (:export :open-port
           :close-port
           :open-seq
           :close-seq
           :with-seq
           :send-queue-ctrl
           :send-ctrl
           :send-note
           :recv
           :velocity
           :note
           :channel
           :param
           ))

(defpackage :cl-alsaseq.quick
  (:use :cl :cl-alsaseq)
  (:export :send-note-on
           :send-note-off
           :send-pgmchange
           :send-chanpress
           :send-pitchbend
           :send-control
           ))

(defpackage :midihelper
  (:use :cl :cffi :cl-alsaseq :optima :optima.extra :calispel)
  (:export :*clock-ochan*
           :*clock-ctrl-chan*
           :*reader-ichan*
           :*reader-ochan*
           :set-master-bpm
           :inspect-midihelper
           :start-midihelper
           :stop-midihelper
           :check-midihelper
           :if-gesture
           :if-clock
           :macromatch
           :drain-channel
           :send-event
           :ev-noteon
           :ev-noteoff
           :ev-pgmchange
           :ev-tick
           :ev-microtick
           :ev-start
           :ev-stop
           :ev-continue
           :ev-songpos
           :ev-cc
	   :ev-pitchbend))

(defpackage #:midiloops
  (:use :cl :cl-alsaseq :midihelper :optima :optima.extra :calispel)
  (:import-from :let-over-lambda
                :g!-symbol-p
                :defmacro/g!
                :o!-symbol-p
                :o!-symbol-to-g!-symbol
                :defmacro!)
  (:nicknames :mloops))
