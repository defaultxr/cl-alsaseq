;;;; cl-alsaseq.asd

(asdf:defsystem #:cl-alsaseq
  :name "cl-alsaseq"
  :description "Common Lisp bindings to ALSA MIDI."
  :author "Rick Venn <richard.venn@gmail.com>"
  :maintainer "modula t. <modula-t at pm dot me>"
  :license "GPL"
  :depends-on (#:cffi #:calispel #:optima #:let-over-lambda)
  :serial t
  :components ((:file "package")
               (:file "driver/bindings")
               (:file "driver/event-lookup")
               (:file "driver/cl-alsaseq")
               (:file "midihelper/reader")
               (:file "midihelper/writer")
               (:file "midihelper/clock")
               (:file "midihelper/midihelper")
               (:file "easy-api")
               (:file "readtable")
               (:file "midiloops")))
