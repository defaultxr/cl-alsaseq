;;;; cl-alsaseq.asd

(asdf:defsystem cl-alsaseq
  :serial t
  :description "CL bindings to alsa midi sequencer"
  :author "Rick Venn <richard.venn@gmail.com>"
  :license "GPL"
  :depends-on (#:cffi #:calispel #:optima #:let-over-lambda)
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
