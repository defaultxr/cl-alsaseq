(in-package :cl-alsaseq.quick)

(defmacro def-event-func (event args &body body)
  `(defun ,(intern (format nil "SEND-~A" event))
       ,args ,@body))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (mapcar
   (lambda (event-type)
     (let ((ctrl-type (intern (format nil "SND_SEQ_EVENT_~A" event-type) :keyword)))
       (compile (intern (format nil "SEND-~A" event-type))
                `(lambda (channel param value
                          *seq my-port)
                   (send-ctrl channel param value ,ctrl-type *seq my-port)))))
   '(:PGMCHANGE
     :CHANPRESS
     :PITCHBEND
     :CONTROL
     :NONREGPARAM
     :REGPARAM)))


(defun send-note-on (velocity note channel
                     *seq my-port)
  (send-note velocity note channel :SND_SEQ_EVENT_NOTEON *seq my-port))

(defun send-note-off (velocity note channel
                      *seq my-port)
  (send-note velocity note channel :SND_SEQ_EVENT_NOTEOFF *seq my-port))
