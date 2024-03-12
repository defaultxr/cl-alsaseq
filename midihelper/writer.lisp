(in-package :midihelper)

(defvar *seq* nil);;sequence struct
(defvar **seq nil);;pointer to sequence struct (for memory deallocation)
(defvar *my-ports* nil)

(defun %send-event (description &optional (port (car *my-ports*)) (seq *seq*))
  (match description
    ((plist :EVENT-TYPE (guard event-type (or (equal event-type :snd_seq_event_noteoff)
                                              (equal event-type :snd_seq_event_noteon)))
            :EVENT-DATA (plist ;; DURATION duration OFF_VELOCITY off_velocity
                         VELOCITY velocity NOTE note CHANNEL channel))
     (send-note velocity note channel event-type seq port))
    ((plist :EVENT-TYPE (guard event-type (or (equal event-type
                                                     :snd_seq_event_controller)
                                              (equal event-type
                                                     :snd_seq_event_songpos)
                                              (equal event-type
                                                     :snd_seq_event_pgmchange)
                                              (equal event-type
                                                     :snd_seq_event_chanpress)
                                              (equal event-type
                                                     :snd_seq_event_pitchbend)
                                              (equal event-type
                                                     :snd_seq_event_control14)
                                              (equal event-type
                                                     :snd_seq_event_nonregparam)
                                              (equal event-type
                                                     :snd_seq_event_regparam)))
            :EVENT-DATA (plist VALUE value PARAM param CHANNEL channel))
     (send-ctrl channel param value event-type seq port))
    ((plist :EVENT-TYPE (guard event-type (or (equal event-type
                                                     :snd_seq_event_clock)
                                              (equal event-type
                                                     :snd_seq_event_start)
                                              (equal event-type
                                                     :snd_seq_event_stop)
                                              (equal event-type
                                                     :snd_seq_event_continue))))
     (send-queue-ctrl 0 event-type seq port))
    (_ (format t "unknown event ~S~%" description))))

(defun start-writer ()
  (assert (null **seq))
  (assert (null *seq*))
  (assert (null *my-ports*))
  (setf **seq (open-seq "CL"))
  (setf *seq* (mem-ref **seq :pointer))
  (setf *my-ports*
    (list (open-port (format nil "Output")
             *seq*
             :output))))

(defun stop-writer ()
  (assert **seq)
  (close-seq **seq)
  (setf *seq* nil)
  (setf **seq nil)
  (setf *my-ports* nil))

(defvar *writer-thread* nil)
(defvar *writer-ichan* (make-nonblock-buf-channel))

(defun start-writer-thread ()
  (assert (null *writer-thread*))
  (setf *writer-thread*
        (bt:make-thread
         (lambda ()
           (declare (optimize (debug 3)))
           (with-seq (thread-seq :name "CL")
             (unwind-protect
                  (progn
                    (let ((port (open-port "port0" thread-seq :output)))
                      (handler-case
                          (loop
                            (let ((message (? *writer-ichan*)))
                              (restart-case
                                  (%send-event message
                                               port
                                               thread-seq)
                                (carry-on-writing ()))))
                        (stop-thread ()))))
               (setf *writer-thread* nil))))
         :name "midihelper writer")))

(defun stop-writer-thread ()
  (bt:interrupt-thread
   *writer-thread* (lambda ()
                     (error 'stop-thread))))

(defun send-event (event)
  (! *writer-ichan* event))
