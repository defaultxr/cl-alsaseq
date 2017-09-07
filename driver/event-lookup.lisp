(in-package :cl-alsaseq)
(defmacro lookup-table ()
  `(list
   :SND_SEQ_EVENT_SYSTEM nil
   :SND_SEQ_EVENT_NOTE '(:struct snd_seq_ev_note_t)
   :SND_SEQ_EVENT_CONTROLLER '(:struct snd_seq_ev_ctrl_t)
   :SND_SEQ_EVENT_START '(:struct snd_seq_ev_queue_control_t)
   :SND_SEQ_EVENT_TUNE_REQUEST nil
   :SND_SEQ_EVENT_CLIENT_START '(:struct snd_seq_addr_t)
   :SND_SEQ_EVENT_USR0 nil
   :SND_SEQ_EVENT_SYSEX '(:struct snd_seq_ev_ext_t)
   :SND_SEQ_EVENT_USR_VAR0 nil))

(defun snd-seq-condition (key val)
  `((>= event-type (foreign-enum-value 'snd_seq_event_type ,key))
    ',val))

(defmacro cond-lookup ()
  (labels ((lookup (rest)
             (if rest
                 (cons (snd-seq-condition (car rest)
                                          (cadr rest))
                       (lookup (cddr rest))))))
    `(cond ,@(reverse (lookup (lookup-table))))))
