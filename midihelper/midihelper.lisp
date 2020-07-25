(in-package :midihelper)
(defun inspect-midihelper ()
  (list '*reader-thread* *reader-thread*
        '*clock-thread* *clock-thread*
        '*writer-thread* *writer-thread*))

(defun start-midihelper (&optional
                           (master-slave :master)
                           (ppqn 96) (reader-map #'identity))
  (assert (or (= ppqn 96)
              (= ppqn 24)))
  (assert (or (eq master-slave :master)
              (eq master-slave :slave)))
  (alexandria:doplist (key val (inspect-midihelper))
    (when val
      (warn "At least one midihelper thread already appears to be running; to restart, run #'stop-midihelper first")
      (return-from start-midihelper nil)))
  (drain-channel *clock-ochan*)
  (drain-channel *clock-ctrl-chan*)
  (start-reader *clock-ctrl-chan* reader-map)
  (start-clock *clock-ctrl-chan* master-slave ppqn)
  (start-writer-thread))

(defun check-midihelper ()
  (alexandria:doplist
      (key val (inspect-midihelper))
    (if (null val)
        (warn "Helper ~A not running" key))))

(defun stop-midihelper ()
  (check-midihelper)
  (ignore-errors (stop-reader))
  (setf *reader-thread* nil)
  (ignore-errors (stop-writer-thread))
  (setf *writer-thread* nil)
  (ignore-errors (stop-clock))
  (setf *clock-thread* nil)
  (inspect-midihelper))
