(in-package :midiloops)

(defparameter +quneo-chan+ 15)
(defparameter +quneo-led-chan+ 15)

(defmacro quneo-note (note &body body)
  `((plist :EVENT-TYPE :SND_SEQ_EVENT_NOTEON
           :EVENT-DATA (plist NOTE ,note
                              CHANNEL +quneo-chan+))
    ,@body))

(defvar *active-loop* 1)
(defvar *loop-group* 1)

(defun loop-ctrl (function &optional (loop-id *active-loop*))
  (list :event-type :loop-ctrl
        :loop-id loop-id
        :function function))

(defun global-ctrl (function)
  (list :event-type :global-ctrl
        :function function))

(defun quneo-map-event (in-event)
  (macromatch in-event
    ;;Transport buttons
    (quneo-note 26
      (list (loop-ctrl #'loop-play)))
    (quneo-note 25
      (list (loop-ctrl #'loop-stop)))
    (quneo-note 24
      (list (loop-ctrl #'loop-erase)))

    ;; odd numbers are loop groups,
    ;; even numbers are loops in group
    (quneo-note 11
      (list (global-ctrl (lambda () (loop-group 1)))))
    (quneo-note 12
      (setf *active-loop* 1)
      nil)

    (quneo-note 13
      (list (global-ctrl (lambda () (loop-group 2)))))
    (quneo-note 14
      (setf *active-loop* 2)
      nil)

    (quneo-note 15
      (list (global-ctrl (lambda () (loop-group 3)))))
    (quneo-note 16
      (setf *active-loop* 3)
      nil)

    (quneo-note 17
      (list (global-ctrl (lambda () (loop-group 4)))))
    (quneo-note 18
      (setf *active-loop* 4)
      nil)

    ;;toggle metronome
    (quneo-note 19
      (list (global-ctrl #'toggle-metronome)))

    ;;Left hand big circle for overdub toggle
    (quneo-note 4
      (list (loop-ctrl #'loop-overdub)))
    ;;Right hand big circle for usual loop-cycle
    (quneo-note 5
      (list (loop-ctrl #'loop-cycle)))

    ;;Horizontal slider for tempo control
    ((plist :EVENT-TYPE :SND_SEQ_EVENT_CONTROLLER
            :EVENT-DATA (plist VALUE control-val
                               PARAM 10
                               CHANNEL +quneo-chan+))
     (set-master-bpm (+ 50 (* control-val 2)))
     nil)

    ;;Pass through any other events
    (_ (list in-event))))

(defun quneo-reader (in-events)
  (apply #'append
         (mapcar #'quneo-map-event
                 in-events)));;monoidal space for in-events list-list

;; transport buttons: 33, 34, 35
;; left hand side bank buttons:
;; 36, 37
;; 38, 39
;; 40, 41
;; 42, 43
