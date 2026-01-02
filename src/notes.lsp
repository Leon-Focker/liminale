;; * notes

(in-package :relax)

(defstruct (note
            (:constructor make-note
                (&key start duration freq velocity
                 &aux (time-left duration))))
  (start 0 :type integer)		; in miliseconds
  (duration 0 :type integer)		; in miliseconds
  (freq 440 :type number)		; in Hz
  (velocity 0.7 :type number)		; 0-1
  (time-left 0 :type integer))          ; in miliseconds


;; ** generation

;; *** add-harmony
;;; TODO
;;; Given a list of currently playing note, which notes should additionally
;;; start to play? Return additional notes as a list.
(defun add-harmony (note-list &optional (time 0))
  ;; use the 'time-left slot to see how harmony developes.
  ;; depending on how many notes are playing, add some
  (case (length note-list)
    (0 (list (make-note :start time :duration 2000)))
    (1 )
    (2 )
    (3 )))

;; *** generate-relaxing-notes
;;; Generate the note material for some relaxing music, focused on long, slow,
;;; and consonant harmony.
;;; - duration: Duration in seconds.
;;; - time-step: rhythmical grid resolution in seconds.
(defun generate-relaxing-notes (duration time-step)
  (setf duration (round (* 1000 duration)))
  (setf time-step (round (* 1000 time-step)))
  (let ((time 0)
	(note-list '())
	(active-notes '())) ; a list of notes that are playing at 'time
    (loop while (<= time duration) do
      ;; update list of active-notes
      (setf active-notes
	    (loop for note in active-notes
		  do (decf (note-time-left note) time-step)
		  when (> (note-time-left note) 0)
		    collect note))
      ;; add harmony
      (loop for note in (add-harmony active-notes time)
	    do (push note active-notes)
	       (push note note-list))
      ;; step forward in time 
      (incf time time-step))
    ;; vary start-times
    (mess-with-start-times note-list)))

;; ** modifiers

;; *** mess-with-start-times
;;; TODO
(defun mess-with-start-times (note-list)
  ;; I want some sections to have regular rhythms and some to be more
  ;; 'arbitrary' -> shifting notes with fixed seed randomness depending on
  ;; sine-wave-esque multiplier...?
  note-list
  )

;; ** tests

(defun test-note-list (note-list &optional (fn-name "test-note-list"))
  (unless (listp note-list)
    (error "~a: note-list not a list!" fn-name))
  (unless (loop for note in note-list always (equal 'note (type-of note)))
    (error "~a: not all elements are notes!" fn-name)))

;; ** midi

(defun notes-to-midi (note-list file)
  (test-note-list note-list "notes-to-midi")
  (let ((pitches '())
	(durs '())
	(starts '())
	(velos '()))
    (loop for note in note-list
	  do (push (sc::freq-to-midi (note-freq note)) pitches)
	  do (push (/ (note-duration note) 1000) durs)
	  do (push (/ (note-start note) 1000) starts)
	  do (push (note-velocity note) velos))
    (lists-to-midi pitches durs starts
		   :velocity-list velos
		   :file file)))

;; EOF notes.lsp
