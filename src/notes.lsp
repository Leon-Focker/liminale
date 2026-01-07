;; * notes

(in-package :relax)

(defstruct (note
            (:constructor make-note
                (&key start duration freq velocity type
                 &aux (time-left duration))))
  (start 0 :type integer)		; in miliseconds
  (duration 0 :type integer)		; in miliseconds
  (freq 440 :type number)		; in Hz
  (velocity 0.7 :type number)		; 0-1
  (time-left 0 :type integer)           ; in miliseconds
  (type 'long))                         ; which role this note plays


;; ** generation

;; *** add-harmony
;;; TODO
;;; Given a list of currently playing notes, which notes should additionally
;;; start to play? Return additional notes as a list.
;;;
(defun add-harmony (note-list &optional (time 0))
  ;;(format t "~&time: ~a, ~&notelist: ~a" time note-list)
  (let* ((new-notes '()))
    ;; use the 'time-left slot to see how harmony developes.
    ;; depending on how many notes are playing, add some
    (case (length note-list)
      ;; when no note is playing, check last played long note and generate a
      ;; long note that goes a few steps up or down.
      ;; decide whether to add another note with recursive call.
      (0 (push (make-note :start time
			  :duration (get-long-duration) :type 'long
			  :freq (get-new-frequency))
	       new-notes)
       (setf new-notes (append new-notes (add-harmony new-notes time))))
      ;; check whether other note is long or not
      (1 (push (make-note :start time
			  :duration (get-short-duration) :type 'short
			  :freq (apply #'get-new-frequency (mapcar #'note-freq note-list)))
	       new-notes)
       (setf new-notes
	     (append new-notes (add-harmony (append note-list new-notes) time))))
      ;; check whether other notes are long or not
      (2 (push (make-note :start time
			  :duration (get-short-duration) :type 'short
			  :freq (apply #'get-new-frequency (mapcar #'note-freq note-list)))
	       new-notes))
      ;; check whether other notes are long or not
      (3 ))
    ;; add new-notes to last-played and return them
    ;; (loop for note in new-notes do (push note last-played))
    new-notes))

;; *** similar-freqp
;;; check whether two frequencies are similar; two frequencies are considered
;;; similar when one is no more than 4% greater than the other.
(defun similar-freqp (freq1 freq2 &optional (percent 4))
  (let* ((diff (abs (- freq1 freq2)))
	 (min (min freq1 freq2)))
    (<= diff (* min (/ percent 100)))))

;; *** similar-durp
;;; check whether two durations are similar; two durations are considered
;;; similar when one is no more than 10% greater than the other.
(defun similar-durp (dur1 dur2 &optional (percent 10))
  (let* ((diff (abs (- dur1 dur2)))
	 (min (min dur1 dur2)))
    (<= diff (* min (/ percent 100)))))

;; *** find-sounds-with-sufficient-dur
;;; Find sounds in a soundpile that are long enough to be played at freq for dur
(defun find-sounds-with-sufficient-dur (dur freq soundpile)
  (loop for sound in (data soundpile)
	for sound-dur = (duration sound)
	for required-dur = (* (/ freq (car (fundamental-frequency sound))) dur)
	when (>= sound-dur required-dur) collect sound))

;; *** get-durations
(let ((last '())
      (min-duration-short 500)
      (max-duration-short 10000)
      (min-duration-long 5000)
      (max-duration-long 30000))

  (defun reset-last-durations ()
    (setf last '()))

;;; get a duration somewhere between 5 and 30 seconds
  (defun get-long-duration ()
    (get-duration-aux min-duration-long max-duration-long))
;;; get a duration somewhere between 0.5 and 10 seconds
  (defun get-short-duration ()
    (get-duration-aux min-duration-short max-duration-short))
  
  (defun get-duration-aux (min-dur max-dur)
    (let ((min-mult (ceiling min-dur *relax-grid-mseconds*))
	  (max-mult (floor max-dur *relax-grid-mseconds*)))
      (loop for random-nr = (random-nldd 0.8 (random-relax))
	    for mult = (round (scale-to-log random-nr min-mult max-mult))
	    for new-dur = (* mult *relax-grid-mseconds*)
	    while (remove-if-not #'(lambda (x) (similar-durp x new-dur))
				 (if (> (length last) *min-no-repetitions*)
				     (subseq last 0 *min-no-repetitions*)
				     last))
	    finally (progn (push new-dur last)
			   (return new-dur))))))

;; *** get-new-frequency
;;;
(let ((last-freqs '())
      (min-freq 40)
      (max-freq 600)
      (ratios
	(append '(1/2 2/3 3/4); 4/5 5/6 6/7 7/8)
		'(2 3/2 4/3 5/4); 6/5 7/6 8/7)
		'(3 2 5/3 3/2); 7/5 4/3 9/7)
		'(1/3 1/2 3/5 2/3); 5/7 3/4 7/9)
		'(4 5))))

  (defun reset-last-freqs ()
    (setf last-freqs '()))

  ;; for each playing freq: calculate possible harmonic intervals
  ;; compare lists from each freq, use those present on all lists. 
  (defun get-new-frequency (&rest freqs)
    (when (null freqs)
      (push (if (null last-freqs) 528 (car last-freqs)) freqs))
    (let ((options '())
	  (similar-options '())
	  result)
      ;; calculate freqs from ratios for each input-freq
      (loop for freq in freqs
	    do (push (loop for ratio in ratios
			   for new-freq = (* freq ratio)
			   when (<= min-freq new-freq max-freq)
			     collect new-freq)
		     options))
      ;; filter options
      (setf similar-options
	    (loop for freq in (car options)
		  when (every #'(lambda (ls) (member freq ls :test #'similar-freqp))
			      options)
		    collect freq))
      ;; in case similar-options is null
      (setf options (or similar-options options))
      ;;(setf options (remove-duplicates options :test #'=))
      ;; try and pick the most original frequency
      (setf similar-options
	    (loop for freq in options
		  unless (member freq
				 (if (> (length last-freqs) *min-no-repetitions*)
				     (subseq last-freqs 0 *min-no-repetitions*)
				     last-freqs)
				 :test #'similar-freqp)
		    collect freq))
      ;; again, in case similar-options is null
      (setf options (or similar-options options))
      ;; pick one
      ;; (setf result (nth (floor (* (random-relax) (length options))) options))
      (setf result (first (sort options #'<)))
      (push result last-freqs)
      (round result))))

;; *** generate-relaxing-notes
;;; Generate the note material for some relaxing music, focused on long, slow,
;;; and consonant harmony. This uses *relax-grid-mseconds* as isochronal grid-size
;;; - duration: Duration in seconds.
(defun generate-relaxing-notes (duration)
  (setf duration (round (* 1000 duration)))
  (let ((time 0)
	(note-list '())
	(active-notes '())) ; a list of notes that are playing at 'time
    (loop while (<= time duration) do
      ;; update list of active-notes
      (setf active-notes
	    (loop for note in active-notes
		  do (decf (note-time-left note) *relax-grid-mseconds*)
		  when (> (note-time-left note) 0)
		    collect note))
      ;; add harmony
      (loop for note in (add-harmony active-notes time)
	    do (push note active-notes)
	       (push note note-list))
      ;; step forward in time 
      (incf time *relax-grid-mseconds*))
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
