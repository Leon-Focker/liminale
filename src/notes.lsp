;; * notes

(in-package :liminale)

(defstruct (note
            (:constructor make-note
                (&key start duration freq velocity type delay-time
                 &aux (time-left duration))))
  (start 0 :type integer)		; in miliseconds
  (duration 0 :type integer)		; in miliseconds
  (freq 440 :type number)		; in Hz
  (velocity 0.7 :type number)		; 0-1
  (time-left 0 :type integer)           ; in miliseconds
  (type :pad))                          ; which role this note plays


;; ** generation

(defparameter *liminale-grid-mseconds* 100)

;; *** generate-notes
;;; Generate a sequence of notes for at least the given duration. Notes are
;;; generated according to their note-type. This uses *liminale-grid-mseconds*
;;; as isochronal grid-size.
;;; - duration: Duration in seconds.
;;; - note-types: A list of note-types (which can be defined with #'define-note-type)
;;;   Unknown types will just use the generic methods for generation.
;;; - reset-liminale: bool, whether to reset the random-number generator at the start.
;;; - verbose: when t, print a lot of stuff.
(defun generate-notes (duration
		       &optional (note-types '(:pad)) (reset-liminale t) verbose)
  (when reset-liminale
    (reset-random-liminale)
    (loop for type in note-types do (reset-note-type type)))
  (setf duration (round (* 1000 duration)))
  (let ((time 0)
	(note-list '())
	(active-notes '())) ; a list of notes that are playing at 'time
    (loop while (<= time duration) do
      (when verbose
	(format t "~&Generating Notes at ~ams." time))
      ;; update list of active-notes
      (setf active-notes
	    (loop for note in active-notes
		  do (decf (note-time-left note) *liminale-grid-mseconds*)
		  when (> (note-time-left note) 0)
		    collect note))
      ;; Call generation-methods for each note-type
      ;; and push new notes to note-list and active-notes.
      (loop for note-type in note-types
	    do (mapcar #'(lambda (note)
			   (push note active-notes)
			   (push note note-list))
		       (generate-new-notes
			note-type
			time
			:active-notes active-notes
			:note-list note-list
			:verbose verbose)))
      ;; step forward in time 
      (incf time *liminale-grid-mseconds*))
    note-list))

;; *** get-new-duration-aux
;;; Aux-function for getting a random but original duration between min-dur
;;; and max-dur.
(defun get-new-duration-aux (type
			     &optional
			       durs
			       (picking-fn #'first)
			       (similar-dur-percent 10))
  (let ((last-durs (first-n (get-last-durs type) (min-no-repetitions type)))
	(options '())
	(similar-options '())
	result)
    (unless durs (push (or (car last-durs) (car (get-last-durs type))) durs))
    (when (dur-ratios type)
      ;; calculate durs from ratios for each input-dur
      (loop for dur in durs
	    for derivatives
	      = (loop for ratio in (dur-ratios type)
		      for new-dur = (* dur ratio)
		      when (<= (min-duration type) new-dur (max-duration type))
			collect new-dur)
	    when derivatives do (push derivatives options))
      ;; filter options
      (setf similar-options (filter-similar-options options similar-dur-percent))
      ;; try and pick the most original frequency
      (setf
       similar-options
       (pick-original-options
	similar-options
	last-durs
	#'(lambda (x y) (similarp x y similar-dur-percent))))
      ;; pick one
      (setf result (funcall picking-fn similar-options)))
    ;; when no ratios are supplied or no matches are found, find a random dur
    (unless options
      (setf result (get-random-original-dur type last-durs similar-dur-percent)))
    (add-last-dur type result)
    (round result)))

(defun get-random-original-dur (type last-durs similar-dur-percent)
  (let ((min-mult (ceiling (min-duration type) *liminale-grid-mseconds*))
	(max-mult (floor (max-duration type) *liminale-grid-mseconds*)))
    (labels ((helper (last-durs i)
	       (let* ((random-nr (random-nldd 0.8 (random-liminale)))
		      (mult (round (scale-to-log random-nr min-mult max-mult)))
		      (new-dur (* mult *liminale-grid-mseconds*)))
		 (if (remove-if-not
		      #'(lambda (dur) (similarp dur new-dur similar-dur-percent))
		      last-durs)
		     (if (> i 100)
			 (helper (first-n last-durs (floor (length last-durs) 2)) 0)
			 (helper last-durs (1+ i)))
		     new-dur))))
      (helper last-durs 0))))

;; *** get-new-frequency-aux
;;; Aux-function for getting a frequency that fits a list of other frequencies.
;;; - freqs: A list of frequencies, for which a match is calculated
;;; - picking-fn: a list of options will be generated. This function will be
;;;   called to select one of these frequencies.
(defun get-new-frequency-aux (type
			      &optional
				freqs
				(picking-fn #'first)
				(similar-freq-percent (* 100 (1- (expt 2 1/24)))))
  (let ((last-freqs (first-n (get-last-freqs type) (min-no-repetitions type)))
	(options '())
	(similar-options '())
	result)
    (unless freqs (push (or (car last-freqs) (car (get-last-freqs type))) freqs))
    ;; calculate freqs from ratios for each input-freq
    (loop for freq in freqs
	  for derivatives
	    = (loop for ratio in (freq-ratios type)
		    for new-freq = (* freq ratio)
		    when (<= (min-freq type) new-freq (max-freq type))
		      collect new-freq)
	  when derivatives do (push derivatives options))
    ;; filter options
    (setf similar-options (filter-similar-options options similar-freq-percent))
    ;; try and pick the most original frequency
    (setf
     similar-options
     (pick-original-options
      similar-options
      last-freqs
      #'(lambda (x y) (similarp x y similar-freq-percent))))
    ;; pick one
    (setf result (funcall picking-fn similar-options))
    (add-last-freq type result)
    (round result)))

;; *** playing-at-time
;;; time in ms
(defun playing-at-time (list-of-notes time)
  (loop for note in list-of-notes
	for start = (note-start note)
	for end = (+ start (note-duration note))
	when (<= start time end) collect note))


;; ** tests

;; *** test-note-list
(defun test-note-list (note-list &optional (fn-name "test-note-list"))
  (unless (listp note-list)
    (error "~a: note-list not a list!" fn-name))
  (unless (loop for note in note-list always (equal 'note (type-of note)))
    (error "~a: not all elements are notes!" fn-name)))


;; ** midi

;; *** notes-to-midi
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
