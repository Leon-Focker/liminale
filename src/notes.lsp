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
  (type 'pad)                           ; which role this note plays
  (delay-time 0.5 :type number))        ; delay time, only applies to some sounds


;; ** generation

;; *** add-pad-harmony
;;; Given a list of currently playing pad notes, which notes should additionally
;;; start to play? Return additional notes as a list.
(defparameter *pad-velocity-period* 300) ; in seconds

(let ((vel-mod (get-sine-modulator *pad-velocity-period* (* pi 1/2))))
  (defun add-pad-harmony (note-list &optional (time 0))
    (unless (every #'is-pad note-list)
      (error "add-pad-harmony: I want only pad notes!"))
    (let ((new-note
	    (make-note :start time
		       :duration (get-pad-duration)
		       :type 'pad
		       :velocity (get-mod-value vel-mod time))))
      (case (length note-list)
	(0
	 (setf (note-freq new-note) (get-new-pad-frequency))
	 (cons new-note (add-pad-harmony (list new-note) time)))
	((1 2 3 4 5) 
	 (setf (note-freq new-note)
	       (apply #'get-new-pad-frequency (mapcar #'note-freq note-list)))
	 (cons new-note (add-pad-harmony (cons new-note note-list) time)))
	(6
	 ;; TODO this probability could depend on grid-size
	 (when (> (random-liminale) 0.8)
	   (setf (note-freq new-note)
		 (apply #'get-new-pad-frequency (mapcar #'note-freq note-list)))
	   (list new-note)))
	(7
	 (when (and (< (get-mod-value vel-mod time) 0.2) (> (random-liminale) 0.6))
	   (setf (note-freq new-note)
		 (apply #'get-new-pad-frequency (mapcar #'note-freq note-list)))
	   (list new-note)))))))

;; *** add-contemplative
;;; Given a list of currently playing pad notes, add some contemplative notes.
(defparameter *conteplative-velocity-period* 100) ; in seconds

(let ((vel-mod (get-cut-off-sine-modulator *conteplative-velocity-period* 0.3 0.2)))
  (defun add-contemplative (note-list &optional (time 0))
    (let ((contemplative-notes (remove-if-not #'is-contemplative note-list)))
      (when (null contemplative-notes)
	(let ((duration (get-contemplative-duration)))
	  (list
	   (make-note :start time
		      :duration duration
		      :type 'contemplative
		      :velocity (get-mod-value vel-mod time) ; between 0.0 and 0.5
		      :delay-time (scale-until-in-range (/ duration 1000) 0.2 0.45 3)
		      :freq (apply #'get-new-contemplative-frequency
				   (mapcar #'note-freq note-list)))))))))

;; *** add-noise
;;; Given a list of currently playing noisy notes, which notes should additionally
;;; start to play? Return additional notes as a list.
(defun add-noise (note-list &optional (time 0))
  (unless (every #'is-noise note-list)
    (error "add-noise: I want only noise notes!"))
  (let ((new-note
	  (make-note :start time
		     :duration (get-noise-duration)
		     :type 'noise
		     :velocity (random-liminale))))
    (case (length note-list)
      (0 (list new-note))
      ;; TODO this probability could depend on grid-size
      (1 (when (> (random-liminale) 0.9)
	   (list new-note))))))

;; *** is-pad
(defun is-pad (note)
  (equal 'pad (note-type note)))

;; *** is-contemplative
(defun is-contemplative (note)
  (equal 'contemplative (note-type note)))

;; *** is-noise
(defun is-noise (note)
  (equal 'noise (note-type note)))

;; *** get-durations

;;; Some (possibly user-defined) values that guide the duration-selection.
(defparameter *liminale-grid-mseconds* 100)
(defparameter *min-no-repetitions* 5)
(defparameter *min-duration-con* 200)
(defparameter *max-duration-con* 600)
(defparameter *min-duration-con-pause* 5000)
(defparameter *max-duration-con-pause* 15000)
(defparameter *min-duration-pad* 10000)
(defparameter *max-duration-pad* 40000)
(defparameter *min-duration-noise* 25000)
(defparameter *max-duration-noise* 60000)

;;; Functions for duration selection follow here
(let ((last-pad '())
      (last-con '(500))
      (last-noise '()))

  (defun reset-last-durations ()
    (setf last-pad '())
    (setf last-con '(500)))

   ;;; get a duration for the pad sounds, mostly random.
  (defun get-pad-duration ()
    (get-duration-aux
     last-pad #'(lambda (x) (push x last-pad))
     *min-duration-pad* *max-duration-pad*))

  ;;; get a duration for the noise sounds, mostly random.
  (defun get-noise-duration ()
    ;; lets only have, say, 20 different options for length:
    (let* ((nr-options 10)
	   (len-diff (- *max-duration-noise* *min-duration-noise*))
	   (factor (/ len-diff *liminale-grid-mseconds*
		      (max nr-options (1+ *min-no-repetitions*)))))
      (* (get-duration-aux
	  last-noise #'(lambda (x) (push x last-noise))
	  (floor *min-duration-noise* factor)
	  (floor *max-duration-noise* factor))
	 factor)))
  
;;; get a duration for the contemplative sounds.
    (defun get-contemplative-duration ()
      (let* ((last-dur (car last-con))
	     (nr-of-reps (length (loop for i in last-con while (= i last-dur)
				       collect i))))
	(if (>= last-dur *min-duration-con-pause*)
	    (get-duration-aux
	     last-con #'(lambda (x) (push x last-con))
	     *min-duration-con* *max-duration-con*)
	    (if (< (random-liminale) (* nr-of-reps 0.1))
		(get-duration-aux
		 last-con #'(lambda (x) (push x last-con))
		 *min-duration-con-pause* *max-duration-con-pause*)
		(progn (push last-dur last-con) last-dur))))))

;;; Aux-function for getting a random but original duration between min-dur
;;; and max-dur. last-ls is a list of durations to not chose, setter-fn should
;;; be a lambda-function which adds the new value to a list.
(defun get-duration-aux (last-ls setter-fn min-dur max-dur)
  (let ((min-mult (ceiling min-dur *liminale-grid-mseconds*))
	(max-mult (floor max-dur *liminale-grid-mseconds*)))
    (loop for random-nr = (random-nldd 0.8 (random-liminale))
	  for mult = (round (scale-to-log random-nr min-mult max-mult))
	  for new-dur = (* mult *liminale-grid-mseconds*)
	  while (remove-if-not #'(lambda (x) (similar-durp x new-dur))
			       (subseq last-ls 0 (min (length last-ls)
						      *min-no-repetitions*)))
	  finally (progn
		    (funcall setter-fn new-dur)
		    (return new-dur)))))

;; *** filter-similar-options
(defun filter-similar-options (options)
    (let ((result '()))
      (loop for freq in (car options)
	    when (every #'(lambda (ls) (member freq ls :test #'similar-freqp))
			options)
	      do (push freq result))
      (or result (filter-similar-options (cdr options)))))

;; *** pick-original-freqs
(defun pick-original-freqs (options last-freqs)
  (unless options
    (error "pick-original-freqs: no options!"))
  (let ((result '()))
    (loop for freq in options
	  unless (member freq last-freqs :test #'similar-freqp)
	    do (push freq result))
    (or result
	(pick-original-freqs
	 options
	 (subseq last-freqs 0 (1- (length last-freqs)))))))

;; *** get-new-frequency
;;; TODO make these constants later
(defparameter +pad-ratios+
  (append '(1/2 2/3 3/4)		; 4/5 5/6 6/7 7/8)
	  '(2 3/2 4/3 5/4)		; 6/5 7/6 8/7)
	  '(3 2 5/3 3/2)		; 7/5 4/3 9/7)
	  '(1/3 1/2 3/5 2/3)		; 5/7 3/4 7/9)
	  '(4 5)))

(defparameter +con-ratios+
  (append '(1 4/3 3/2 8/5 5/3 12/7 7/4)
	  '(2 3/2 4/3 5/4 6/5 7/6 8/7)
	  '(3 2 5/3 3/2 7/5 4/3 9/7)
	  '(2/3 1 6/5 4/3 10/7 3/2 14/9)
	  '(4 5 6 7 8 9 10 11 12 13 14 15)))

(defparameter +min-freq+ 40)
(defparameter +max-freq+ 600)

;;; Functions for frequency selection follow here
(let ((last-pad-freqs '(528))
      (last-con-freqs '(528)))

  (defun reset-last-freqs ()
    (setf last-pad-freqs '(528))
    (setf last-con-freqs '(528)))
  
  ;;; get a frequency for the pad sounds 
  (defun get-new-pad-frequency (&rest freqs)
    (get-new-frequency-aux
     last-pad-freqs #'(lambda (x) (push x last-pad-freqs))
     freqs +pad-ratios+ +min-freq+ +max-freq+))
  
  ;;; get a frequency for the contemplative sounds
  (defun get-new-contemplative-frequency (&rest freqs)
    ;; Sorting them by #'< means that lower freqs are more important in
    ;; #'filter-similar-options
    (setf freqs (sort freqs #'<))
    (get-new-frequency-aux
     last-con-freqs #'(lambda (x) (push x last-con-freqs))
     freqs +con-ratios+ +min-freq+ (* 3 +max-freq+)
     #'(lambda (ls) (or (find (car last-con-freqs) ls :test #'(lambda (x y) (<= x y)))
		   (first ls))))))

;;; Aux-function for getting a frequency that fits a list of other frequencies.
;;; - last-ls: a list of freqs to avoid.
;;; - setter-fn: a function that adds the new value to a list.
;;; - freqs: list of other frequencies
;;; - ratios: a list of ratios that the new frequency should be compared to the
;;;   freqs in freqs.
;;; - picking-fn: a list of options will be generated, sorted from lowest to
;;;   highest. This function will be called to select one of these frequencies.
;;;   If none is provided, the first is chosen.
(defun get-new-frequency-aux (last-ls setter-fn freqs ratios min-freq max-freq
			      &optional (picking-fn #'first))
  (let ((options '())
	(similar-options '())
	result)
    (unless freqs (push (car last-ls) freqs))
    ;; calculate freqs from ratios for each input-freq
    (loop for freq in freqs
	  do (push (loop for ratio in ratios
			 for new-freq = (* freq ratio)
			 when (<= min-freq new-freq max-freq)
			   collect new-freq)
		   options))
    ;; filter options
    (setf similar-options (filter-similar-options options))
    ;; try and pick the most original frequency
    (setf similar-options
	  (pick-original-freqs
	   similar-options
	   (subseq last-ls 0 (min (length last-ls) *min-no-repetitions*))))
    ;; pick one
    (setf result (funcall picking-fn (sort similar-options #'<)))
    (funcall setter-fn result)
    (round result)))

;; *** generate-liminaleing-notes
;;; Generate the note material for some relaxing music, focused on long, slow,
;;; and consonant harmony. This uses *liminale-grid-mseconds* as isochronal grid-size
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
		  do (decf (note-time-left note) *liminale-grid-mseconds*)
		  when (> (note-time-left note) 0)
		    collect note))
      ;; add new notes
      (let* ((active-pad-notes (remove-if-not #'is-pad active-notes))
	     (new-pad-notes (add-pad-harmony active-pad-notes time))
	     (new-contemplative (add-contemplative active-notes time)))
	(mapcar #'(lambda (note) (push note active-notes) (push note note-list))
		(append new-pad-notes new-contemplative)))
      ;; step forward in time 
      (incf time *liminale-grid-mseconds*))
    ;; TODO
    ;; I want some sections to have regular rhythms (for the short sounds) and
    ;; some to be more 'arbitrary' -> shifting notes with fixed seed randomness.
    ;; Use something like (random (get-cut-off-sine-modulator (* 5 60))).
    note-list))

;; *** playing-at-time
(defun playing-at-time (list-of-notes time)
  (loop for note in list-of-notes
	for start = (note-start note)
	for end = (+ start (note-duration note))
	when (<= start time end) collect note))

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
