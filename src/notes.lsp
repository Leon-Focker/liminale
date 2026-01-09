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
  (type 'pad))                          ; which role this note plays


;; ** generation

;; *** add-pad-harmony
;;; Given a list of currently playing pad notes, which notes should additionally
;;; start to play? Return additional notes as a list.
(defun add-pad-harmony (note-list &optional (time 0))
  (unless (every #'is-pad note-list)
    (error "get-pad-harmony: I want only pad notes!"))
  (let (new-note)
    (case (length note-list)
      (0 (setf
	  new-note
	  (make-note :start time
		     :duration (get-pad-duration)
		     :type 'pad
		     :freq (get-new-pad-frequency)))
       (cons new-note (add-pad-harmony (list new-note) time)))
      ((1 2) (setf
	      new-note
	      (make-note :start time
			 :duration (get-pad-duration)
			 :type 'pad
			 :freq (apply #'get-new-pad-frequency
				      (mapcar #'note-freq note-list))))
       (cons new-note (add-pad-harmony (cons new-note note-list) time)))
      (3 (when (> (random-relax) 0.8)
	   (list
	    (make-note :start time
		       :duration (get-pad-duration)
		       :type 'pad
		       :freq (apply #'get-new-pad-frequency
				    (mapcar #'note-freq note-list)))))))))

;; *** add-contemplative
;;; Given a list of currently playing pad notes, add some contemplative notes.
(defun add-contemplative (note-list &optional (time 0))
  (let ((contemplative-notes (remove-if-not #'is-contemplative note-list)))
    (when (null contemplative-notes)
      (list
       (make-note :start time
		  :duration (get-contemplative-duration)
		  :type 'contemplative
		  :freq (apply #'get-new-contemplative-frequency
			       (mapcar #'note-freq note-list)))))))

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

;; *** is-pad
(defun is-pad (note)
  (equal 'pad (note-type note)))

;; *** is-contemplative
(defun is-contemplative (note)
  (equal 'contemplative (note-type note)))

;; *** find-sounds-with-sufficient-dur
;;; Find sounds in a soundpile that are long enough to be played at freq for dur
(defun find-sounds-with-sufficient-dur (dur freq soundpile)
  (loop for sound in (data soundpile)
	for sound-dur = (duration sound)
	for required-dur = (* (/ freq (car (fundamental-frequency sound))) dur)
	when (>= sound-dur required-dur) collect sound))

;; *** get-durations

;;; Some (possibly user-defined) values that guide the duration-selection.
(defparameter *relax-grid-mseconds* 100)
(defparameter *min-no-repetitions* 5)
(defparameter *min-duration-con* 200)
(defparameter *max-duration-con* 600)
(defparameter *min-duration-con-pause* 5000)
(defparameter *max-duration-con-pause* 15000)
(defparameter *min-duration-pad* 5000)
(defparameter *max-duration-pad* 30000)

;;; Functions for duration selection follow here
(let ((last-pad '())
      (last-con '(500)))

  (defun reset-last-durations ()
    (setf last-pad '())
    (setf last-con '(500)))

   ;;; get a duration for the pad sounds, mostly random.
  (defun get-pad-duration ()
    (get-duration-aux
     last-pad #'(lambda (x) (push x last-pad))
     *min-duration-pad* *max-duration-pad*))
  
  ;;; get a duration for the contemplative sounds.
  (defun get-contemplative-duration ()
    (let* ((last-dur (car last-con))
	   (nr-of-reps (length (loop for i in last-con while (= i last-dur)
				     collect i))))
      (if (>= last-dur *min-duration-con-pause*)
	  (get-duration-aux
	   last-con #'(lambda (x) (push x last-con))
	   *min-duration-con* *max-duration-con*)
	  (if (< (random-relax) (* nr-of-reps 0.1))
	      (get-duration-aux
	       last-con #'(lambda (x) (push x last-con))
	       *min-duration-con-pause* *max-duration-con-pause*)
	      (progn (push last-dur last-con) last-dur))))))

;;; Aux-function for getting a random but original duration between min-dur
;;; and max-dur. last-ls is a list of durations to not chose, setter-fn should
;;; be a lambda-function which adds the new value to a list.
(defun get-duration-aux (last-ls setter-fn min-dur max-dur)
  (let ((min-mult (ceiling min-dur *relax-grid-mseconds*))
	(max-mult (floor max-dur *relax-grid-mseconds*)))
    (loop for random-nr = (random-nldd 0.8 (random-relax))
	  for mult = (round (scale-to-log random-nr min-mult max-mult))
	  for new-dur = (* mult *relax-grid-mseconds*)
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
(defparameter +max-freq+ 1200)

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
     freqs +pad-ratios+))
  
  ;;; get a frequency for the contemplative sounds
  (defun get-new-contemplative-frequency (&rest freqs)
    (get-new-frequency-aux
     last-con-freqs #'(lambda (x) (push x last-con-freqs))
     freqs +con-ratios+
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
(defun get-new-frequency-aux (last-ls setter-fn freqs ratios
			      &optional (picking-fn #'first))
  (let ((options '())
	(similar-options '())
	result)
    (unless freqs (push (car last-ls) freqs))
    ;; calculate freqs from ratios for each input-freq
    (loop for freq in freqs
	  do (push (loop for ratio in ratios
			 for new-freq = (* freq ratio)
			 when (<= +min-freq+ new-freq +max-freq+)
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
      ;; add new notes
      (let* ((active-pad-notes (remove-if-not #'is-pad active-notes))
	     (new-pad-notes (add-pad-harmony active-pad-notes time))
	     (new-contemplative (add-contemplative active-notes time)))
	(mapcar #'(lambda (note) (push note active-notes) (push note note-list))
		(append new-pad-notes new-contemplative)))
      ;; step forward in time 
      (incf time *relax-grid-mseconds*))
    ;; vary start-times
    ;; TODO only for short sounds, not pad?
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
