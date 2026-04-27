;; * helpers.lsp

(in-package :liminale)

;; ** randomness

(let* ((seed 5)
       (rn (make-random-number seed)))
  
  (defun random-liminale ()
    (get-next rn))
  
  (defun reset-random-liminale ()
    (setf rn (make-random-number seed))
    (random-liminale))
    
  (defun set-liminale-seed (new-seed)
    (setf seed new-seed)
    (reset-random-liminale)))


;; ** simple ringbuffer

(defstruct (simple-ringbuffer
	    (:constructor make-simple-ringbuffer
		(size
		 &key init-element
		 &aux (data (make-array size :initial-element init-element)))))
  (data)
  (size 1 :type integer)
  (idx 0 :type integer))

;; *** srb-get-all
;;; get all values in the ringbuffer, starting with the most recent one
(defun srb-get-all (srb)
  (loop for idx from (1+ (simple-ringbuffer-idx srb))
	repeat (simple-ringbuffer-size srb)
	collect (aref (simple-ringbuffer-data srb)
		      (mod idx (simple-ringbuffer-size srb)))))

(defun srb-add-value (srb value)
  (setf (aref (simple-ringbuffer-data srb) (simple-ringbuffer-idx srb))
	value
	(simple-ringbuffer-idx srb)
	(mod (1+ (simple-ringbuffer-idx srb)) (simple-ringbuffer-size srb))))

(let ((test (make-simple-ringbuffer 4)))
  (srb-add-value test 8)
  (srb-add-value test 9)
  (srb-add-value test 10)
  (srb-add-value test 11)
  (srb-add-value test 12)
  (srb-get-all test))


;; ** musical helpers

;; *** first-n
;;; subseq without out of bounds
(defun first-n (sequence n)
  (let ((len (length sequence)))
    (subseq sequence 0 (min len n))))

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

;; *** scale-to-log
;;; Scale number x on a logarithmic scale from min to max.
(defun scale-to-log (x min max)
  (let ((log-min (log min))
        (log-max (log max)))
    (exp (+ log-min (* x (- log-max log-min))))))

;; *** scale-until-in-range
;;; Divide or multiply a value by factor until it is in between min and max
(defun scale-until-in-range (value min max &optional (factor 2))
  (if (<= min value max)
      value
      (if (> value max)
	  (scale-until-in-range (/ value factor) min max)
	  (scale-until-in-range (* value factor) min max))))

;; *** prefer-first-options
(defun prefer-first-options (ls)
  (let ((weights (reverse (loop for i from 1 repeat (length ls) collect i))))
    (nth (decider (random-liminale) weights) ls)))

;; *** filter-similar-options
;;; Get a list of lists of options and find options, that appear in as many of
;;; those lists as possible. The Latter options-lists are prefered, the earlier
;;; ones discarded first.
(defun filter-similar-options (options-lists)
  (when options-lists
    (let ((result '()))
      (loop for freq in (car options-lists)
	    when (every #'(lambda (ls) (member freq ls :test #'similar-freqp))
			options-lists)
	      do (push freq result))
      (or result (filter-similar-options (cdr options-lists))))))

;; *** pick-original-options
;;; Given a list of last picks, try and find options that haven't recently been
;;; picked.
(defun pick-original-options (options last-picks
			      &optional (test #'similar-freqp))
  (unless options
    (error "pick-original-option: no options!"))
  (let ((result '()))
    (loop for freq in options
	  unless (member freq last-picks :test test)
	    do (push freq result))
    (or result
	(pick-original-options
	 options
	 (subseq last-picks 0 (1- (length last-picks)))))))

;; *** find-sounds-with-sufficient-dur
;;; Find sounds in a soundpile that are long enough to be played at freq for dur
(defun find-sounds-with-sufficient-dur (dur freq soundpile)
  (loop for sound in (data soundpile)
	for sound-dur = (duration sound)
	for required-dur = (* (/ freq (car (fundamental-frequency sound))) dur)
	when (>= sound-dur required-dur) collect sound))


;; ** CLM

(defmacro wsound (name out-channels &body body)
  `(with-sound (:header-type clm::mus-riff :srate 48000
		:output (format nil "~a~a" (liminale-path ,name) ".wav")
		:channels ,out-channels :play nil :scaled-to 0.98
		:force-recomputation nil :statistics nil)
     ,@body))

;; additionally convert the .wav file to .mp3 using ffmpeg in the terminal
(defmacro wsoundmp3 (name out-channels &body body)
  `(let ((outwav (format nil "~a~a" (liminale-path ,name) ".wav"))
	 (outmp3 (format nil "~a~a" (liminale-path ,name) ".mp3")))
     (with-sound (:header-type clm::mus-riff :srate 48000
		  :output outwav
		  :channels ,out-channels :play nil :scaled-to 0.98
		  :force-recomputation nil :statistics nil)
       ,@body)
     #+unix(cl-user::run-program
	    "/usr/bin/ffmpeg"
	    (list "-i" outwav "-y" outmp3)
	    :output ,*standard-output*)))

;; EOF helpers.lsp
