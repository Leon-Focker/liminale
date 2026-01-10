;; * helpers.lsp

(in-package :relax)

;; ** reset

(defun reset-relax ()
  (reset-random-relax)
  (reset-last-durations)
  (reset-last-freqs))

;; ** randomness

(let* ((seed 5)
       (rn (make-random-number seed)))
  
  (defun random-relax ()
    (get-next rn))
  
  (defun reset-random-relax ()
    (setf rn (make-random-number seed))
    (random-relax))
    
  (defun set-relax-seed (new-seed)
    (setf seed new-seed)
    (reset-random-relax)))

;; ** musical helpers

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
		:output (format nil "~a~a" (relax-path ,name) ".wav")
		:channels ,out-channels :play t :scaled-to 0.98
		:force-recomputation nil :statistics nil)
     ,@body))

;; EOF helpers.lsp
