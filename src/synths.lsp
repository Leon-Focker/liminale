;; * synths

(in-package :relax)

;; ** dreamy-pad

(defun dreamy-pad (note &optional (i 0))
  (let* ((start (/ (note-start note) 1000.0))
	 (duration (/ (note-duration note) 1000.0))
	 (freq (note-freq note))
	 (sounds (or (find-sounds-with-sufficient-dur
		      duration freq *acc-samples-double*)
		     (data *acc-samples-double*)))
	 (sound (nth-mod i sounds))
	 (sound-freq (car (fundamental-frequency sound)))
	 (base-srt (/ freq sound-freq))
	 (amp-env '(0 0  5 1  95 1  100 0)))
    (append
     (loop for mult in '(1 2 3)
	   append (clm::moog
		   (path sound)
		   start
		   :amp 0.2
		   :amp-env amp-env
		   :moog t
		   :srt (* base-srt mult)
		   :duration duration
		   :res-env '(0 0.5  1 0.5)
		   :freq-env `(0 0  0.7 4000  1 ,freq)
		   :freq-env-expt 8))
     (clm::sine start duration freq 0.1 :amp-env amp-env))))


;; ** pluck
(defun pluck (note &optional (i 0))
  (let* ((start (/ (note-start note) 1000.0))
	 (duration 0.5)
	 (freq (note-freq note))
	 (sounds (find-sounds-with-sufficient-dur
		  duration freq *acc-samples-basic*))
	 (sound (nth-mod i sounds))
	 (sound-freq (car (fundamental-frequency sound)))
	 (base-srt (/ freq sound-freq))
	 (amp-env '(0 1  100 0)))
    (append
     (loop for mult in '(1 2 3)
	   append (clm::moog
		   (path sound)
		   start
		   :start 1
		   :amp-env-expt 4
		   :amp 0.2
		   :amp-env amp-env
		   :moog t
		   :srt (* base-srt mult)
		   :duration duration
		   :res-env '(0 0.5  1 0.5)
		   :freq-env '(0 0  1 4000  100 0)
		   :freq-env-expt 8))
     #+nil(clm::sine start duration freq 0.1 :amp-env amp-env))))

;; ** splinter
(defun splinter (file time)
  (clm::splinter file time))


;; EOF synths.lsp
