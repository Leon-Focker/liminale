(in-package :relax)

;; * relaxing...

;; ** Samples

(defparameter *acc-samples* 
      (soundpile-from-folder 'accordion "/E/relax/acc_samples/" :analyse t))

;; ** Generating Notelists

;;; duration in seconds
;;; time-step in seconds
(defun generate-relaxing-notes (duration time-step)
  (setf duration (round (* 1000 duration)))
  (setf time-step (round (* 1000 time-step)))
  (let ((time 0)	    ; in miliseconds
	(note-list '())
	(active-notes '())) ; a list of notes that are playing at 'time
    (loop while (<= time duration)
	  do
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
    note-list))

(generate-relaxing-notes 10 2)

;; ** Generating Soundfiles

(format t "~& ~&relaxing now :3~& ~&")

(wsound "this is a test" 2
  (fplay 0 100
    (soundpile *acc-samples*)
    (sound (nth-mod (random 100) (data soundpile)))
    (rhythm (random 5))
    (duration (+ 5 (random 30)))
    (amp-env '(0 1  90 1  100 0))
    (srt (+ 0.5 (random 0.1)))
    (amp (+ 0.5 (random 0.2)))
    (degree (random 90))))

(stop-playing)

(wsound "sine-test" 1 (clm::simple-sine 0 1 214 0.5))

(wsound "moog-test" 2
  (loop for note in (generate-relaxing-notes 10 2)
	for start = (/ (note-start note) 1000.0)
	for duration = (/ (note-duration note) 1000.0)
	collect  
	(append
	 (clm::moog "/E/relax/acc_samples/acc52.wav" start 
		    :amp 0.2
		    :moog t
		    :srt 1/5
		    :duration duration
		    :res-env '(0 0.5  1 0.5)
		    :freq-env '(0 0  1 4000)
		    :freq-env-expt 8)
	 (clm::moog "/E/relax/acc_samples/acc52.wav" start 
		    :amp 0.2
		    :moog t
		    :srt 2/5
		    :duration duration
		    :res-env '(0 0.5  1 0.5)
		    :freq-env '(0 0  1 4000)
		    :freq-env-expt 8)
	 (clm::moog "/E/relax/acc_samples/acc52.wav" start 
		    :amp 0.2
		    :moog t
		    :srt 3/5
		    :duration duration
		    :res-env '(0 0.5  1 0.5)
		    :freq-env '(0 0  1 4000)
		    :freq-env-expt 8))))

;; EOF relax.lsp
