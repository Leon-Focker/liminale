;; * synths

(in-package :liminale)

;; ** Sounds

#+nil(defparameter *acc-samples*
  (let ((text-file (liminale-path "acc-samples.txt")))
    (if (probe-file text-file)
	(load-from-file text-file)
	(let ((sp (soundpile-from-folder
		   'accordion "/E/liminale/acc_samples/" :analyse t)))
	  (store-in-text-file sp text-file)
	  sp))))

(defparameter *acc-samples-double*
  (let ((text-file (liminale-path "acc-samples-double.txt")))
    (if (probe-file text-file)
	(load-from-file text-file)
	(let ((sp (soundpile-from-folder
		   'accordion "/E/liminale/samples/double/" :analyse t)))
	  (store-in-text-file sp text-file)
	  sp))))

(defparameter *acc-samples-basic*
  (let ((text-file (liminale-path "acc-samples-basic.txt")))
    (if (probe-file text-file)
	(load-from-file text-file)
	(let ((sp (soundpile-from-folder
		   'accordion "/E/liminale/samples/basic/" :analyse t)))
	  (store-in-text-file sp text-file)
	  sp))))

;; ** dreamy-pad

(defun dreamy-pad (note &optional (i 0))
  (let* ((start (/ (note-start note) 1000.0))
	 (duration (/ (note-duration note) 1000.0))
	 (freq (note-freq note))
	 (soundpile (if (< (random-liminale) (note-velocity note))
			*acc-samples-double*
			*acc-samples-basic*))
	 (longest (first (sort (copy-seq (data soundpile)) #'>
			       :key #'duration)))
	 (sounds (or (find-sounds-with-sufficient-dur
		      duration freq soundpile)
		     (list longest)))
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
  (let* ((amp (/ (note-velocity note) 4))
	 (start (/ (note-start note) 1000.0))
	 (duration 0.5)
	 (freq (note-freq note))
	 (sounds (find-sounds-with-sufficient-dur
		  duration freq *acc-samples-basic*))
	 (sound (nth-mod i sounds))
	 (sound-freq (car (fundamental-frequency sound)))
	 (base-srt (/ freq sound-freq))
	 (amp-env '(0 1  100 0)))
  (clm::sound-let
      ((moog-sound (:statistics nil)
		   (loop for mult in '(1 2 3)
			 append
			 (append
			  (clm::moog
			   (path sound)
			   0
			   :start 1
			   :amp-env-expt 4
			   :amp amp
			   :amp-env amp-env
			   :moog t
			   :srt (* base-srt mult)
			   :duration duration
			   :res-env '(0 0.5  1 0.5)
			   :freq-env `(0 0  1 ,(* freq 3)  1000 0)
			   :freq-env-expt 8)
			  (clm::sine 0 duration (* mult freq) amp :amp-env amp-env)))))
    (clm::simple-echo moog-sound start
		      :delay (note-delay-time note)
		      :feedback 0.7))))

;; ** splinter
(defun splinter (note)
  (let* ((start (/ (note-start note) 1000.0))
	 (duration (/ (note-duration note) 1000.0))
	 (vel (note-velocity note))
	 (file1 "/E/liminale/samples/hagel.wav")
	 (file2 "/E/liminale/samples/regen.wav")
	 (file3 "/E/liminale/samples/pinknoise.wav"))
    (clm::sound-let
	((splint (:statistics nil)
		 (loop for file in (list file1 file2 file3)
		       for amp in (list vel (- 1.0 vel) 0.05)
		       append 
		       (clm::splinter file 0
				      :channel 0
				      :duration duration
				      :grain-env (list 0 (dry-wet 5 0.3 vel)  100 (dry-wet 5 0.3 vel))
				      :center-deviation-env '(0 1  100 1)
				      :voices 10
				      :srt 0.8
				      :amp amp
				      :ramp 1000
				      :silence-env '(0 1  40 0  60 0.3  100 1)
				      :amp-env '(0 0  30 1  70 1  100 0)
				      :amp-env-base 0.5
				      ))))
      (clm::moog splint start
		 :amp (+ 0.1 (* 0.2 vel))
					; :amp-env amp-env
		 :moog t
		 :duration duration
		 :res-env '(0 0 1 0)
		 :freq-env '(0 1000  40 5000  60 5000  100 1000)
		 :freq-env-expt 8))))

;; EOF synths.lsp
