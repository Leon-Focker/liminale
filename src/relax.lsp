(in-package :relax)

;; * relaxing...

;; ** Globals

(defparameter *acc-samples* 
  (if (probe-file (relax-path "acc-samples.txt"))
    (load-from-file (relax-path "acc-samples.txt"))
    (soundpile-from-folder 'accordion "/E/relax/acc_samples/" :analyse t)))

(defparameter *acc-samples-basic* 
  (if (probe-file (relax-path "acc-samples-basic.txt"))
    (load-from-file (relax-path "acc-samples-basic.txt"))
    (soundpile-from-folder 'accordion "/E/relax/samples/basic/" :analyse t)))

(defparameter *acc-samples-double* 
  (if (probe-file (relax-path "acc-samples-double.txt"))
    (load-from-file (relax-path "acc-samples-double.txt"))
    (soundpile-from-folder 'accordion "/E/relax/samples/double/" :analyse t)))

(defparameter *relax-grid-mseconds* 100)
(defparameter *min-no-repetitions* 5)

;; ** Generating Soundfiles

(format t "~& ~&relaxing now :3~& ~&")

(wsound "sine-test" 1 (clm::simple-sine 0 1 214 0.5))

(wsound "moog-test" 2
  (progn
    (reset-relax)
    (loop for i from 0
	  for note in (generate-relaxing-notes 100)
	  collect (case (note-type note)
		    (long
		     (setf (note-freq note) (/ (note-freq note) 2))
		     (dreamy-pad note i))
		    (short
		     (pluck note i))
		    (t )))))

(wsound "moog-test" 2
  (progn
    (reset-relax)
  (loop for note in (generate-relaxing-notes 100)
	for start = (/ (note-start note) 1000.0)
	for duration = (/ (note-duration note) 1000.0)
	for freq = (note-freq note)
	for sound = (find-with-id 'acc52 *acc-samples*)
	for sound-freq = (car (fundamental-frequency sound))
	for base-srt = (/ freq sound-freq)
	collect (loop for mult in '(1 2 3)
		      append (clm::simple-sine start duration freq 0.01)))))


(stop-playing)

;; EOF relax.lsp
