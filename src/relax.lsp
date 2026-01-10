(in-package :relax)

;; * relaxing...

;; ** Globals

(defparameter *acc-samples*
  (let ((text-file (relax-path "acc-samples.txt")))
    (if (probe-file text-file)
	(load-from-file text-file)
	(let ((sp (soundpile-from-folder
		   'accordion "/E/relax/acc_samples/" :analyse t)))
	  (store-in-text-file sp text-file)
	  sp))))

(defparameter *acc-samples-double*
  (let ((text-file (relax-path "acc-samples-double.txt")))
    (if (probe-file text-file)
	(load-from-file text-file)
	(let ((sp (soundpile-from-folder
		   'accordion "/E/relax/samples/double/" :analyse t)))
	  (store-in-text-file sp text-file)
	  sp))))

(defparameter *acc-samples-basic*
  (let ((text-file (relax-path "acc-samples-basic.txt")))
    (if (probe-file text-file)
	(load-from-file text-file)
	(let ((sp (soundpile-from-folder
		   'accordion "/E/relax/samples/basic/" :analyse t)))
	  (store-in-text-file sp text-file)
	  sp))))

;; ** Generating Soundfiles

(format t "~& ~&relaxing now :3~& ~&")

(wsound "sine-test" 1 (clm::simple-sine 0 1 214 0.5))

(wsound "moog-test" 2
  (progn
    (reset-relax)
    (loop for i from 0
	  for note in (generate-relaxing-notes 20)
	  collect (case (note-type note)
		    (pad
		     (setf (note-freq note) (/ (note-freq note) 2))
		     (dreamy-pad note i))
		    (contemplative
		     (pluck note i))
		    (t )))))

(stop-playing)

;;; TODO
;;; - modulate pluck velocity
;;; - noise layer with splinter
;;; - maybe modulate pluck start times?

;; EOF relax.lsp
