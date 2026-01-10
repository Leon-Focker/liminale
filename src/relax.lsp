(in-package :relax)

;; * relaxing...

;; ** Globals

(setf clm::*srate* 48000)

;; ** Generating Soundfiles

(format t "~& ~&relaxing now :3~& ~&")

(wsound "sine-test" 1 (clm::simple-sine 0 1 214 0.5))

(wsound "moog-test" 2
  (progn
    (reset-relax)
    (loop for i from 0
	  for note in (generate-relaxing-notes 10)
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
;;; - modulate soundpile / Sample-Auswahl

;;; - possibly capable of multichannel-audio generation -> Intro-Musik NA

;; EOF relax.lsp
