(in-package :liminale)

;; * relaxing...

;; ** Generating Soundfiles

(format t "~& ~&relaxing now :3~& ~&")

;; (wsound "sine-test" 1 (clm::simple-sine 0 1 214 0.5))

(wsound "liminale" 2
  (progn
    (reset-liminale)
    (loop for i from 0
	  for note in (generate-relaxing-notes 300)
	  collect (case (note-type note)
		    (pad
		     (setf (note-freq note) (/ (note-freq note) 2))
		     (dreamy-pad note i))
		    (contemplative
		     (pluck note i))
		    (t )))))

(stop-playing)



;;; TODO
;;; - noise layer with splinter
;;; - maybe modulate pluck start times?

;;; - possibly capable of multichannel-audio generation -> Intro-Musik NA

;; EOF liminale.lsp
