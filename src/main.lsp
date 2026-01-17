(in-package :liminale)

;; * relaxing...

;; ** Generating Soundfiles

(format t "~& ~&relaxing now :3~& ~&")

;; (wsound "sine-test" 1 (clm::simple-sine 0 1 214 0.5))

(wsound "liminale_long_new_rain" 2
  (progn
    (reset-liminale)
    (loop for i from 0
	  for note in (generate-relaxing-notes 1000)
	  collect (case (note-type note)
		    (pad
			  (setf (note-freq note) (/ (note-freq note) 2))
			  (dreamy-pad note i))
		    (contemplative
			  (pluck note i))
		    (noise
		     (splinter note))
		    (t )))))

(wsound "splinter-test" 1 (splinter (make-note :velocity 0 :type 'noise :duration 10000)))

(stop-playing)



;;; TODO
;;; - dickere Harmonien wenn Pluck nicht spielt? allgemein etwas höhere cahnce auf harmonien.
;;; - Pluck manchmal zu schief

;;; When sampling (for example with samp0) before passing to moog we could:
;;; - play samples longer by looping/reversing...?
;;; - possibly capable of multichannel-audio generation -> Intro-Musik NA

;; EOF liminale.lsp
