(in-package :liminale)

;; * relaxing...

;; ** Generating Soundfiles

(format t "~& ~&relaxing now :3~& ~&")

(setf *liminale-grid-mseconds* 100)

;; (wsound "sine-test" 1 (clm::simple-sine 0 0.1 214 0.1))

;; test ratios
(let ((ratio 23)
      (freq 200))
  (wsound "sine-test" 1
    (append (clm::simple-sine 0 1 freq 0.5)
	    (clm::simple-sine 0 1 (* ratio freq) 0.5)))
  (wsound "pad-test" 1
    (append (dreamy-pad (make-note :freq freq :velocity 0.7 :duration 10000))
	    (pluck (make-note :freq (* ratio freq) :velocity 0.7 :duration 10000)))))

(progn
  (wsoundmp3 "liminale_test_new" 2
    (progn
      (loop for i from 0
	    for note in (generate-notes 100 '(:pad :contemplative :noise))
	    collect (case (note-type note)
		      (:pad
		       (setf (note-freq note) (/ (note-freq note) 2))
		       (dreamy-pad note i))
		      (:contemplative
		       (pluck note i))
		      (:noise
		       (splinter note))
		      (t )))))
  (uiop:run-program
   "/usr/bin/scp -P42 /home/leon/quicklisp/local-projects/liminale/liminale_test.mp3 leon@172.16.0.5:/home/Drive/Sonstiges/"
   :output *standard-output*))


(wsound "moog-test3" 1 (dreamy-pad (make-note :freq 350 :velocity 0.7 :duration 100000)))

;; (wsound "splinter-test" 1 (splinter (make-note :velocity 0 :type 'noise :duration 10000)))

(stop-playing)

;;; TODO
;;;
;;; - clearer Code and Doc for modulations happening
;;; - schöne Pad Harmonie, nur selten Tiefenmarsch: Vllt Limit für Noten unter ~100hz?
;;;
;;; Conts: allgemein manchmal noch zu schief!
;;; - sollten letzte freqs mit einbeziehen, da diese noch Echo haben können. Wie viele?
;;; - lowpass, wenn leiser?
;;; - max freq?

;;; IDEAS
;;;
;;; - add capability of multichannel-audio generation
;;;

;; EOF liminale.lsp
