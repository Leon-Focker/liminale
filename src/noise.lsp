;; * noise

(in-package :liminale)

(define-note-type :noise
  :min-freq 110
  :max-freq 1234
  :min-duration 200000
  :max-duration 420000
  )

;; *** generate-new-notes
;;; Given a list of currently playing noisy notes, which notes should 
;;; additionally start to play? Return additional notes as a list.
(defmethod get-new-note ((type (eql :noise)) time
			 &key silent &allow-other-keys)
  (make-note :start time
	     :type type
	     :freq 0
	     :duration (get-new-duration type)
	     :velocity (if silent 0 (+ 0.5 (* 0.5 (random-liminale))))))

(let ((last-played? t))
  (defmethod generate-new-notes ((type (eql :noise)) time
				 &key active-notes
				 &allow-other-keys)
    (when (<= time 0) (setf last-played? t))
    (let ((note-list (remove-if-not #'is-noise active-notes)))
      (when (null note-list)
	(prog1 (list (get-new-note type time :silent last-played?))
	  (setf last-played? (not last-played?)))))))

;; *** get-new-duration
(defmethod get-new-duration ((type (eql :noise)) &rest durs)
  (get-new-duration-aux type durs 30))

;; EOF noise.lsp
