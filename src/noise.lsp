;; * noise

(in-package :liminale)

(define-note-type :noise
  :min-freq 110
  :max-freq 1234
  :min-duration 120000
  :max-duration 300000
  )

;; *** generate-new-notes
;;; Given a list of currently playing noisy notes, which notes should 
;;; additionally start to play? Return additional notes as a list.
(let ((last-played? t))
  (defmethod generate-new-notes ((type (eql :noise)) time
				 &key active-notes
				 &allow-other-keys)
    (when (<= time 0) (setf last-played? t))
    (let ((note-list (remove-if-not #'is-noise active-notes)))
      (when (null note-list)
	(prog1 (list
		(make-note :freq 0
			   :start time
			   :duration (get-new-duration :noise)
			   :type :noise
			   :velocity (if last-played?
					 0
					 (+ 0.5 (* 0.5 (random-liminale))))))
	  (setf last-played? (not last-played?)))))))

;; *** get-new-duration
(defmethod get-new-duration ((type (eql :noise)))
  (get-new-duration-aux type 30))

;; EOF noise.lsp
