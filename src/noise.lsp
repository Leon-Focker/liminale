;; * noise

(in-package :liminale)

(define-note-type :noise
  :min-freq 110
  :max-freq 1234
  :min-duration 200000
  :max-duration 420000
  :last-played? t)

;; *** get-new-duration
(defmethod get-new-duration ((type (eql :noise)) &rest durs)
  (get-new-duration-aux type durs #'first 30))

;; *** get-new-note
(defmethod get-new-note ((type (eql :noise)) time
			 &key &allow-other-keys)
  (make-note :start time
	     :type type
	     :freq 0
	     :duration (get-new-duration type)
	     :velocity (if (last-played? type) 0 (+ 0.5 (* 0.5 (random-liminale))))))

;; *** generate-new-notes
;;; Get a new note every time no noise note is playing
(defmethod generate-new-notes ((type (eql :noise)) time
			       &rest keys
			       &key active-notes
			       &allow-other-keys)
  (let ((note-list (remove-if-not #'is-noise active-notes)))
    (when (null note-list)
      (prog1 (list (apply #'get-new-note type time keys))
	(setf (last-played? type) (not (last-played? type)))))))

;; *** reset
(defmethod reset-note-type :after ((type (eql :noise)) &key &allow-other-keys)
  (setf (last-played? type) t))


;; EOF noise.lsp
