;; * gong

(in-package :liminale)

;; ** type
 
(define-note-type :gong
  :min-freq 110
  :max-freq 800
  :min-duration 20000
  :max-duration 120000
  :min-no-repetitions 0
  :remember-n-last-notes 3
  :freq-ratios *pad-ratios*)

;; *** get-new-note
(defmethod get-new-note ((type (eql :gong)) time
			 &key freqs &allow-other-keys)
  (make-note :start time
	     :type type
	     :freq (apply #'get-new-frequency type freqs)
	     :duration (get-new-duration type)
	     ;; TODO 
	     :velocity (random-liminale)))

;; *** generate-new-notes
;;; Given a list of currently playing notes, which notes should additionally
;;; start to play? Return additional notes as a list.
(defmethod generate-new-notes ((type (eql :gong))
			       time
			       &rest keys
			       &key active-notes
			       &allow-other-keys)
  (let* ((gong-notes (remove-if-not #'is-gong active-notes))
	 (pad-notes (remove-if-not #'is-pad active-notes))
	 (harmony-stable (every (lambda (x) (> (note-time-left x) 10000))
			       pad-notes)))
    (when (and (not gong-notes) harmony-stable)
      (list (apply #'get-new-note type time
		   :freqs (mapcar #'note-freq pad-notes)
		   keys)))))

;; EOF gong.lsp
