;; * gong

(in-package :liminale)

;; ** type
 
(define-note-type :gong
  :min-freq 200
  :max-freq 783
  :min-duration 10000
  :max-duration 85000
  :min-no-repetitions 0
  :remember-n-last-notes 3
  :freq-ratios *pad-ratios*
  :vel-mod (get-cut-off-sine-modulator 400 0 0.3 0 1.42 (* pi 5/3)))

;; *** is-recent-gong
(defun is-recent-gong (note &optional (max-time 10000))
  (and (is-gong note)
       (> max-time (- (note-duration note) (note-time-left note)))))

;; *** is-recent-guitar
(defun is-recent-guitar (note &optional (max-time 10000))
  (and (is-guitar note)
       (> max-time (- (note-duration note) (note-time-left note)))))

;; *** get-new-note
(defmethod get-new-note ((type (eql :gong)) time
			 &key freqs &allow-other-keys)
  (make-note :start time
	     :type type
	     :freq (apply #'get-new-frequency type freqs)
	     :duration (get-new-duration type)
	     ;; TODO 
	     :velocity (* (get-mod-value (vel-mod type) time)
			  (random-liminale))))

;; *** generate-new-notes
;;; Given a list of currently playing notes, which notes should additionally
;;; start to play? Return additional notes as a list.
(defmethod generate-new-notes ((type (eql :gong))
			       time
			       &rest keys
			       &key active-notes
			       &allow-other-keys)
  (when (> time 60000)
    (let* ((gong-notes (remove-if-not #'is-gong active-notes))
	   (pad-notes (remove-if-not #'is-pad active-notes))
	   (guitar-notes (remove-if-not #'is-recent-guitar active-notes))
	   (harmony-stable (every (lambda (x) (> (note-time-left x) 10000))
				  pad-notes)))
      (when (and (not gong-notes)
		 (or (< (get-mod-value (vel-mod :gong) time) 0.5) (not guitar-notes))
		 harmony-stable)
	(list (apply #'get-new-note type time
		     :freqs (mapcar #'note-freq pad-notes)
		     keys))))))

;; EOF gong.lsp
