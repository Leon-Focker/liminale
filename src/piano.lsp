;; * piano

(in-package :liminale)

(setf *liminale-grid-mseconds* 70)

(define-note-type :piano-chords
  :min-freq 27
  :max-freq 2000
  :min-duration (* 8 *liminale-grid-mseconds*)
  :max-duration (* 64 *liminale-grid-mseconds*)
  :min-no-repetitions 1
  :freq-ratios (append
	   ;; start with ratios > 1
		'(2 3 4 5 6 7 8 9 10)
		'(3/2 4/3 5/4 6/5 7/6 8/7)
		'(5/3 6/4 9/7)
		'(5/2 7/4 8/5)
		'(7/3 9/5)
		'(7/2 8/3 9/4)
		;; now < 1
		'(1/2 1/3 1/4 1/5 1/6 1/7 1/8 1/9 1/10)
		'(2/3 3/4 4/5 5/6)
		'(3/5)
		'(2/5 5/8)
		'(3/7 5/9)
		'(2/7 3/8 4/9)))

(setf (dur-ratios :piano-chords)
      (append
       '(2 3 4 6 8)
       '(2/3 3/4)
       '(1/2 1/3 1/4 1/6 1/8)
       '(3/2 4/3)))

;; (append
;;  '(3 5 6 7 9 10)
;;  '(3/2 4/3 5/4 6/5 7/6 8/7 9/8)
;;  '(5/2 7/4)
;;  '(1/3 1/5 1/6 1/10)
;;  '(2/3 3/4 4/5 5/6 6/7 7/8 8/9)
;;  '(2/5 4/7))

(defmethod generate-new-notes ((type (eql :piano-chords)) time
			       &key active-notes
			       &allow-other-keys)
  (case (length active-notes)
    ((0 1 2 3 4 5 6)
     (let* ((durs (mapcar #'note-duration active-notes))
	    (freqs (mapcar #'note-freq active-notes))
 	    (new-note (make-note :start time
				 :type type
				 :duration (get-new-duration type durs)
				 :velocity 0.7
				 :freq (apply #'get-new-frequency type freqs))))
       (cons new-note
	     (generate-new-notes :piano-chords time
				 :active-notes (cons new-note active-notes)))))))

(defmethod get-new-frequency ((type (eql :piano-chords)) &rest freqs)
  ;; Sorting them by #'< means that lower freqs are more important in
  ;; #'filter-similar-options
  (setf freqs (sort freqs #'<))
  (get-new-frequency-aux type freqs #'(lambda (ls) (prefer-first-options (sort ls #'<)))))

(notes-to-midi (generate-notes 100 '(:piano-chords))
	       "/home/leon/piano-chords.mid")


;; EOF piano.lsp
