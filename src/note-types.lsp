;; * note-types

(in-package :liminale)

;; ** generic methods

;; *** generic parameters

(defparameter *verbose*)
(defmethod get-min-freq (type) 1)
(defmethod get-max-freq (type) 2000)
(defmethod get-min-duration (type) 1000)
(defmethod get-max-duration (type) 10000)
(defmethod get-ratios (type) '(2 3 4 5 6 7 8 1/2 1/3 1/4 1/5 1/6 1/7 1/8))
(defmethod get-min-no-repetitions (type) 5)

;; *** generation

(defmethod generate-new-notes (type time &key &allow-other-keys) '())
;;; this :around method acts as a check for the result of all generate-new-notes
(defmethod generate-new-notes :around (type time &key &allow-other-keys)
  (when *verbose* (format t "~&Generating new ~a notes..." type))
  (let ((result (call-next-method)))
    (test-note-list result (format nil "generate-new-notes with type ~a" type))
    result))

(defmethod get-new-duration (type)
  (get-new-duration-aux type))

(defmethod get-new-duration :before (type)
  (when *verbose* (format t "~&Generating new ~a Duration" type)))

(defmethod get-new-frequency (type &rest freqs)
  (get-new-frequency-aux type freqs))

(defmethod get-new-frequency :before (type &rest freqs)
  (declare (ignore freqs))
  (when *verbose* (format t "~&Generating new ~a Frequency" type)))
  

;; ** def-note-type
;;; (def-note-type :test :min-freq 5)
(defmacro define-note-type (type-name
			    &key
			      min-freq
			      max-freq
			      min-duration
			      max-duration
			      min-no-repetitions
			      remember-n-last-notes
			      ratios
			      init-freq
			      init-dur)
  (let ((predicate-name (intern (format nil "IS-~A" type-name)))
	(last-durs (gensym))
	(last-freqs (gensym)))
    (when (and min-no-repetitions remember-n-last-notes)
      (when (> min-no-repetitions remember-n-last-notes)
	(warn "define-note-type ~a: This type needs to remember at least ~a notes"
	      type-name min-no-repetitions)))
    `(progn
       ;; this adds the is-type-name predicate for notes
       (defun ,predicate-name (note)
	 (eq ',type-name (note-type note)))
       ;; add methods for this type when the keyword-argument is supplied,
       ;; else the generic method defined above is called.
       ,(when min-freq
	  `(defmethod get-min-freq ((type (eql ,type-name))) ,min-freq))
       ,(when max-freq
	  `(defmethod get-max-freq ((type (eql ,type-name))) ,max-freq))
       ,(when min-duration
	  `(defmethod get-min-duration ((type (eql ,type-name))) ,min-duration))
       ,(when max-duration
	  `(defmethod get-max-duration ((type (eql ,type-name))) ,max-duration))
       ,(when min-no-repetitions
	  `(defmethod get-min-no-repetitions
	       ((type (eql ,type-name))) ,min-no-repetitions))
       ,(when ratios
	  `(defmethod get-ratios ((type (eql ,type-name))) ,ratios))
       ;; Getters and Setters for last durs and freqs
       (let ((,last-durs (make-simple-ringbuffer
			  (max (or ,remember-n-last-notes 0)
			       (get-min-no-repetitions ,type-name))
			  :init-element (or ,init-dur 0)))
	     (,last-freqs (make-simple-ringbuffer
			   (max (or ,remember-n-last-notes 0)
			       (get-min-no-repetitions ,type-name))
			   :init-element (or ,init-freq 528))))
	 (defmethod get-last-durs ((type (eql ,type-name))) (srb-get-all ,last-durs))
	 (defmethod add-last-dur ((type (eql ,type-name)) dur) (srb-add-value ,last-durs dur))
	 (defmethod get-last-freqs ((type (eql ,type-name))) (srb-get-all ,last-freqs))
	 (defmethod add-last-freq ((type (eql ,type-name)) freq) (srb-add-value ,last-freqs freq))
	 ;; TODO Reset
	 ))))

;; TODO!
  (defun reset-last-freqs ()
    (setf last-pad-freqs '(528))
    (setf last-con-freqs '(528)))

  (defun reset-last-durations ()
    (setf last-pad '())
    (setf last-con '(500))
    (setf last-noise '()))


;; EOF gen-types.lsp
