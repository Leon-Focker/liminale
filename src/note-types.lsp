;; * note-types

(in-package :liminale)

;; ** generic methods

;; *** generic parameters

(defmacro define-generics-for-note-types (&rest generic-values)
  `(progn
     ,@(loop for (keyword value) on generic-values by #'cddr
	     for getter-name = (intern (format nil "~A" keyword))
	     collect `(defmethod ,getter-name (type) ,value)
	     collect `(defmethod (setf ,getter-name) (new-value type)
			(error "Setf ~a: ~a~a~a~a~a"
			       ',getter-name
			       "You're trying to set the generic value, "
			       ',keyword
			       " was not defined for type "
			       type
			       "!")))))
  
(define-generics-for-note-types
    :min-freq 1
  :max-freq 2000
  :min-duration 1000
  :max-duration 10000
  :min-no-repetitions 1
  :remember-n-last-notes 1
  :freq-ratios '(2 3 4 5 6 7 8 1/2 1/3 1/4 1/5 1/6 1/7 1/8)
  :dur-ratios nil
  :init-freq 528
  :init-dur 1000)

(defmethod get-last-durs (type) '())
(defmethod add-last-dur (type dur) '())
(defmethod get-last-freqs (type) '())
(defmethod add-last-freq (type freq) '())

;; *** debugging

;;; Playing around with dynmically scoped variables to capture values from
;;; deep down the stack:
(defmacro with-debug-proxy (&body body)
  `(if (boundp '*liminale-debug-proxy*)
       ,@body
       (let (*liminale-debug-proxy*)
	 (declare (special *liminale-debug-proxy*))
	 ,@body)))

(defun liminale-log (info)
  (declare (special *liminale-debug-proxy*))
  (let ((proxy (boundp '*liminale-debug-proxy*)))
    (when proxy
      (push info *liminale-debug-proxy*))))

(defmacro liminale-dump-log (place)
  `(let ((proxy (boundp '*liminale-debug-proxy*)))
     (declare (special *liminale-debug-proxy*))
     (when proxy
       (setf ,place *liminale-debug-proxy*))))


;; *** generation

(defmethod reset-note-type (type &key &allow-other-keys))

(defmethod reset-note-type :before (type &key verbose &allow-other-keys)
  (when verbose (format t "~&Resetting ~a" type)))

(defmethod generate-new-notes (type time &rest keys &key &allow-other-keys)
  (list (apply #'get-new-note type time keys)))

;;; this :around method acts as a check for the result of all generate-new-notes
(defmethod generate-new-notes :around (type time &key verbose &allow-other-keys)
  (when verbose (format t "~&Generating new ~a notes..." type))
  (let ((result (call-next-method)))
    (test-note-list result (format nil "generate-new-notes with type ~a" type))
    result))

(defmethod get-new-note (type time &key &allow-other-keys)
  (make-note :start time :type type))

(defmethod get-new-note :around (type time &key debug &allow-other-keys)
  (if debug
      (with-debug-proxy
	(let ((new-note (call-next-method)))
	  (liminale-dump-log (note-debug new-note))
	  new-note))
      (call-next-method)))

(defmethod get-new-duration (type &rest durs)
  (get-new-duration-aux type durs))

(defmethod get-new-frequency (type &rest freqs)
  (get-new-frequency-aux type freqs))


;; ** def-note-type
;;; (define-note-type :test :min-freq 5)
(defmacro define-note-type (type-name
			    &rest kws
			    &key
			      min-freq
			      max-freq
			      min-duration
			      max-duration
			      min-no-repetitions
			      remember-n-last-notes
			      freq-ratios
			      dur-ratios
			      init-freq
			      init-dur
			    &allow-other-keys)
  (let ((predicate-name (intern (format nil "IS-~A" type-name)))
	(last-durs (gensym))
	(last-freqs (gensym))
	(mods (gensym)))
    (when (and min-no-repetitions remember-n-last-notes)
      (when (> min-no-repetitions remember-n-last-notes)
	(warn "define-note-type ~a: This type needs to remember at least ~a notes"
	      type-name min-no-repetitions)))
    `(progn
       ;; this adds the is-type-name predicate for notes
       (defun ,predicate-name (note)
	 (eq ',type-name (note-type note)))
       ;; add getter and setter methods for this type for all supplied keywords.
       ,@(loop for (keyword value) on kws by #'cddr
	       for getter-name = (intern (format nil "~A" keyword))
	       for var-name = (gensym)
	       collect `(let ((,var-name ,value))
			  (defmethod ,getter-name ((type (eql ,type-name))) ,var-name)
			  (defmethod (setf ,getter-name) (new-value (type (eql ,type-name)))
			    (setf ,var-name new-value))))
       ;; Getters and Setters for last durs and freqs
       (let (,last-durs
	     ,last-freqs)
	 (defmethod get-last-durs ((type (eql ,type-name))) (srb-get-all ,last-durs))
	 (defmethod add-last-dur ((type (eql ,type-name)) dur) (srb-add-value ,last-durs dur))
	 (defmethod get-last-freqs ((type (eql ,type-name))) (srb-get-all ,last-freqs))
	 (defmethod add-last-freq ((type (eql ,type-name)) freq) (srb-add-value ,last-freqs freq))
	 ;; this adds the reset-note-type method for the type
	 (defmethod reset-note-type ((type (eql ,type-name)) &key &allow-other-keys)
	   (setf ,last-durs
		 (make-simple-ringbuffer
		  (max (remember-n-last-notes ,type-name)
		       (min-no-repetitions ,type-name))
		  :init-element (init-dur ,type-name))
		 ,last-freqs
 		 (make-simple-ringbuffer
		  (max (remember-n-last-notes ,type-name)
		       (min-no-repetitions ,type-name))
		  :init-element (init-freq ,type-name)))
	   t)
	 ;; call the reset function to init the closure variables
	 (reset-note-type ,type-name)))))


;; EOF gen-types.lsp
