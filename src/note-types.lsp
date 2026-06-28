;; * note-types
;;;
;;; This is an attempt of translating the idea of an interface/trait to Lisp.
;;; I wanted to be able to define 'note-types' that are guaranteed to implement
;;; the methods defined by the trait and are able to costumize them. While this
;;; could have been done with CLOS (or structs even) and inheriting generic
;;; methods of a super-class, I wanted to do it a fun way! Here I'm still
;;; utilizing dynamic dispatch from defmethod to implement generic methods and
;;; the ability to 'overload' them. But the specific methods get bound to
;;; keywords instead of objects. Data is then bound via closures, than can be
;;; accessed with (custom and automatically generated) methods.
;;;
;;; That means that you can use any keyword as a 'note-type' and have it use
;;; the generic values and methods for generating notes, unless you define a
;;; new 'note-type' with that keyword and customize it. Look at the
;;; #'define-note-type macro and src/piano.lsp, src/pad.lsp, ... etc for
;;; inspiration.

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
;;; Put #'liminale-log anywhere in your code, #'liminale-dump-log anywhere
;;; else (should be called after log to make sense). They only work when they
;;; are somehow called from within a #'with-debug-proxy block.
(defmacro with-debug-proxy (&body body)
  `(if (boundp '*liminale-debug-proxy*)
       (progn ,@body)
       (let (*liminale-debug-proxy*)
	 (declare (special *liminale-debug-proxy*))
	 ,@body)))

(defun liminale-log (info)
  (declare (special *liminale-debug-proxy*))
  (let ((proxy (boundp '*liminale-debug-proxy*)))
    (when proxy
      (push (copy-tree info) *liminale-debug-proxy*))))

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

;;; when called wth debug = t, push any information that was logged from
;;; functions that are called here (get-new-frequency etc.) into the created
;;; note. This way we bind debug information specifically to the notes it
;;; belongs to.
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
;;; This defines a new note-type, aka a keyword with data and methods bound to
;;; it. Additional to the keyword-arguments listed in the signature below, you
;;; can use any keyword as a keyword-argument and bind a value to it. This value
;;; can then be accessed and setf'ed via (keyword type-name). Keywords listed in
;;; the function signature have a generic value that cannot be changed after
;;; definition of the note-type.
;;; EXAMPLE:
#|
(define-note-type :crazy-chords
  :min-freq 10
  :crazy-numbers '(528 813 666))

(min-freq :crazy-chords)
=> 10

(max-freq :crazy-chords)
=> 2000

(pop (crazy-numbers :crazy-chords))
=> 528

(get-new-note :crazy-chords 0)
=> #S(NOTE
      :START 0
      :DURATION 0
      :FREQ 440
      :VELOCITY 0.7
      :TIME-LEFT 0
      :TYPE :CRAZY-CHORDS
      :DEBUG NIL)
|#
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
