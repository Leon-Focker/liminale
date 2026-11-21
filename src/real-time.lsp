;; * real-time

(in-package :liminale)

(let ((cursor-ms 0)
      (types '())
      (active-notes '()))
  
  (defun set-rt-note-types (note-types)
    (setf types note-types))

  (defun reset-rt ()
    (reset-random-liminale)
    (loop for type in types do (reset-note-type type))
    (setf cursor-ms 0
	  active-notes '()))

  (defun generate-notes-until (horizon-ms)
    (multiple-value-bind (notes active end-time)
	(run-generation-loop
	 cursor-ms
	 horizon-ms
	 types
	 :active-notes active-notes)
      (setf active-notes active
	    cursor-ms end-time)
      notes))

  (defun set-cursor (time-ms)
    (reset-rt)
    (multiple-value-bind (notes active end-time)
	(run-generation-loop
	 0
	 time-ms
	 types)
      (setf active-notes active
	    cursor-ms end-time)
      (loop for note in active
	    for copy = (copy-structure note)
	    do (setf (note-start copy) time-ms
		     (note-duration copy) (note-time-left copy))
	    collect copy))))

;; EOF real-time.lsp
