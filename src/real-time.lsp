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
      notes)))


;; EOF real-time.lsp
