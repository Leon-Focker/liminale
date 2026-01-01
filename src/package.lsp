;; * package

(defpackage :relax
  (:use :common-lisp :layers-utils :clm)
  (:nicknames :rl))

(in-package :relax)

;; get path to local files with #'relax-path
(let ((relax-src-dir (lyu::get-pathname-dir (asdf:system-source-directory :relax))))
  (defun relax-path (name)
    (format nil "~a~a" relax-src-dir name)))

;; EOF package.lsp
