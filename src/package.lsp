;; * package

(defpackage :relax
  (:use :common-lisp :layers-utils :clm)
  (:nicknames :rl))

(in-package :relax)

;; get path to local files with #'relax-path
(let ((relax-src-dir (lyu::get-pathname-dir (asdf:system-source-directory :relax))))
  (defun relax-path (name)
    (format nil "~a~a" relax-src-dir name)))

;; same for #'clm-path
(let ((clm-src-dir (lyu::get-pathname-dir (asdf:system-source-directory :clm))))
  (defun clm-path (name)
    (format nil "~a~a" clm-src-dir name)))

;; load additional clm instruments

(load (compile-file (clm-path "moog.lisp")))
(load (compile-file (clm-path "svf.lisp")))
(load (compile-file (relax-path "src/moog.ins")))
(load (compile-file (relax-path "src/sine.ins")))

;; EOF package.lsp
