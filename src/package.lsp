;; * package

(defpackage :liminale
  (:use :common-lisp :layers-utils :clm)
  (:nicknames :li))

(in-package :liminale)

;; get path to local files with #'liminale-path
(let ((liminale-src-dir (lyu::get-pathname-dir (asdf:system-source-directory :liminale))))
  (defun liminale-path (name)
    (format nil "~a~a" liminale-src-dir name)))

;; same for #'clm-path
(let ((clm-src-dir (lyu::get-pathname-dir (asdf:system-source-directory :clm))))
  (defun clm-path (name)
    (format nil "~a~a" clm-src-dir name)))

;; load additional clm instruments

(load (compile-file (clm-path "moog.lisp")))
(load (compile-file (clm-path "svf.lisp")))
(load (compile-file (liminale-path "src/moog.ins")))
(load (compile-file (liminale-path "src/sine.ins")))
(load (compile-file (liminale-path "src/simple-echo.ins")))
(load (compile-file (liminale-path "src/splinter.ins")))

;; EOF package.lsp
