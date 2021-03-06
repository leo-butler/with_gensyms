;;/* -*- Mode: lisp -*- */
;;
;; $Id:$
;;
;; Author: Leo Butler (l_butler@users.sourcerforge.net)
;;
;; This file is Maxima/Lisp code (http://maxima.sourceforge.net/)
;; 
;; It is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or (at your
;; option) any later version.
;; 
;; This software is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public
;; License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this file. If not, see http://www.gnu.org/licenses/. 
;;
;; Time-stamp: <2017-11-23 12:07:54>

(in-package :maxima)

(defun maxima-symbol-p (x)
  (let ((n (symbol-name x)))
    (or (and (> (length n) 2)                                    ;; single character symbols like $A should be discarded, but true=t kept
             (or (char= (char n 0) #\$) (char= (char n 0) #\%))) ;; only symbols beginning with $ or %
	(cdr (mfuncall '$properties x)))))                       ;; any symbol with a non-trivial property list

(declaim (special $symbols))
(defmvar $wg_reversealias t)
(defmvar $wg_gensymize_is_idempotent t)

(defmfun $add_maxima_symbol (x &optional (s $symbols))
  (assert (or (symbolp x) ($listp x)))
  (cond ((symbolp x)
	 (setf (gethash x s) t))
	(($listp x)
	 (dolist (e (cdr x))
	   ($add_maxima_symbol e s)))
	(t nil))
  '$done)


(defmvar $symbols
  (let ((s (make-hash-table :test #'eq)))
    (do-symbols (x)
      (if (maxima-symbol-p x) ($add_maxima_symbol x s)))
    s))

(defmfun $maxima_symbolp (x)
  (or (gethash x $symbols nil)
      (member x (mapcar #'caar (cdr $functions)) :test #'eq)
      (member x (mapcar #'caar (cdr $macros)) :test #'eq)))


(defmfun $remove_maxima_symbols (x)
  (assert (listp x))
  (remove-if #'$maxima_symbolp x))

(defmfun $delete_maxima_symbols (x)
  "Deletes `x' from the hash-table of symbols `$symbols'. The input
may be a symbol or mlist of symbols."
  (assert (or ($listp x) (symbolp x)))
  (cond ((symbolp x)
	 (remhash x $symbols))
	(($listp x)
	 (dolist (e (cdr x)) (remhash e $symbols)))
	(t ;; never get here
	 nil))
  '$done)

(defmfun $maxima_symbols ()
  (let (s)
    (maphash (lambda (k v)
	       (declare (ignore v))
	       (push k s)) $symbols)
    (cons '(mlist simp) s)))

(defmfun $wg_gensymize (x)
  (declare (special $wg_reversealias $wg_gensymize_is_idempotent))
  (assert (symbolp x))
  ;; make function idempotent by returning a symbol produced by $wg_gensymize
  ;; unless $wg_gensymize_is_idempotent is nil
  (cond ((and $wg_gensymize_is_idempotent (mfuncall '$featurep x '$gensym))
	 x)
	(t
	 (let ((w (gensym (format nil "%~a_" (stripdollar x)))))
	   (setf (get w 'reversealias) (if $wg_reversealias (make-symbol (format nil "%~a" x)) x))
	   (mfuncall '$declare w '$gensym)
	   w))))

;; Local Variables:
;; time-stamp-format: "%:y-%02m-%02d %02H:%02M:%02S"
;; End:
;;/* end of symbols.lisp */
