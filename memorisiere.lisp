;;;; -*- mode: lisp -*-
;;;; -*- coding: utf-8 -*-
;;;; Dateiname: defmemos.lisp
;;;; Beschreibung: Routinen, die rechenintensiv sind
;;;; ------------------------------------------------------------------------
;;;; Author: Sascha Biermanns, <skkd.h4k1n9@yahoo.de>
;;;; Lizenz: ISC
;;;; Copyright (C) 2011-2014 Sascha Biermanns
;;;; Permission to use, copy, modify, and/or distribute this software for any
;;;; purpose with or without fee is hereby granted, provided that the above
;;;; copyright notice and this permission notice appear in all copies.
;;;; THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
;;;; WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
;;;; MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
;;;; ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
;;;; WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
;;;; ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
;;;; OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
;;;; ------------------------------------------------------------------------


(in-package #:cl-hilfsroutinen)


(defmacro defmemo (fn args &body body)
  "Definiere eine sich erinnernde Funktion."
  `(memorisiere (defun ,fn ,args ,@body)))


(defun lösche-memorisiere (fn-name)
  "Löscht die Hash-Tabelle einer MEMO-Funktion."
  (let ((table (get fn-name 'memo)))
	(when table (clrhash table))))


(defun memorisiere (fn-name &key (key #'first) (test #'eql))
  "Ersetzt FN-NAME's globale definition mit einer MEMORIZE-Version."
  (setf (symbol-function fn-name)
		(memo (symbol-function fn-name) fn-name key test)))


(defun memo (fn name key test)
  "Liefert eine MEMO-Funktion von FN zurück."
  (let ((table (make-hash-table :test test)))
	(setf (get name 'memo) table)
	#'(lambda (&rest args)
		(let ((k (funcall key args)))
		  (multiple-value-bind (val found-p)
			  (gethash k table)
			(if found-p
				val
				(setf (gethash k table) (apply fn args))))))))
