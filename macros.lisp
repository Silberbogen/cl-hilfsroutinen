;;;; -*- mode: lisp -*-
;;;; -*- coding: utf-8 -*-
;;;; Dateiname: macros.lisp
;;;; Beschreibung: Routinen, die mich bei diversen Aufgaben unterstützen
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


(defmacro with-gensym (syms &body body)
  "Generiert ein gensym je Element aus der Liste SYMS."
  `(let ,(mapcar #'(lambda (s) `(,s (gensym)))
				 syms)
	 ,@body))




(defmacro dosequence ((var seq &optional result) &body body
					  &aux (seq-len (length seq)))
  "Iteriert über die gegebene Sequenz SEQ."
  (with-gensym (i)
	`(do ((,i 0 (1+ ,i)))
		 ((= ,i ,seq-len)
		  ,result)
	   (let ((,var (elt ,seq ,i)))
		 ,@body))))


(defmacro for ((var start stop &optional (step 1)) &body body)
  "Eine for-Schleife mit optionaler Schrittweite"
  (with-gensym (gstop)
	`(do ((,var ,start (+ ,var ,step))
		  (,gstop ,stop))
		 ((if (minusp ,step)
			  (< ,var ,gstop)
			  (> ,var ,gstop)))
	   ,@body)))


(defmacro forever (&body body)
  "Eine Endlos-Schleife"
  `(do ()
	   (nil)
	 ,@body))


(defmacro in (obj &rest choices)
  "Prüft ob OBJ in CHOICES vorkommt und gibt entsprechend T oder NIL zurück."
  (with-gensym (gobj)
	`(let ((,gobj ,obj))
	   (or ,@(mapcar #'(lambda (c) `(eql ,gobj ,c))
					 choices)))))


(defmacro let1 (var val &body body)
  "Dient zum schnellen Anlegen und Zuweisen einer einzigen Variablen."
  `(let ((,var ,val))
	 ,@body))


(defmacro mac (form)
  "Expandiert ein Macro und gibt es schön formatiert aus."
  `(pprint (macroexpand-1 ',form)))


(defmacro permutations-rang (n lst)
  "Translator zwischen Mensch und Maschine, um die Zählung bei 1 (Mensch) gegen die Zählung bei 0 (Maschine) auszutauschen"
  `(nth-permutation (1- ,n) ,lst))


(defmacro until (test &body body)
  "Eine until-Kontrollstruktur"
  `(do ()
	   (,test)
	 ,@body))


(defmacro while (test &body body)
  "Eine while-Kontrollstruktur"
  `(do ()
	   ((not ,test))
	 ,@body))
