;;;; -*- mode: lisp -*-
;;;; -*- coding: utf-8 -*-
;;;; Dateiname: macros.lisp
;;;; Beschreibung: Routinen, die mich bei diversen Aufgaben unterstützen
;;;; ------------------------------------------------------------------------
;;;; Author: Sascha K. Biermanns, <skkd PUNKT h4k1n9 AT yahoo PUNKT de>
;;;; Lizenz: GPL v3
;;;; Copyright (C) 2011-2015 Sascha K. Biermanns
;;;; This program is free software; you can redistribute it and/or modify it
;;;; under the terms of the GNU General Public License as published by the
;;;; Free Software Foundation; either version 3 of the License, or (at your
;;;; option) any later version.
;;;;
;;;; This program is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
;;;; Public License for more details.
;;;;
;;;; You should have received a copy of the GNU General Public License along
;;;; with this program; if not, see <http://www.gnu.org/licenses/>. 
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
