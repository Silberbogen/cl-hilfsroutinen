;;;; -*- mode: lisp -*-
;;;; -*- coding: utf-8 -*-
;;;; Dateiname: memorisiere.lisp
;;;; Beschreibung: Routinen, die rechenintensiv sind
;;;; ------------------------------------------------------------------------
;;;; Author: Sascha K. Biermanns, <skbierm AT gmail PUNKT com>
;;;; Lizenz: GPL v3
;;;; Copyright (C) 2011-2016 Sascha K. Biermanns
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


(defmacro defmemo (fn args &body body)
  "Definiere eine sich erinnernde Funktion."
  `(memorisiere (defun ,fn ,args ,@body)))


(defun lösche-memorisiere (fn-name)
  "Löscht die Hash-Tabelle einer MEMO-Funktion."
  (let ((table (get fn-name 'memo)))
	(when table (clrhash table))))


(defun lösche-alle-memos ()
  (dolist (i '(achteckszahl
               dreieckszahl
               faktor
               fibonacci
               fünfeckszahl
               nächste-primzahl
               nth-permutation
               primfaktoren
               primzahl
               siebeneckszahl))
    (lösche-memorisiere i)))


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
