;;;; -*- mode: lisp -*-
;;;; -*- coding: utf-8 -*-
;;;; Dateiname: packages.lisp
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


(in-package :cl-user)


(defpackage #:cl-hilfsroutinen
  (:nicknames :hilfsroutinen :hilfe :hr)
  (:use :common-lisp)
  (:export
   ;; MEMOS
   #:defmemo
   #:lösche-memorisiere
   #:lösche-alle-memos
   ;; MACROS
   #:with-gensym
   #:define-constant
   #:dosequence
   #:for
   #:forever
   #:in
   #:let1
   #:mac
   #:permutations-rang
   #:until
   #:while
   ;; PREDICATES
   #:abundante-zahl-p
   #:befreundete-zahl-p
   #:defiziente-zahl-p
   #:dreieckszahlp
   #:echte-teilmenge-p
   #:fünfeckszahlp
   #:j-oder-n-p
   #:ja-oder-nein-p
   #:kreisförmige-primzahl-p
   #:lychrel-zahl-p
   #:palindromp
   #:pandigitalp
   #:primzahlp
   #:quadratzahlp
   #:trunkierbare-primzahl-p
   #:vollkommene-zahl-p
   ;; FUNCTIONS
   #:2d-array->list
   #:achteckszahl
   #:addiere-ziffern
   #:alle-permutationen
   #:alphabetischer-wert
   #:arabisch->römisch
   #:but-nth
   #:collatz
   #:collatz-sequenz
   #:divisoren
   #:dreieckszahl
   #:dreisatz
   #:durchschnitt
   #:eingabe
   #:faktor
   #:fibonacci
   #:fibonacci-folge
   #:fünfeckszahl
   #:fünfeckszahl-folge
   #:gleichwertige-elemente
   #:mischen
   #:münzwurf
   #:nächste-primzahl
   #:nth-permutation
   #:nur-buchstaben
   #:nur-ziffern
   #:phi-tabelle
   #:primfaktoren
   #:primzahl
   #:prozent
   #:quadratzahl
   #:quersumme
   #:römisch->arabisch
   #:sechseckszahl
   #:sieb-des-eratosthenes
   #:siebeneckszahl
   #:sortiere-ziffern
   #:string-aufteilen
   #:summe-der-farey-folge
   #:summe-fortlaufender-primzahlen
   #:tausche-ziffer
   #:temperatur
   #:text-ausgabe
   #:text-auswahl
   #:text-eingabe
   #:umwandeln
   #:wochentag
   #:würfelwurf
   #:wurzel
   #:zahl->ziffern
   #:zähle-buchstaben
   #:ziffern->zahl
   ;; SYMBOLS
   #:montag
   #:dienstag
   #:mittwoch
   #:donnerstag
   #:freitag
   #:samstag
   #:sonntag))



