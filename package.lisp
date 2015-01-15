;;;; -*- mode: lisp -*-
;;;; -*- coding: utf-8 -*-
;;;; Dateiname: packages.lisp
;;;; ------------------------------------------------------------------------
;;;; Author: Sascha K. Biermanns, <skkd PUNKT h4k1n9 AT yahoo PUNKT de>
;;;; Lizenz: ISC
;;;; Copyright (C) 2011-2015 Sascha K. Biermanns
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

(in-package :cl-user)

(defpackage #:cl-hilfsroutinen
  (:nicknames :hilfsroutinen :hilfe :hr)
  (:use :common-lisp)
  (:export
   ;; MACROS
   #:with-gensym
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
   #:dreieckszahl
   #:dreisatz
   #:durchschnitt
   #:echte-teiler
   #:eingabe
   #:faktor
   #:fibonacci
   #:fibonacci-folge
   #:fünfeckszahl
   #:fünfeckszahl-folge
   #:gleichwertige-elemente
   #:liste->zahl
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
   #:römisch->arabisch
   #:sechseckszahl
   #:sieb-des-eratosthenes
   #:siebeneckszahl
   #:sortiere-ziffern
   #:string-aufteilen
   #:summe-der-farey-folge
   #:summe-fortlaufender-primzahlen
   #:tausche-ziffer
   #:teiler
   #:temperatur
   #:textausgabe
   #:text-auswahl
   #:umwandeln
   #:wochentag
   #:würfelwurf
   #:zahl->liste
   #:zähle-buchstaben
   #:ziffer-summe))



