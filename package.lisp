;;;; -*- mode: lisp -*-
;;;; -*- coding: utf-8 -*-
;;;; package.lisp

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
   #:collatz-rang
   #:collatz-sequenz
   #:divisoren
   #:dreieckszahl
   #:dreisatz
   #:durchschnitt
   #:eingabe
   #:faktor
   #:fibonacci-folge
   #:fibonaccizahl
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
   #:primzahl-rang
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
   #:temperatur
   #:textausgabe
   #:text-auswahl
   #:umwandeln
   #:wochentag
   #:würfelwurf
   #:zahl->liste
   #:zähle-buchstaben
   #:ziffer-summe))



