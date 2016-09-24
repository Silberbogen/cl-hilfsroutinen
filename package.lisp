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
   #:kombinierbare-primzahlen-p
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
   #:faktor-festlegen
   #:fibonacci
   #:fibonacci-folge
   #:fünfeckszahl
   #:fünfeckszahl-folge
   #:gleichwertige-elemente
   #:kombiniere-integer
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
   #:sonntag
   #:yoctometer
   #:zeptometer
   #:am
   #:attometer
   #:femtometer
   #:fm
   #:picometer
   #:pm
   #:Ångström
   #:Å
   #:nanometer
   #:nm
   #:mikrometer
   #:mm²
   #:µm
   #:quadratmillimeter
   #:cm²
   #:quadratzentimeter
   #:mm
   #:millimeter
   #:tausendstel
   #:cm
   #:dm²
   #:hundertstel
   #:quadratdezimeter
   #:centimeter
   #:inch
   #:zoll
   #:dm
   #:dezimeter
   #:zehntel
   #:foot
   #:fuß
   #:gerte
   #:schritt
   #:yard
   #:yd
   #:bit
   #:g
   #:gramm
   #:m
   #:m²
   #:meter
   #:qm
   #:quadratmeter
   #:fathom
   #:fth
   #:byte
   #:octet
   #:oktett
   #:square-foot
   #:duzend
   #:shackle
   #:shot
   #:a
   #:ar
   #:hundert
   #:gros
   #:gröthen
   #:gruessa
   #:tylt
   #:pfund
   #:kb
   #:kg
   #:km
   #:kilobyte
   #:kilogram
   #:kilometer
   #:myriameter
   #:tausend
   #:kib
   #:kibibyte
   #:square-inch
   #:meile
   #:mile
   #:großes-gros
   #:großgros
   #:maß
   #:seemeile
   #:international-nautical-mile
   #:acre
   #:league
   #:nautical-league
   #:sea-league
   #:ha
   #:hektar
   #:zehntausend
   #:zentner
   #:ztr
   #:dezitonne
   #:dezitonnen
   #:doppelzentner
   #:dt
   #:dz
   #:km²
   #:mb
   #:megabyte
   #:megameter
   #:mib
   #:mebibyte
   #:square-mile
   #:gb
   #:gigabyte
   #:gigameter
   #:gm
   #:kilotonne
   #:kilotonnen
   #:kt
   #:milliarde
   #:gib
   #:gibibyte
   #:billion
   #:megatonne
   #:mt
   #:tb
   #:terabyte
   #:terameter
   #:tm
   #:tebibyte
   #:tib
   #:billiarde
   #:pb
   #:petabyte
   #:petameter
   #:pebibyte
   #:pib
   #:eb
   #:exabyte
   #:em
   #:exameter
   #:exbibyte
   #:eib
   #:zb
   #:zettabyte
   #:zebibyte
   #:zib
   #:yb
   #:yottabyte
   #:yottameter
   #:yobibyte
   #:yib))



