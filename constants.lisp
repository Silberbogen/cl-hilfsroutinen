;;;; -*- mode: lisp -*-
;;;; -*- coding: utf-8 -*-
;;;; Dateiname: constants.lisp
;;;; Beschreibung: Konstanten, die Allgemeingültigkeit haben
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


;; Elektromagnetismus
(define-constant lichtgeschwindigkeit-im-vakuum 299792458 "Lichtgeschwindigkeit in m/s")
(define-constant magnetische-feldkonstante 12.566370614e-7 "H/m")
(define-constant elektrische-feldkonstante 8.85418781762039e-12 "F/m")
(define-constant coloumb-konstante 8987551787.3681764 "m/F")
(define-constant elementarladung 1.602176620898e-19 "C")
(define-constant von-klitzing-konstante 25812.807455559 "Ω")

;; Gravitation
(define-constant gravitationskonstante 6.6740831e-11 "m³/(kg * s²)")

;; Thermodynamik
(define-constant absoluter-nullpunkt -273.15 "°C")
(define-constant avogadro-konstante 6.02214085774e23 "1/mol")
(define-constant boltzmann-konstante 1.3806485279e-23 "J/K")
(define-constant loschmidt-konstante 2.686781115e25 "m-³")
(define-constant molares-volumen-eines-idealen-gases 0.02241396213 "m³/mol")
(define-constant stefan-boltzmann-konstante 5.67036713e-8 "W/(m² * K^4)")
(define-constant universelle-gaskonstante 8.314459848 "J/(K * mol)")

;; Teilchenphysik
(define-constant erste-strahlungskonstante 3.74177179046e-16 "W * m²")
(define-constant spektrale-strahlungskonstante 1.19104286953e-16 "(m^4 * kg) / s³")
(define-constant zweite-strahlungskonstante 1.4387773683e-2 "m * K")
(define-constant bohrscher-radius 5.291772106712e-11 "m")
(define-constant bohrsches-magneton 9.27400999457e-24 "J/T")
(define-constant kernmagneton 5.05078369931e-27 "J/T")
(define-constant plancksches-wirkungsquantum 1.05457180013e-34 "J * s")
(define-constant feinstrukturkonstante-mit-planckladung 7.297352566417e-3)

