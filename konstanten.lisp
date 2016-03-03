;;;; -*- mode: lisp -*-
;;;; -*- coding: utf-8 -*-
;;;; Dateiname: konstanten.lisp
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



(defmacro defcon (name symbol zahl . dimensionen)
  `(defparameter ,name ,zahl ",symbol ,dimensionen"))

;; Elektromagnetismus
(defcon lichtgeschwindigkeit-im-vakuum c 299792458 m/s)
(defcon magnetische-feldkonstante µ0 12.566370614e-7 H/m)
(defcon elektrische-feldkonstante ε0 8.85418781762039e-12 F/m)
(defcon coloumb-konstante k 8987551787.3681764 m/F)
(defcon elementarladung e 1.602176620898e-19 C)
(defcon von-klitzing-konstante Rk 25812.807455559 Ω)

;; Gravitation
(defcon gravitationskonstante G 6.6740831e-11 m³/(kg * s²))

;; Thermodynamik
(defcon absoluter-nullpunkt T0 -273.15 °C)
(defcon avogadro-konstante N 6.02214085774e23 1/mol)
(defcon boltzmann-konstante kB 1.3806485279e-23 J/K)
(defcon loschmidt-konstante NL 2.686781115e25 m-³)
(defcon molares-volumen-eines-idealen-gases Vm 0.02241396213 m³/mol)
(defcon stefan-boltzmann-konstante σ 5.67036713e-8 W/(m² * K^4))
(defcon universelle-gaskonstante R0 8.314459848 J/(K * mol))


