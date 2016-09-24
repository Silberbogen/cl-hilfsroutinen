cl-hilfsroutinen
================

Diverse Funktionen und Module, die leicht in eigene CL-Programme
importiert werden können. Mir dienten sie unter anderem zur
Bearbeitung von Aufgaben des Projects Euler.


*Memoisierung*
--------------
* **defmemo**
* **lösche-memorisiere**
* **lösche-alle-memos**


*Macros*
--------
* **with-gensym**
* **define-constant**
* **dosequence**
* **for**
* **forever**
* **in**
* **let1**
* **mac**
* **permutations-rang**
* **until**
* **while**


*Prädikate*
-----------
* **abundante-zahl-p**
* **befreundete-zahl-p**
* **defiziente-zahl-p**
* **dreieckszahlp**
* **echte-teilmenge-p**
* **fünfeckszahlp**
* **j-oder-n-p**
* **ja-oder-nein-p**
* *kombinierbare-primzahlen-p*
* **kreisförmige-primzahl-p**
* **lychrel-zahl-p**
* **palindromp**
* **pandigitalp**
* **primzahlp**
* **quadratzahlp**
* **trunkierbare-primzahl-p**
* **vollkommene-zahl-p**


*Funktionen*
------------
* **2d-array->list**
* **achteckszahl**
* **addiere-ziffern**
* **alle-permutationen**
* **alphabetischer-wert**
* **arabisch->römisch**
* **but-nth**
* **collatz**
* **collatz-sequenz**
* **divisoren**
* **dreieckszahl**
* **dreisatz**
* **durchschnitt**
* **eingabe**
* **faktor**
* **faktor-festlegen**
* **fibonacci**
* **fibonacci-folge**
* **fünfeckszahl**
* **fünfeckszahl-folge**
* **gleichwertige-elemente**
* *kombiniere-integer*
* **mischen**
* **münzwurf**
* **nächste-primzahl**
* **nth-permutation**
* **nur-buchstaben**
* **nur-ziffern**
* **phi-tabelle**
* **primfaktoren**
* **primzahl**
* **prozent**
* **quadratzahl**
* **quersumme**
* **römisch->arabisch**
* **sechseckszahl**
* **sieb-des-eratosthenes**
* **siebeneckszahl**
* **sortiere-ziffern**
* **string-aufteilen**
* **summe-der-farey-folge**
* **summe-fortlaufender-primzahlen**
* **tausche-ziffer**
* **temperatur**
* **text-ausgabe**
* **text-auswahl**
* **text-eingabe**
* **umwandeln**
* **wochentag**
* **würfelwurf**
* **wurzel**
* **zahl->ziffern**
* **zähle-buchstaben**
* **ziffer-summe**


Um ein Gefühl für die Geschwindigkeit zu erhalten:
--------------------------------------------------
Für die Berechnung der Bildschirmfotos wurde z.B. eine Liste aller Primzahlen bis 16.000.000 erstellt, wofür ein 1-GHz-Rechner mit einem Kern gerade einmal 0.781 Sekunden benötigte.
Weiterhin sieht man diverse andere Berechnungsroutinen, so auch eine Auflistung der ersten 136 Fibonacci-Zahlen (weil sie so schön auf ein Bildschirmfoto passten). Die Berechnung erfolgte in Bruchteilen einer Sekunde, so das der Timer auf 0,000 Sekunden verblieben ist. Die Berechnung der ersten 10.000 Zahlen der Fibonacci-Folge benötigten immer noch nur den Bruchteil einer Sekunde.
Wem das zu langsam ist: Es geht vermutlich noch viel, viel schneller,
wenn man unbedingt compilerspezifisch optimieren möchte, oder wenn man
Sicherheit und Debugmöglichkeit verbannt, aber das war und ist nicht
mein Ansinnen. Meine Routinen habe ich nach bestem Wissen und Gewissen
geschrieben, sie sollten stets noch einen Hauch von _Eleganz_
darstellen, außerdem hasse ich es, mich ewig zu wiederholen, ich
spiele gleichermassen gerne mit do, dolist, dotimes und loop. Ein
letztes Kriterium ist, das sämtliche Routinen _universell mit jeder
vollständigen Implementation von Common Lisp lauffähig_ sein sollen.


Bildschirmfotos
---------------
![Bildschirmfoto](/bildschirmfoto.png)
![Bildschirmfoto2](/bildschirmfoto2.png)
![Bildschirmfoto3](/bildschirmfoto3.png)
![Bildschirmfoto4](/bildschirmfoto4.png)

