#lang racket
#|
Übungszettel 3
@author Larissa Rutzen 6927940, Tim Rückert 6946317
Gruppe: Mittwoch 10:00 - 11:30 Benjamin Seppke

Beispiele befinden sich immer auskommentiert am Ende der jeweilgen Aufgaben
|#

;;;Aufgabe 1.1 - Eine Datensturktur für die Zuordnung der Buchstaben zu ihren Buchstabierschlüsseln
;Wir benutzen hier eine assoziative Liste. Sie speichert zu jeden Buchstaben einen Schlüssel.
;Diese Art der Liste hat den Vorteil, das man mit "assoc" direkt auf die Werte zugreifen kann.
(define Buchstabiertafel
  '(
    (#\A Alfa)
    (#\T Tango)
    (#\B Bravo)
    (#\U Uniform)
    (#\C Charlie)
    (#\V Viktor)
    (#\D Delta)
    (#\W Whiskey)
    (#\E Echo)
    (#\X X-ray)
    (#\F Foxtrott)
    (#\Y Yankee)
    (#\G Golf)
    (#\Z Zulu)
    (#\H Hotel)
    (#\I India)
    (#\J Juliet)
    (#\K Kilo)
    (#\L Lima)
    (#\M Mike)
    (#\N November)
    (#\O Oscar)
    (#\P Papa)
    (#\Q Quebec)
    (#\R Romeo)
    (#\S Sierra)
    (#\T Tango)
    (#\U Uniform)
    (#\V Viktor)
    (#\W Whiskey)
    (#\X X-ray)
    (#\Y Yankee)
    (#\Z Zulu)
    (#\0 Nadazero)
    (#\1 Unaone)
    (#\2 Duotwo)
    (#\3 Terrathree)
    (#\4 Carrefour)
    (#\5 Pentafive)
    (#\6 Soxisix)
    (#\7 Setteseven)
    (#\8 Oktoeight)
    (#\9 Novonine)
    (#\, Decimal)
    (#\. Stop)))

;;;Aufgabe 1.2 - Codierungsfunktion: char->key
;Buchstabe muss hier ein Char sein
(define (char->key Buchstabe)
  (cadr (assoc Buchstabe Buchstabiertafel)))

;Beispiel 1.2
;(char->key #\F)
;(char->key #\.)

;;;Aufgabe 1.3 - Wandelt einen String in eine Charlist um
; str - ein String, der das Alphabet und Zahlen, sowie , und . umfasst
(define (string->buchstabierschluessel str)
  (charlist->Tafel (string->list str)))

;;;Aufgabe 1.3 - Wandelt rekursiv die charlist in eine Liste mit den Buchstabierschlüssel um.
(define (charlist->Tafel charlist)
  (if (empty? charlist)
      '()
      (cons (char->key (car charlist))
            (charlist->Tafel (cdr charlist)))))

;Beispiel 1.3
;(string->buchstabierschluessel "ABCD.,12345")

;;;Aufgabe 2.1
(require se3-bib/flaggen-module)
;; Liste aus Paaren von char -> string
;Wir verwenden hier einen Hashtable. Dieser bildet genau eine Value zu einem Key ab.
;Dies bietet sich auf Grund der guten Lesbarkeit an und da wir immer genau einen Char und
;einen Wert haben, passt der Hashtable sehr gut. Zugriffszeit ist O(log N),
;wobei log N ("effectively constant-time access and update")
(define Flaggentafel
   (hash #\A A
     #\B B
     #\C C
     #\D D
     #\E E
     #\F F
     #\G G
     #\H H
     #\I I
     #\J J
     #\K K
     #\L L
     #\M M
     #\N N
     #\O O
     #\P P
     #\Q Q
     #\R R
     #\S S
     #\T T
     #\U U
     #\V V
     #\W W
     #\X X
     #\Y Y
     #\Z Z
     #\0 Z0
     #\1 Z1
     #\2 Z2
     #\3 Z3
     #\4 Z4
     #\5 Z5
     #\6 Z6
     #\7 Z7
     #\8 Z8
     #\9 Z9))

;;;Aufgabe 2.2 Codierungsfunktion
;Gibt bei Eingabe eines chars die HashValue aus dem Table heraus. Durch das Flaggenmodul wird dadurch eine Flagge
(define (char->flag char)
  (if (hash-has-key? Flaggentafel char)
      (hash-ref Flaggentafel char)
      (error "Falsche Eingabe")))

;Beispiel Aufgabe 2.2
;(char->flag #\8)
;(char->flag #\B)

;;;Aufgabe 2.3 - Funktion zum Buchstabieren eines Strings in Flaggen
; str wird in eine Charlist umgewandelt und es wird charlist->flag aufgerufen mit der Charlist als Argument
(define (string->flag str)
  (charlist->flag (string->list str)))
;Es wird geprüft ob die Charliste leer ist, falls dem nicht so ist wird das erste Element als Flagge in eine Liste geschrieben und die Funktion erneut rekursiv aufgerufen,
;bis die charlist leer ist. Beim rekursiven Aufruf wird nur der Tail der Charlist übergeben.
(define (charlist->flag charlist)
  (if (empty? charlist)
      '()
      (cons (char->flag (car charlist))
            (charlist->flag (cdr charlist)))))

;Beispiel Aufgabe 2.3
;(string->flag "ABZYF0203C")

;;;Aufgabe 3.1
(require racket/gui/base)
;Wir verwenden hier einen Hashtable. Dieser bildet genau eine Value zu einem Key ab.
;Dies bietet sich auf Grund der guten Lesbarkeit an und da wir immer genau einen Char und
;einen String haben, passt der Hashtable sehr gut. Zugriffszeit ist O(log N),
;wobei log N ("effectively constant-time access and update")
(define Morsehash
  (hash #\A "./Morse-A.wav"
     #\B "./Morse-B.wav"
     #\C "./Morse-C.wav"
     #\D "./Morse-D.wav"
     #\E "./Morse-E.wav"
     #\F "./Morse-F.wav"
     #\G "./Morse-G.wav"
     #\H "./Morse-H.wav"
     #\I "./Morse-I.wav"
     #\J "./Morse-J.wav"
     #\K "./Morse-K.wav"
     #\L "./Morse-L.wav"
     #\M "./Morse-M.wav"
     #\N "./Morse-N.wav"
     #\O "./Morse-O.wav"
     #\P "./Morse-P.wav"
     #\Q "./Morse-Q.wav"
     #\R "./Morse-R.wav"
     #\S "./Morse-S.wav"
     #\T "./Morse-T.wav"
     #\U "./Morse-U.wav"
     #\V "./Morse-V.wav"
     #\W "./Morse-W.wav"
     #\X "./Morse-X.wav"
     #\Y "./Morse-Y.wav"
     #\Z "./Morse-Z.wav"
     ))
;;;Aufgabe 3.2
;Gibt den String heraus, wenn der Key in dem Hashtable ist
(define (char->morse char)
  (if (hash-has-key? Morsehash char)
      (hash-ref Morsehash char)
      (error "Falsche Eingabe")))

;Beispiel 3.2
;(char->morse #\X)

;;;Aufgabe 3.3
;Hilfsfunktion - Wandelt einen String in eine Charlist um
(define (string->morse str)
  (charlist->morse (string->list str)))

;Rekursive Funktion die sequenziell die Charlist durchgeht und für jeden Char den jeweiligen Morsecode abspielt 
(define (charlist->morse charlist)
  (cond
  [(empty? charlist) '()]
  [else
    (play-sound (char->morse (car charlist)) #f)
        (charlist->morse (cdr charlist))]))
         
;Beispiel 3.3
;(string->morse "SOS")