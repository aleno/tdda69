;; Uppgift 1: Applicative order vs. normal order

;; Applicative order:
;; Först evalueras argumenten, deluttrycken för att sedan appliceras på
;; funktionen, vilket innebär att båda grenar kommer evalueras och funktionen
;; p hamnar i en evighetsloop.

;; Normal order:
;; Först expanderas funktionen så långt det går, sedan evalueras argumenten.
;; I detta fall evalueras inte argumenten förens de behövs, vilket gör att
;; funktionen p aldrig anropas.

;; Uppgift 2: Special forms

;; If är en special form då interpretation inte evaluerar denna funktion på
;; samma sätt som standard funktioner. Den börjar med att evaluera predikatet
;; (det första argumentet), för att sedan evaluera antingen den sanna eller
;; falska grenen.

;; Vid anropet av sqrt-iter kommer alla argumenten evalueras. Detta gör att det
;; rekursiva anropet för den falska grenen kommer att resultera i en
;; evighetsloop.
