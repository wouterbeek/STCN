# Vragen bij het converteren van de STCN

## Regelonderbrekingen

Regelonderbrekeingen zorgen vaak voor ambiguiteit.

De eerste vorm van ambiguiteit komt doordat het onduidelijk is of een regel
een nieuwe KMC code begint of een continuering is van de vorige (zie [1]).

~~~
[1] 4043 !075556677!Putte, Abraham van der (I) Amsterdam, 1665-1688, 1696, 1706-
    1711
~~~

De meest voorkomende ambiguiteit (aanwezig in bijna alle PPN entries)
is dat spaties aan het begin van een continuerende regel lijken te zijn
weggehaald. Bijvoorbeeld bij [2] moeten we een spatie invoegen tussen
de eerste en de tweede regel, maar bij [3] moeten we geen spatie invoegen.

~~~
[2] 4000 De @historie der vry-metzelaars [...]. Uit de nagelaten schriften van een
    lid van het genootschap
[3] 4043 !075556677!Putte, Abraham van der (I) Amsterdam, 1665-1688, 1696, 1706-
    1711
~~~

## Onherkenbare karakters

Een groot aantal karakters lijkt te duiden op een probleem in de encodering
van het tekstbestand (zie [4]-[6]).

~~~
[4] 4000 [@Christelyke doods-betrachting, bestaende in verscheyde sterf-gesangen. /
    `IT`By Willem Sluiter`LO`]
[5] 4060 A-B`SUP`8`LO`
[6] 4062 8Â°
~~~

## SET-regel

~~~
SET: S23 [81226] TTL: 1 PPN: 337888469 PAG: 1 .
~~~

Is `SET` een afkorting / waar staat het voor?

Wat betekent `S23`?

Wat betekent `[81226]`

Is `TTL` een afkorting / waar staat het voor?

Is `PAG` een afkorting / waar staat het voor?

Welke kennis in deze regel gaat over de data (de PPN beschrijving)
en welke kennis in deze regel gaat over de serializatie (het feit dat
de PPN beschrijving op pagina (`PAG`?) 1 is afgedrukt en als eerste (`TTL`?)
in dit bestand voorkomt?

## Ingevoerd-regel

~~~
Ingevoerd: 1996:27-09-11 Gewijzigd: 1996:27-09-11 09:03:30 Status: 1996:27-09-
11
~~~

Betekent `Ingevoerd` dat een PPN beschrijving op dat moment aan de STCN
is toegevoegd?

Staat `Gewijzigd` voor de datum waarop een specifieke PPN beschrijving
voor de _laatste_ keer gewijzigd is?

Op welke datum is de waarde voor `Gewijzigd` vastgesteld
(``laatst gewijzigd'' is een conntext-afhankelijk begrip)?

Mag uit gelijke waardes voor `Ingevoerd` en `Gewijzigd` worden geconcludeerd
dat een PPN beschrijving voor de datum van serializatie geen keer is
aangepast?

Wat betekent `Status`?

Hoe moeten de waardes in deze regel gelezen worden?
~~~
1996:27-09-11 09:03:30
~~~

Jaar, maand, day, uur, minuut, seconde?
M.b.t. welke calender en tijdzone?


