# Vragen bij het converteren van de STCN

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

