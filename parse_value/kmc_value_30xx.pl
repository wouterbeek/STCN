:- module(kmc_value_30xx, []).

/** <module> Parse KMC Value: 3000-3099

Parses the 'status' of a work.

Known problems
==============

Some author PPNs have lower case x in them, e.g. PPN 06907352x.
Some author PPNs have upper case x in them, e.g. PPN 14895961X.
These are considered to be the same.
Mapped to upper case X by using option `case(upper)`.

KMC 3000: Primary author
========================

KMC 3000 contains the primary author.

The format is:
```text
voornaam/tussenvoegsel@achternaam!ppn!
```

For example:

```text
Jan/de@Wit!123456789!
```

Looking up an author PPN in Picarta
-----------------------------------

```text
http://picarta.pica.nl/DB=3.11/SET=1/TTL=1/CMD?
  ACT=SRCHA&
  IKT=1016&
  SRT=YOP&
  TRM=ppn+070105464&
  REC=*
```

---

@author Wouter Beek
@see http://www.kb.nl/kbhtml/stcnhandleiding/0500.html
@see http://support.oclc.org/ggc/richtlijnen/php/showPresentation.php?id=12&ln=nl&sec=k-0500
@see http://support.oclc.org/ggc/richtlijnen/php/showPresentation.php?id=13&ln=nl&par=p-103
@version 2015/02
*/

:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plc(dcg/dcg_content)).

:- use_module(stcn(stcn_generics)).

:- multifile(kmc:kmc_value//3).





kmc:kmc_value(KmcCode, Publication, Graph) -->
  {between(3000, 3099, KmcCode)}, !,
  '...',
  "!",
  ppn('Author', AuthorCode),
  "!",
  {
    rdf_global_id(stcno:AuthorCode, Author),
    atom_number(KmcLocalName, KmcCode),
    rdf_global_id(stcno:KmcLocalName, P),
    rdf_assert(Publication, P, Author, Graph)
  }.
