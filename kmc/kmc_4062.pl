:- module(
  kmc_4062,
  [
    assert_schema_kmc_4062/1, % +Graph:graph
    kmc_4062//2, % +Graph:atom
                 % +PPN:uri
    statistics_kmc_4062/2, % +Graph:atom
                           % -Rows:list(list)
    translate_format/3 % ?Format:uri
                       % ?Number:integer
                       % ?Name:atom
  ]
).

/** <module> KMC 4062 - FORMAT

*|KMC 4062|* contains the format, given in the documented format.

The formats are also displayed in the fingerprint kmc_2275.pl (second
position).

# Tables

## Main formats (not all are documented in STCN)

| *Name*          | *Symbol* | *|Number of leaflets|* | *|[Kettinglijnen]|* | *Watermark*                             | *Visual*                     |
| Plano           | 1°       | 1                      | horizontal          | 1/4 [lengteas] halfway [breedteas]      | [[../../www/img/plano.png]]  |
| Folio           | 2°       | 2                      | vertical            | middle                                  | [[../../www/img/folio.png]]  |
| Quarto          | 4°       | 4                      | horizontal          | part: halfway the inner margin          | [[../../www/img/kwarto.png]] |
| Octavo          | 8°       | 8                      | vertical            | part: at the top, in the innner margin  | [[../../www/img/octavo.png]] |
| Duodecimolong   | 12°      | 12                     | vertical            | part: halfway of the top margin         | [[../../www/img/duodl.png]]  |
| Duodecimocommon | 12°      | 12, 8, 6 or 4          | horizontal          | 1/3 outer margin                        | [[../../www/img/duodc.png]]  |
| 16mo            | 16°      | 8                      | horizontal          | part: at the top, in the outer margin   | [[../../www/img/16mo.png]]   |
| 18mo            | 18°      | 18, 12/6, 3x6 or 10/8  | vertical            | middle leaflet                          | [[../../www/img/18mo.png]]   |
| 24mo-long       | 24°      | 3x8 or 2x12            | vertical            | part: haldway the outer margin          | [[../../www/img/24mol.png]]  |
| 24mo            | 24°      | 3x8 or 2x12            | horizontal          | part: at the top, in the inner margin   | [[../../www/img/24mo.png]]   |
| 32mo            | 32°      | 32                     | vertical            | *Undocumented*                          | *Undocumented*               |
| 64mo            | 64°      | 64                     | horizontal          | *Undocumented*                          | *Undocumented*               |
| Other           | Other    | 0                      | does not apply      | *Undocumented*                          | *Undocumented*               |

## Format modifiers, that occur in combination with a main format

These can occur either before or after the main format specifier (except for
the =OtherFormat= main format, which takes no modifiers).

| *Name*       |
| =Agenda=     |
| =Broadsheet= |
| =Oblong=     |

## Undocumented or incorrectly documented formats

| *|Unrecognized value|* | *|Assumed equivalent|* | *Comment*                            |
| =|?|=                  | =UnknownFormat=        |                                      |
| =|°|=                  | =UnknownFormat=        |                                      |
| =|4ç|=                 | =|4°|=                 |                                      |
| =4=, for example       | =|4°|=, for example    | Recognized values without the '°'.   |
| =agenda=               | =Agenda=               | Case-distinction                     |
| =oblong=               | =Oblong=               | Case-distinction                     |
| =|Other°|=             | =Other=                | Recognized values with an extra '°'. |
| =|8 °|=                | =|8°|=                 | E.g. PPN 236634119                   |
| =|8ê|=                 | =|8°|=                 | E.g. PPN 109363094                   |
| =|12 °|=               | =|12°|=                | E.g. PPN 189120959                   |
| =|?ê|=                 | =|Unknown|=            | E.g. PPN 109361423                   |
| =|4ê|=                 | =|4°|=                 | E.g. PPN 108526747                   |
| =|2ê|=                 | =|2°|=                 | E.g. PPN 108495264                   |
| <m> x <n> mm           | =Unknown=              | E.g. PPN 317798766                   |

@tbd Verify this using KMC 2275 redundancy.

# PPN 333614321

Strange string.

# PPN 31858817X

Strange string: '2 volumes'

# PPN 31858817X

'93 x 162 mm'

Swapped with KMC 6040.

# PPN 291582117

Extra space at beginning.

# PPN 115264302

48mo

# Duodecimolong or Duodecimocommon

@author Wouter Beek
@version 2013/01-2013/03, 2013/06, 2013/09, 2014/03, 2015/02
*/

:- use_module(library(debug)).
:- use_module(library(semweb/rdf_db), except([rdf_node/1])).

:- use_module(plc(dcg/dcg_ascii)).
:- use_module(plc(dcg/dcg_cardinal)).
:- use_module(plc(dcg/dcg_content)).
:- use_module(plc(dcg/dcg_generics)).
:- use_module(plc(generics/db_ext)).

:- use_module(plRdf(api/rdf_build)).
:- use_module(plRdf(api/rdfs_build)).
:- use_module(plRdf(api/rdfs_read)).
:- use_module(plRdf(vocabulary/rdf_stat)).

:- meta_predicate(parse_value(+,+,-)).

:- rdf_meta(kmc_4062_extra(?,r)).
:- rdf_meta(translate_format(r,?)).

user:file_search_path(images, project('Images')).
user:prolog_file_type(png, png).





assert_schema_kmc_4062(G):-
  rdfs_assert_class(stcno:'FormatValue', G),
  rdfs_assert_label(stcno:'FormatValue', [nl]-'formaat waarde', G),
  
  rdfs_assert_subclass(stcno:'PrimaryFormatValue', stcno:'FormatValue', G),
  rdfs_assert_label(
    stcno:'PrimaryFormatValue',
    [nl]-'eerste formaat waarde',
    G
  ),
  
  rdfs_assert_subclass(stcno:'SecondaryFormatValue', stcno:'FormatValue', G),
  rdfs_assert_label(
    stcno:'SecondaryFormatValue',
    [nl]-'tweede formaat waarde',
    G
  ),
  
  rdf_assert_property(stcno:image, G),
  rdfs_assert_label(stcno:image, [nl]-'heeft afbeelding', G),
  
  rdf_assert_property(stcno:format, G),
  rdfs_assert_label(stcno:format, [nl]-'heeft formaat', G),
  rdf_assert_string(stcno:format, stcno:kb_name, 'KMC 4062', G),
  rdf_assert(
    stcno:format,
    stcno:documentatie,
    'http://www.kb.nl/kbhtml/stcnhandleiding/4062.html',
    G
  ),
  rdf_assert_langstring(stcno:format, stcno:picarta_name, [nl]-'formaat', G),
  
  forall(
    format(FormatID, _Symbol, _NumberOfLeaflets, _Kettinglijnen, _, _),
    (
      rdf_global_id(stcn:FormatID, Format),
      rdf_assert_instance(Format, stcno:'PrimaryFormatValue', G),
      rdfs_assert_label(Format, [nl]-FormatID, G),
      (   once(has_image(FormatID, ImageID))
      ->  absolute_file_name(
            images(ImageID),
            Image,
            [access(read),file_type(png)]
          ),
          rdf_assert_string(Format, stcno:image, Image, G)
      ;   % Not every format has an image.
          true
      )
    )
  ),
  rdf_assert_instance(stcno:agenda, stcno:'SecondaryFormatValue', G),
  rdfs_assert_label(stcno:agenda, [nl]-agenda, G),
  
  rdf_assert_instance(stcno:broadsheet, stcno:'SecondaryFormatValue', G),
  rdfs_assert_label(stcno:broadsheet, [nl]-broadsheet, G),
  
  rdf_assert_instance(stcno:oblong, stcno:'SecondaryFormatValue', G),
  rdfs_assert_label(stcno:oblong, [nl]-oblong, G),
  
  rdf_assert_instance(stcno:other_format, stcno:'FormatValue', G),
  rdfs_assert_label(stcno:other_format, [nl]-'ander formaat', G),
  
  rdf_assert_instance(stcno:unknown_format, stcno:'FormatValue', G),
  rdfs_assert_label(stcno:unknown_format, [nl]-'onbekend formaat', G).

%! format(
%!   ?Name:atom,
%!   ?Symbol:atom,
%!   ?NumberOfLeaflets:list(atom),
%!   ?Kettinglijnen:oneof([horizontal,vertical]),
%!   ?Visual:atom
%! ) is nondet.

format('plano', '1°', '1', horizontal, '1/4 [lengteas] halfway [breedteas]', '[[plano.jpg]]').
format('folio', '2°', '2', vertical, middle, '').
format('quarto', '4°', '4', horizontal, 'part: halfway the inner margin', '').
format('octavo', '8°', '8', vertical, 'part: at the top, in the innner margin', '').
format('duodecimo', '12°', '12', vertical, 'part: halfway of the top margin', '').
%format('duodecimocommon', '12°', '12, 8, 6, 4', horizontal, '1/3 outer margin', '').
%format('duodecimolong', '12°', '12', vertical, 'part: halfway of the top margin', '').
format('16mo', '16°', '8', horizontal, 'part: at the top, in the outer margin', '').
format('18mo', '18°', '18, 12/6, 3x6 or 10/8', vertical, 'middle leaflet', '').
%format('24mo_long', '24°', '3x8 or 2x12', vertical, 'part: halfway the outer margin', '').
format('24mo', '24°', '3x8 or 2x12', horizontal, 'part: at the top, in the inner margin', '').
format('32mo', '32°', '32', vertical, 'Not mentioned on the STCN website!', '').
format('48mo', '48°', '48', unknown, 'Not mentioned on the STCN website!', '').
format('64mo', '64°', '64', horizontal, 'Not mentioned on the STCN website!', '').

has_image('16mo', '16mo').
has_image('18mo', '18mo').
has_image('24mo', '24mo').
%has_image(?, '24mo_long').
%has_image(?, duodecimo_common).
%has_image(?, duodecimo_long).
has_image(duodecimo, duodecimo_common).
has_image(folio, folio).
has_image(quarto, quarto).
has_image(octavo, octavo).
has_image(plano, plano).

% Documented values.
% The extra format properties, such as 'agenda' and 'oblong', can occur
% either before or after the main format descriptor.
kmc_4062(G, PPN) -->
  kmc_4062_extra(G, PPN),
  kmc_4062_main(G, PPN),
  kmc_4062_extra(G, PPN), !.

% What's this?
% PPN 333614321
kmc_4062_extra(_G, _PPN) -->
  blanks,
  atom('Van WingheLiesvelt translation`LO`').
% What to do with this 'volumes' word?
% PPN 31858817X
kmc_4062_extra(_G, _PPN) -->
  blanks,
  atom(volumes).
kmc_4062_extra(G, PPN) -->
  blanks,
  % Case-insensitive parsing is needed, because sometimes
  % 'Agenda' and 'agenda' occur, as well as 'Oblong' and 'oblong'.
  word(Word),
  {
    Word \== '',
    downcase_atom(Word, LowercaseWord),
    rdf_global_id(stcno:LowercaseWord, Format),
    rdf_assert(PPN, stcno:format, Format, G)
  },
  blanks.
kmc_4062_extra(_G, _PPN) --> [].

kmc_4062_main(G, PPN) -->
  integer(_First),
  blanks,
  `x`,
  blanks,
  integer(_Second),
  blanks,
  `mm`,
  {rdf_assert(PPN, stcno:format, stcno:unknown_format, G)}.
kmc_4062_main(G, PPN) -->
  blanks,
  number(Number),
  blanks,
  (
    % °
    [176]
  ;
    % ê
    [234],
    (question_mark, "")
  ;
    % ç
    [231]
  ;
    ""
  ),
  {
    translate_format(Format, Number, _NumberOFLeaflets),
    rdf_assert(PPN, stcno:format, Format, G)
  }.
% Broadsheet.
% Some PPNs have a format that is not in the documentation for this KMC.
kmc_4062_main(G, PPN) -->
  atom('Broadsheet'),
  {rdf_assert(PPN, stcno:format, stcno:broadsheet, G)}.
% Undocumented and/or malformed formats.
kmc_4062_main(G, PPN) -->
  {kmc_4062_malformed(FormatAtom, FormatName)},
  atom(FormatAtom),
  {
    rdf_global_id(stcn:FormatName, Format),
    rdf_assert(PPN, stcno:format, Format, G)
  }.

% Some PPNs have no specific format information but do have this KMC.
% In these cases a special format (or non-format) is given:
% 'stcn:OtherFormat'.
kmc_4062_malformed('Other', other_format).
% Probably a variant of 'OtherFormat', e.g. PPN 317408364.
kmc_4062_malformed('Other°', other_format).
% Some PPNs seem to have the kmc_4062_malformed value '?ê', e.g. PPN 109361423.
kmc_4062_malformed('?ê', unknown_format).
% Some PPNs seem to have the malformed value '°', e.g., PPN 292525966.
kmc_4062_malformed('°', unknown_format).
% Some PPNs seem to have an alternative value for unknown, e.g.,
% PPN 152445323.
kmc_4062_malformed('?', unknown_format).
kmc_4062_malformed('0', unknown_format).

statistics_kmc_4062(G, [[A1, V1], [A2, V2] | T]):-
  A1 = 'Publications with an STCN format',
  count_subjects(stcno:format, _, G, V1),
  debug(stcn_statistics, '~w: ~w', [A1, V1]),

  A2 = 'Publications with no STCN format',
  count_subjects(stcno:format, stcno:unknown_format, G, V2),
  debug(stcn_statistics, '-- ~w: ~w', [A2, V2]),

  findall(
    [A, V],
    (
      format(Name, _Symbol, _NumberOfLeaflets, _Kettinglijnen, _, _),
      rdf_global_id(stcno:Name, Format),
      count_subjects(stcno:format, Format, G, V),
      format(atom(A), 'Publications with format ~w', [Name]),
      debug(stcn_statistics, '-- ~w: ~w', [A, V])
    ),
    T
  ).

%! translate_format(?Format:uri, ?Number:integer, ?Name:atom) is nondet.

translate_format(Format, 1, '1°'):-
  rdf_equal(Format, stcno:plano).
translate_format(Format, 2, '2°'):-
  rdf_equal(Format, stcno:folio).
translate_format(Format, 4, '4°'):-
  rdf_equal(Format, stcno:quarto).
translate_format(Format, 8, '8°'):-
  rdf_equal(Format, stcno:octavo).
translate_format(Format, 12, '12°'):-
  rdf_equal(Format, stcno:duodecimo).
translate_format(Format, 16, '16°'):-
  rdf_equal(Format, stcno:'16mo').
translate_format(Format, 18, '18°'):-
  rdf_equal(Format, stcno:'18mo').
translate_format(Format, 24, '24°'):-
  rdf_equal(Format, stcno:'24mo').
translate_format(Format, 32, '32°'):-
  rdf_equal(Format, stcno:'32mo').
translate_format(Format, 48, '48°'):-
  rdf_equal(Format, stcno:'48mo').
translate_format(Format, 64, '64°'):-
  rdf_equal(Format, stcno:'64mo').

