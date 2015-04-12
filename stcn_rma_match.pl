:- module(stcn_rma_match,
	  [ stcn_rma_match/0 ]).

:- use_module(library(semweb/rdf_db)).
:- use_module(library(dcg/basics)).
:- use_module(library(apply)).
:- use_module(library(thread)).

:- rdf_register_prefix(stcn, 'http://stcn.data2semantics.org/resource/').
:- rdf_register_prefix(edm, 'http://www.europeana.eu/schemas/edm/').

%%	stcn_rma_match
%
%	Match entries on a server with the Short title cataloge
%	Netherlands and Rijksmuseum collection loaded. Get all
%	publications and artworks with notes, and see for evey
%	publications whether it matches a note of an artwork.
stcn_rma_match :-
    findall(Publication, get_publication(Publication), ListOfPublications),
    length(ListOfPublications, NumberOfPublications),
    format('Found ~p publicatoins~n', [NumberOfPublications]),
    findall(Artwork, get_artwork(Artwork), ListOfArtworks),
    length(ListOfArtworks, NumberOfArtworks),
    format('Found ~p artworks~n', [NumberOfArtworks, Artwork]),
    concurrent_maplist(match_publication(ListOfArtworks), ListOfPublications).


%%	match_publication(+ListOfArtworks, +Publication)
%
%	Check for one publication whether it matches one of the
%	artworks.
match_publication(ListOfArtworks, Publication) :-
    maplist(match_artwork(Publication), ListOfArtworks).

%%	match_artwork(+Publication, +Artwork)
%
%	See if the author, title and year are present in the reference
%	of an artwork.
match_artwork(publication(PublicationUri, Author, Title, Year),
	      artwork(ArtworkUri, Reference, Date)) :-
    match(Reference, Title, TitleMatched),
    match(Reference, Author, AuthorMatched),
    match(Date, Year, DateMatched),
    add_match(PublicationUri, ArtworkUri, TitleMatched, AuthorMatched, DateMatched).

%%	match(+RmaReference, STCNValue, -Boolean)
%
%	See if a value of STCN is a substring of the RMA reference.
match(RMA, STCN, true) :-
    sub_atom(RMA, _, _, _, STCN),!.
match(_Date, _Year, false).

%%	add_match(+PublicationUri, +ArtworkUri, +Match1, +Match2,
%	Match3)
%
%	Add hasPart triple if all three aspects were matched in the
%	artwork reference.
add_match(PublicationUri, ArtworkUri, true, true, true) :-
    !,
    format('Adding match ~p and ~p~n', [PublicationUri, ArtworkUri]),
    rdf_assert(PublicationUri, dcterms:hasPart, ArtworkUri, 'stcn_rma_links').
add_match(_, _, _, _, _).

%%	get_publication(-Publication)
%
%	Get a title, author and publication date of a publication.
get_publication(publication(Uri, Author, Title, Year)) :-
    rdf(Uri, rdf:type, 'http://stcnv.data2semantics.org/resource/vocab/Publication'),
    publication_title(Uri, Title),
    publication_author(Uri, Author),
    publication_year(Uri, Year).

%%	publication_title(+Uri, -CleanTitle)
%
%	Get the title beloning to a uri and normalize it.
publication_title(Uri, CleanTitle) :-
    rdf(Uri, stcn:title, literal(type(_T, DirtyTitle))),
    debug(title, 'stcn:title: ~p', [DirtyTitle]),
    sanitize_title(DirtyTitle, CleanTitle).

% get and cleanup stcn author
publication_author(Uri, CleanAuthor) :-
    rdf(Uri, stcn:primary_author, AuthorUri),
    rdf(AuthorUri, stcn:name_full, literal(type(_T, Author))),
    sanitize(Author, CleanAuthor).

% get stcn publication year
publication_year(Uri, Year) :-
    rdf(Uri, stcn:exact_publication_year, literal(type(_T, Year))).

%%	get_artwork(-Artwork)
%
%	Get a reference and creation date of an artwork.
get_artwork(artwork(Uri, Reference, Date)) :-
    rdf(Uri, rdf:type, edm:'ProvidedCHO'),
    artwork_reference(Uri, Reference),
    artwork_date(Uri, Date).

% get and cleanup possible reference
artwork_reference(Uri,CleanReference) :-
    rdf(Uri, skos:note, literal(lang(_, Reference))),
    sanitize(Reference, CleanReference),
    debug(reference, 'skos:note ~p', [Reference]).

% get and cleanup date created
artwork_date(Uri, Date) :-
    rdf(Uri, dcterms:created, literal(Date)),
    debug(date, 'dcterms:created: ~p', [Date]).

%%	sanitize_title(+Dirty, -Clean)
%
%	Make a title lowercase, remove spaces, only consider the part
%	before the slash (removing possible added publisher/author) and
%	remove strange characters.
sanitize_title(Dirty, Clean) :-
    debug(sanitize_title, 'Parsing: ~p', [Dirty]),
    downcase_atom(Dirty, Low),
    normalize_space(atom(Normal), Low),
    atom_codes(Normal, NormalCodes),
    phrase(title_to_slash(SlashedTitle), NormalCodes, _Rest),
    phrase(only_characters(SanitizedCodes), SlashedTitle, _Rest2),
    atom_codes(Clean, SanitizedCodes),
    debug(sanitize_title, 'Cleaned title: ~p', [Clean]).

%%	sanitize(+Dirty, -Clean)
%
%	Make a string lowercase, remove spaces and remove strange
%	characters.
sanitize(Dirty, Clean) :-
    debug(sanitize, 'Parsing: ~p', [Dirty]),
    downcase_atom(Dirty, Low),
    normalize_space(atom(Normal), Low),
    atom_codes(Normal, NormalCodes),
    phrase(only_characters(SanitizedCodes), NormalCodes, _Rest2),
    atom_codes(Clean, SanitizedCodes),
    debug(sanitize, 'Cleaned title: ~p', [Clean]).

title_to_slash(Title) -->
    string(Title), "/", !.
title_to_slash(Title) -->
    string(Title), eos, !.

only_characters([H|T]) -->
	character_or_digit(H), !,
	only_characters(T).
only_characters(T) -->
	no_character, !,
	only_characters(T).
only_characters([]) -->
	[].

character_or_digit(C) -->
	[C],
	!,
	{ atom_codes(Character, [C]),
	  member(Character, [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z])
	}.
character_or_digit(D) -->
	[D],
	!,
	{ code_type(D, digit)
	}.

no_character -->
    [_C].
