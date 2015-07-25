%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                %
%                                                                                %
% Author: Ebrahim Azarisooreh                                                    %
% E-mail: ebrahim.azarisooreh@gmail.com                                          %
% IRC Nick: eazar001                                                             %
% Title: Yes-Bot Prolog Object Search Extension                                  %
% Description: Predicate/Object Search Extension for ##prolog                    %
%                                                                                %
%                                                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- module(swi_object_search, [swi_object_search/1]).

:- use_module(dispatch).
:- use_module(library(http/http_open)).
:- use_module(library(sgml)).
:- use_module(library(xpath)).
:- use_module(library(uri)).
:- use_module(submodules/docs).
:- use_module(submodules/utils).
:- use_module(parser).


%--------------------------------------------------------------------------------%
% Main Interface
%--------------------------------------------------------------------------------%


% All search results are delivered to ##prolog
% search_form is used to search for predicates and functions. (with suggestions)
% search_form_lib is used to search for libraries. (without suggestions)
% search_sugg is the url used for deriving suggestions.

target("##prolog", "yesbot").


swi_object_search(Msg) :-
  ignore(swi_object_search_(Msg)).


swi_object_search_(Msg) :-
  setup_call_cleanup(
    do_search(Msg, Link, Query, Quiet, Rec, Stream),
    parse_structure(Link, Query, Quiet, Rec, Stream),
    close(Stream)
  ).


%--------------------------------------------------------------------------------%


% TBD: Add mirror switch on search request timeout. (US mirror vs. NL mirror)
% TBD: Add more intelligent/interactive/helpful version of manual and library
% searchess

%% do_search(+Msg, -Link, -Query, -Quiet, -Rec, -Stream) is semidet.
%
% do_search/6 listens for the appropriate search patterns and arguments.
% Certain patterns will correspond with implicit or explicit quietness options.
% These patterns will also generate specific links for handling different types of
% situations a user might throw at yesbot. The link and quietness information will
% be unified so that this information can be passed to parse_structure/4.

do_search(Msg, Link, Query, Quiet, Rec, Stream) :-
  % Message should begin with the prefix ?search
  Msg = msg(_Prefix, "PRIVMSG", _, [63,115,101,97,114,99,104,32|Cs]),
  determine_recipient(swi_object_search, Msg, Rec),
  atom_codes(A0, Cs),
  normalize_space(codes(Tail), A0),
  (
     % ?search library(...)
     Tail = [108,105,98,114,97,114,121,40|T],
     append(Rest, `)`, T),
     Quiet = lib, !
  ;
     % ?search manual(...)
     Tail = [109, 97, 110, 117, 97, 108, 40|T],
     append(Rest, `)`, T),
     Quiet = man, !
  ;
     % search -q
     Tail = [45,113,32|Rest],
     Quiet = q, !
  ;
     % search -qq
     Tail = [45,113,113,32|Rest],
     Quiet = qq, !
  ;
     % Anything else
     Tail = Rest,
     Quiet = q0
  ),
  atom_codes(A, Rest),
  uri_encoded(query_value, A, Encoded),
  atom_string(Encoded, Str),
  normalize_space(string(Query), Str),
  % Determine appropriate search link
  (
     Quiet = lib
  ->
     % Will do a library specific search with no suggestions
     format(string(Link),
       "http://www.swi-prolog.org/pldoc/doc/swi/library/~s.pl", [Query])
  ;
     Quiet = man
  ->
     % Will do a manual specific seaerch with no suggestions
     format(string(Link),
       "http://www.swi-prolog.org/pldoc/man?section=~s", [Query])
  ;
     % Will do a regular search with suggestions
     format(string(Link),
       "http://www.swi-prolog.org/pldoc/doc_for?object=~s", [Query])
  ),
  % Get the results from a search using the appropriate link from above
  http_open(Link, Stream, [timeout(20), status_code(Status)]),
  (
     % Non-lib/man searches
     \+member(Quiet, [lib, man]), !
  ;
     % Lib/man searches with successful requests
     Status = 200, !
  ;
     % Lib/man searches that fail to generate a page
     Status = 404,
     priv_msg("No matching object found.", Rec),
     fail
  ).


%% parse_structure(+Link, +Query, +Quiet, +Rec, +Stream) is semidet.
%
% Load incoming search information as an HTML structure. Scrape information from
% the HTML and determine whether or not there is a match for the user's request.
% found_object/5 will perform the necessary side-effects depending on the
% resolution.

parse_structure(Link, Query, Quiet, Rec, Stream) :-
  load_html(Stream, Structure, [dialect(html5), max_errors(-1)]),
  found_object(Structure, Link, Query, Quiet, Rec).


%% found_object(+Structure, +Link, +Query, +Quiet, +Rec) is det.
%
% If a matching object was found for the user's requests then the necessary
% side-effects would have been performed by found/4. If not, then the user is
% apprised, and a new search is performed by try_again/1. The new search will
% attempt to find any search suggestion to help direct the user.

found_object(Structure, Link, Query, Quiet, Rec) :-
  (
     found(Link, Rec, Quiet, Structure)
  ->
     true
  ;
     priv_msg("No matching object found. ", Rec),
     % Try alternative suggestions only if search is not library or man-specific
     \+memberchk(Quiet, [lib, man]),
     try_again(Query, Rec)
  ).


%% found(+Link, +Rec, +Quiet, +Structure) is semidet.
%
% Determine if relevant information is found with respect to the user's query.
% Display formats vary according to quietness options.

found(Link, Rec, lib, Structure) :-
  xpath_chk(Structure, //title(normalize_space), Title),
  priv_msg(Title, Rec),
  priv_msg(Link, Rec).

found(Link, Rec, man, Structure) :-
  xpath_chk(Structure, //span(@class='sec-title', normalize_space), Title),
  priv_msg(Title, Rec),
  priv_msg(Link, Rec).

found(Link, Rec, Qlevel, Structure) :-
  xpath_chk(Structure, //dt(@class=pubdef, normalize_space), Table),
  (
     Qlevel = q0,
     priv_msg(Table, Rec),
     write_first_sentence(Structure, Rec),
     priv_msg(Link, Rec)
  ;
     Qlevel = q,
     priv_msg(Table, Rec),
     write_first_sentence(Structure, Rec)
  ;
     Qlevel = qq,
     priv_msg(Table, Rec)
  ).


%% try_again(+Query, +Rec) is semidet.
%
% Attempts to search for possible matches if a user has entered a query that
% does not lead to a direct match. Possible results are displayed to the user
% in channel.

try_again(Query, Rec) :-
  get_functor(string_codes $ Query, Fcodes),
  % Get the pure functor from the query
  string_codes(Functor, Fcodes),
  format(string(Retry),
    "http://www.swi-prolog.org/pldoc/search?for=~s", [Functor]),
  setup_call_cleanup(
    http_open(Retry, Stream, [timeout(20)]),
    (
       load_html(Stream, Structure, []),
       % Find all solutions and write them on one line to avoid flooding
       findall(Sugg, find_candidate(Structure, Fcodes, Sugg), Ss),
       (
	  % We have suggestions
	  Ss = [_|_],
	  findnsols(10, C, try_other_candidate(Structure, [39], C), Last),
	  maplist(list_to_set, [Ss, Last], [S1, S2]),
	  union(S1, S2, L), !
       ;
	  % No initial suggestions, so let's find some
          Ss = [],
	  findnsols(10, C, try_other_candidate(Structure, [39,58], C), Cs),
	  (  Cs = []
	  -> findnsols(10, C, try_other_candidate(Structure, [39], C), L)
	  ;  L = Cs
	  )
       ),
       L = [_|_],
       atomic_list_concat(L, ', ', AtomList),
       format(string(Feedback), "Perhaps you meant one of these: ~a", [AtomList]),
       priv_msg(Feedback, Rec)
    ),
    close(Stream)
  ).


%% find_candidate(+Structure, +Fcodes, -Sugg) is nondet.
%
% Scrape candidates that match the base functors. Return a possible Suggestion.

find_candidate(Structure, Fcodes, Sugg) :-
  xpath(Structure, //tr(@class=public), Row),
  xpath(Row, //a(@href=Path, normalize_space), _),
  append(`/pldoc/doc_for?object=`, Functor_Arity, atom_codes $ Path),
  % Functor must match candidate functors
  get_functor(Functor_Arity, Fcodes),
  atom_codes(Atom, Functor_Arity),
  uri_encoded(query_value, A, Atom),
  atom_string(A, Sugg).


%% try_other_candidate(+Structure, +Invalids, -Sugg) is nondet.
%
% This should be the end of the line. These are suggestions that are tried when
% the base functor doesn't have a match. We now rely on the first 10 results of
% of the search page. Invalids are invalid markings that should count against
% candidate selection.

try_other_candidate(Structure, Invalids, Sugg) :-
  xpath(Structure, //tr(@class=public), Row),
  xpath(Row, //a(@href=Path, normalize_space), _),
  append(`/pldoc/doc_for?object=`, Functor_Arity, atom_codes $ Path),
  intersection(Invalids, Functor_Arity, []),
  atom_codes(Atom, Functor_Arity),
  uri_encoded(query_value, A, Atom),
  atom_string(A, Sugg).
  

%% write_first_sentence(+Structure, +Rec) is semidet.
%
% Search for a dd tag that's classified as "defbody" (definition body), attempt
% to extract the first sentence and display it to the channel. If the first
% sentence isn't successfully parsed, then return as much as possible.

write_first_sentence(Structure, Rec) :-
  xpath_chk(Structure, //dd(@class=defbody,normalize_space), D),
  atom_codes(D, Codes),
  (  sentence(Sentence, Codes, _)
  -> priv_msg(Sentence, Rec)
  ;  priv_msg(Codes, Rec)
  ).


%% get_functor(+Original, -Functor) is det.
%
% This predicate will attempt to extract a base functor from some predicate/N
% pattern. If the normal pattern is not successfully parsed, then the original
% input will be assumed a functor. Search will proceed in this manner.

get_functor(Original, Functor) :-
  (  get_functor_(Functor, Original, _)
  -> true
  ;  Original = Functor
  ).

get_functor_([]) --> `/`, !.
get_functor_([C|Rest]) -->
  [C], get_functor_(Rest).


