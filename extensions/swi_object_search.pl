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


%--------------------------------------------------------------------------------%
% Main Interface
%--------------------------------------------------------------------------------%


% All search results are delivered to ##prolog
% search_form is used to search for predicates and functions. (with suggestions)
% search_form_lib is used to search for libraries. (without suggestions)
% search_sugg is the url used for deriving suggestions.

chan("##prolog").
search_form("http://www.swi-prolog.org/pldoc/doc_for?object=").
search_form_lib("http://www.swi-prolog.org/pldoc/doc/swi/library/").
search_sugg("http://www.swi-prolog.org/pldoc/search?for=").


swi_object_search(Msg) :-
  thread_create(ignore(swi_object_search_(Msg)), _Id, [detached(true)]).


swi_object_search_(Msg) :-
  setup_call_cleanup(
    do_search(Msg, Link, Query, Quiet, Stream),
    parse_structure(Link, Query, Quiet, Stream),
    close(Stream)
  ).


%--------------------------------------------------------------------------------%


% TBD: This predicate needs to be refactored.

%% do_search(+Msg, -Link, -Query, -Quiet, -Stream) is semidet.
%
% do_search/5 listens for the appropriate search patterns and arguments.
% Certain patterns will correspond with implicit or explicit quietness options.
% These patterns will also generate specific links for handling different types of
% situations a user might throw at yesbot. The link and quietness information will
% be unified so that this information can be passed to parse_structure/4.

do_search(Msg, Link, Query, Quiet, Stream) :-
  chan(Chan),
  % Message should begin with the prefix ?search
  Msg = msg(_Prefix, "PRIVMSG", [Chan], [63,115,101,97,114,99,104,32|Cs]),
  atom_codes(A0, Cs),
  normalize_space(codes(Tail), A0),
  (
     % ?search library(...)
     Tail = [108,105,98,114,97,114,121,40|T],
     append(Rest, `)`, T),
     Quiet = lib, !
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
     search_form_lib(Form),
     string_concat(Form, Query, Initial),
     string_concat(Initial, ".pl", Link)
  ;
     % Will do a regular search with suggestions
     search_form(Form),
     string_concat(Form, Query, Link)
  ),
  % Get the results from a search using the appropriate link from above
  http_open(Link, Stream, [timeout(20), status_code(Status)]),
  (
     % Non-lib searches
     Quiet \= lib, !
  ;
     % Lib searches with successful requests
     Status = 200, !
  ;
     % Lib searches that fail to generate a page
     Status = 404,
     send_msg(priv_msg, "No matching object found.", Chan),
     fail
  ).


%% parse_structure(+Link, +Query, +Quiet, +Stream) is semidet.
%
% Load incoming search information as an HTML structure. Scrape information from
% the HTML and determine whether or not there is a match for the user's request.
% found_object/5 will perform the necessary side-effects depending on the
% resolution.

parse_structure(Link, Query, Quiet, Stream) :-
  chan(Chan),
  load_html(Stream, Structure, [dialect(html5), max_errors(-1)]),
  found_object(Structure, Link, Query, Quiet, Chan).


%% found_object(+Structure, +Link, +Query, +Quiet, +Chan) is det.
%
% If a matching object was found for the user's requests then the necessary
% side-effects would have been performed by found/4. If not, then the user is
% apprised, and a new search is performed by try_again/1. The new search will
% attempt to find any search suggestion to help direct the user.

found_object(Structure, Link, Query, Quiet, Chan) :-
  (
     found(Link, Chan, Quiet, Structure)
  ->
     true
  ;
     send_msg(priv_msg, "No matching object found. ", Chan),
     try_again(Query)
  ).


%% found(+Link, +Chan, +Quiet, +Structure) is semidet.
%
% Determine if relevant information is found with respect to the user's query.
% Display formats vary according to quietness options.

found(Link, Chan, lib, Structure) :-
  xpath_chk(Structure, //title(normalize_space), Title),
  send_msg(priv_msg, Title, Chan),
  send_msg(priv_msg, Link, Chan).

found(Link, Chan, q0, Structure) :-
  xpath_chk(Structure, //dt(@class=pubdef,normalize_space), Table),
  send_msg(priv_msg, Table, Chan),
  write_first_sentence(Structure),
  send_msg(priv_msg, Link, Chan).

found(_, Chan, q, Structure) :-
  xpath_chk(Structure, //dt(@class=pubdef,normalize_space), Table),
  send_msg(priv_msg, Table, Chan),
  write_first_sentence(Structure).

found(_, Chan, qq, Structure) :-
  xpath_chk(Structure, //dt(@class=pubdef,normalize_space), Table),
  send_msg(priv_msg, Table, Chan).


%% try_again(+Query) is semidet.
%
% Attempts to search for possible matches if a user has entered a query that
% does not lead to a direct match. Possible results are displayed to the user
% in channel.

try_again(Query) :-
  chan(Chan),
  search_sugg(Form),
  string_codes(Query, Q),
  get_functor(Q, Fcodes),
  % Get the pure functor from the query
  string_codes(Functor, Fcodes),
  string_concat(Form, Functor, Retry),
  setup_call_cleanup(
    http_open(Retry, Stream, [timeout(20)]),
    (
       load_html(Stream, Structure, []),
       % Find all solutions and write them on one line to avoid flooding
       findall(Sugg, find_candidate(Structure, Fcodes, Sugg), Ss),
       (
	  % We have suggestions
	  Ss = [_|_],
	  L  = Ss, !
       ;
	  % No initial suggestions, so let's find some
          Ss = [],
          findnsols(10, C, try_other_candidate(Structure, C), L)
       ),
       L = [_|_],
       atomic_list_concat(L, ', ', A0),
       atom_concat('Perhaps you meant one of these: ', A0, A),
       atom_string(Feedback, A),
       send_msg(priv_msg, Feedback, Chan)
    ),
    close(Stream)
  ).


%% find_candidate(+Structure, +Fcodes, -Sugg) is nondet.
%
% Scrape candidates that match the base functors. Return a possible Suggestion.

find_candidate(Structure, Fcodes, Sugg) :-
  xpath(Structure, //tr(@class=public), Row),
  xpath(Row, //a(@href=Path, normalize_space), _),
  atom_codes(Path, Codes),
  append(`/pldoc/doc_for?object=`, Functor_Arity, Codes),
  % Functor must match candidate functors
  get_functor(Functor_Arity, Fcodes),
  atom_codes(Atom, Functor_Arity),
  uri_encoded(query_value, A, Atom),
  atom_string(A, Sugg).


%% try_other_candidate(+Structure, +Query, -Sugg) is nondet.
%
% This should be the end of the line. These are suggestions that are tried when
% the base functor doesn't have a match. We now rely on the first 10 results of
% of the search page.

try_other_candidate(Structure, Sugg) :-
  xpath(Structure, //tr(@class=public), Row),
  xpath(Row, //a(@href=Path, normalize_space), _),
  atom_codes(Path, Codes),
  append(`/pldoc/doc_for?object=`, Functor_Arity, Codes),
  intersection([39,58], Functor_Arity, []),
  atom_codes(Atom, Functor_Arity),
  uri_encoded(query_value, A, Atom),
  atom_string(A, Sugg).
  

%% write_first_sentence(+Structure) is semidet.
%
% Search for a dd tag that's classified as "defbody" (definition body), attempt
% to extract the first sentence and display it to the channel. If the first
% sentence isn't successfully parsed, then return as much as possible.

write_first_sentence(Structure) :-
  chan(Chan),
  xpath_chk(Structure, //dd(@class=defbody,normalize_space), D),
  atom_codes(D, Codes),
  (  sentence(Sentence, Codes, _)
  -> send_msg(priv_msg, Sentence, Chan)
  ;  send_msg(priv_msg, Codes, Chan)
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


