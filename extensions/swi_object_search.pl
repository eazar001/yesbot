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


chan("##prolog").
search_form("http://www.swi-prolog.org/pldoc/doc_for?object=").
search_sugg("http://www.swi-prolog.org/pldoc/search?for=").

  
swi_object_search(Msg) :-
  thread_create(ignore(swi_object_search_(Msg)), _Id, [detached(true)]).


swi_object_search_(Msg) :-
  setup_call_cleanup(
    do_search(Msg, Link, Query, Stream),
    parse_structure(Link, Query, Stream),
    close(Stream)
  ).

do_search(Msg, Link, Query, Stream) :-
  chan(Chan),
  % Message should begin with the prefix ?search
  Msg = msg(_Prefix, "PRIVMSG", [Chan], [63,115,101,97,114,99,104,32|Rest]),
  atom_codes(A, Rest),
  uri_encoded(query_value, A, Encoded),
  atom_string(Encoded, Str),
  normalize_space(string(Query), Str),
  search_form(Form),
  string_concat(Form, Query, Link),
  http_open(Link, Stream, [timeout(20), status_code(_)]).


parse_structure(Link, Query, Stream) :-
  chan(Chan),
  load_html(Stream, Structure, [dialect(html5), max_errors(-1)]),
  found_object(Structure, Link, Query, Chan).


found_object(Structure, Link, Query, Chan) :-
  (
     found(Link, Chan, Structure)
  ->
     true
  ;
     send_msg(priv_msg, "No matching object found. ", Chan),
     try_again(Query)
  ).


found(Link, Chan, Structure) :-
  xpath_chk(Structure, //dt(@class=pubdef,normalize_space), Table),
  send_msg(priv_msg, Table, Chan),
  write_first_sentence(Structure),
  send_msg(priv_msg, Link, Chan).


% Let's try to search for possible matches if user attempts incorrect query
try_again(Query) :-
  chan(Chan),
  search_sugg(Form),
  string_codes(Query, Q),
  get_functor(Q, Fcodes),
  % Get the pure functor from the query
  string_codes(Functor, Fcodes),
  string_concat(Form, Functor, Retry),
  http_open(Retry, Stream, []),
  load_html(Stream, Structure, []),
  % Find all solutions and write them on one line to avoid flooding
  findall(Sugg, find_candidate(Structure, Fcodes, Sugg), Ss),
  Ss = [_|_],
  atomic_list_concat(Ss, ', ', A0),
  atom_concat('Perhaps you meant one of these: ', A0, A),
  atom_string(Feedback, A),
  send_msg(priv_msg, Feedback, Chan).


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


write_first_sentence(Structure) :-
  chan(Chan),
  xpath_chk(Structure, //dd(@class=defbody,normalize_space), D),
  atom_codes(D, Codes),
  (  first_sentence(Sentence, Codes, _)
  -> send_msg(priv_msg, Sentence, Chan)
  ;  send_msg(priv_msg, Codes, Chan)
  ).


first_sentence(`.`) --> `.`, !.
first_sentence([C|Rest]) -->
  [C], first_sentence(Rest).


get_functor(Original, Functor) :-
  (  append(Functor, [47|_], Original)
  -> true
  ;  Functor = Original
  ).


