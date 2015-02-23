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


chan("##prolog").
search_form("http://www.swi-prolog.org/pldoc/doc_for?object=").


swi_object_search(Msg) :-
  thread_create(ignore(swi_object_search_(Msg)), _Id, [detached(true)]).


swi_object_search_(Msg) :-
  setup_call_cleanup(
    do_search(Msg, Link, Stream),
    parse_structure(Link, Stream),
    close(Stream)
  ).

do_search(Msg, Link, Stream) :-
  chan(Chan),
  % Message should begin with the prefix ?search
  Msg = msg(_Prefix, "PRIVMSG", [Chan], [63,115,101,97,114,99,104,32|Rest]),
  string_codes(Str, Rest),
  normalize_space(string(C), Str),
  search_form(Form),
  string_concat(Form, C, Link),
  http_open(Link, Stream, [timeout(20)]).


parse_structure(Link, Stream) :-
  chan(Chan),
  load_html(Stream, Structure, [dialect(html5), max_errors(-1)]),
  found_object(Structure, Link, Chan).
  

found_object(Structure, Link, Chan) :-
  one_or_more(Link, Chan, Structure), !.

found_object(Structure, Link, Chan) :-
  (
     zero(Link, Chan, Structure)
  ->
     true
  ;
     send_msg(priv_msg, "No matching object found", Chan)
  ).


zero(Link, Chan, Structure) :-
  xpath_chk(Structure, //dt(@class=pubdef), Table),
  xpath_chk(Table, //strong, Strong),
  Strong = element(strong, _, [Functor]),
  Term =.. [Functor],
  term_string(Term, Object),
  send_msg(priv_msg, Object, Chan),
  write_first_sentence(Structure),
  send_msg(priv_msg, Link, Chan).


one_or_more(Link, Chan, Structure) :-
  xpath_chk(Structure, //dt(@class=pubdef), Table),
  xpath_chk(Table, //strong, Strong),
  xpath_chk(Table, //var, Var),
  Strong = element(strong, _, [Functor]),
  Var = element(var, _, [Vars]),
  split_string(Vars, ",", ",", Args),
  Term =.. [Functor|Args],
  term_string(Term, Str),
  string_codes(Str, Codes),
  delete(Codes, 34, O1),
  (
     selectchk(10, O1, Object)
  ->
     true
  ;
     Object = O1
  ),
  send_msg(priv_msg, Object, Chan),
  write_first_sentence(Structure),
  send_msg(priv_msg, Link, Chan).


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