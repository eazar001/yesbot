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
  % Message should begin with the prefix ?search
  Msg = msg(_Prefix, "PRIVMSG", [Chan], [63,115,101,97,114,99,104,32|C]),
  search_form(Form),
  string_codes(O, C),
  string_concat(Form, O, Link),
  http_open(Link, Stream, [timeout(20)]),
  load_html(Stream, Structure, [dialect(html5)]),
  (
    xpath_chk(Structure, //dt(@class=pubdef), Table) ->
      xpath_chk(Table, //strong, Strong),
      xpath_chk(Table, //var, Var),
      Strong = element(strong, _, [Functor]),
      Var = element(var, _, [Vars]),
      split_string(Vars, ",", ",", Args),
      Term =.. [Functor|Args],
      term_string(Term, Str),
      string_codes(Str, Codes),
      delete(Codes, 34, O1),
      selectchk(10, O1, Object),
      send_msg(priv_msg, Object, Chan),
      send_msg(priv_msg, Link, Chan)
    ;
      send_msg(priv_msg, "No matching object found", Chan)
  ).

  