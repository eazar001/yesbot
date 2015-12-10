:- module(prolog_eval, [prolog_eval/1]).

:- use_module(library(irc_client)).
:- use_module(library(pengines)).
:- use_module(library(solution_sequences)).
:- use_module(library(lambda)).
:- use_module(submodules/utils).

:- dynamic pengine_port/1.

target("##prolog", "yesbot").

prolog_eval(Msg) :-
  thread_create(ignore(prolog_eval_(Msg)), _, [detached(true)]).


prolog_eval_(Me-Msg) :-
  Msg = msg(_Prefix, "PRIVMSG", _, Text),
  append(`?- `, Rest, Text),
  string_codes(String, Rest),
  term_string(Goal, String, [variable_names(Vars)]),
  pengine_port(Port),
  format(string(Engine), "http://localhost:~d", [Port]),
  pengine_rpc(Engine, findall(Vars, limit(7, call(Goal)), Sols)),
  determine_recipient(prolog_eval, Msg, Recip),
  evaluate(Me, Recip, Sols).


evaluate(Me, Recip, []) :-
  priv_msg(Me, "no.", Recip).

evaluate(Me, Recip, [[]]) :-
  priv_msg(Me, "yes.", Recip).

evaluate(Me, Recip, Sols) :-
  maplist(term_string, Sols, Strings),
  maplist(string_codes, Strings, Codes),
  maplist(\Code^F^(Code=[_|R], format_sols(R,F)), Codes, FormattedCodes),
  maplist(\Msg^(priv_msg(Me, Msg, Recip), sleep(1)), FormattedCodes).


format_sols([_], []).
format_sols([X|Xs], [X|Ys]) :-
  format_sols(Xs, Ys).
