:- module(prolog_eval, [prolog_eval/1]).

:- use_module(library(irc_client)).
:- use_module(library(pengines)).
:- use_module(library(solution_sequences)).

:- dynamic pengine_port/1.


prolog_eval(Msg) :-
  thread_create(ignore(prolog_eval_(Msg)), _, [detached(true)]).


prolog_eval_(Me-Msg) :-
  Msg = msg(_Prefix, "PRIVMSG", ["##prolog"], Text),
  append(`?- `, Rest, Text),
  string_codes(String, Rest),
  term_string(Goal, String),
  term_variables(Goal, Vars),
  pengine_port(Port),
  format(string(Engine), "http://localhost:~d", [Port]),
  pengine_rpc(Engine, findall(Vars, limit(5, call(Goal)), Sols)),
  evaluate(Me, Sols).

evaluate(Me, []) :-
  priv_msg(Me, "no.", "##prolog").

evaluate(Me, [[]]) :-
  priv_msg(Me, "yes.", "##prolog").

evaluate(Me, Sols) :-
  format(string(Solutions), "~w", [Sols]),
  priv_msg(Me, Solutions, "##prolog").
