
:- module(output, [output/1]).

:- use_module(library(irc_client)).

output(Msg) :-
  thread_create(output_(Msg), _, [detached(true)]).


output_(Id-Msg) :-
  Msg = msg(Prefix, Cmd, Params, Trailer),
  format("~s: ~s ~s ~w ~s~n", [Id, Prefix, Cmd, Params, Trailer]).

output_(Id-Msg) :-
  Msg = msg(Prefix, Cmd, Params),
  \+ is_list(Cmd), !,
  format("~s: ~s ~s ~w~n", [Id, Prefix, Cmd, Params]).

output_(Id-Msg) :-
  Msg = msg(Cmd, Params, Trailer),
  format("~s: ~s ~w ~s~n", [Id, Cmd, Params, Trailer]).

output_(Id-Msg) :-
  Msg = msg(Cmd, Params),
  format("~s: ~s ~w~n", [Id, Cmd, Params]).

