
:- module(output, [output/1]).

:- use_module(library(irc_client)).

output(Msg) :-
  thread_create(output_(Msg), _, [detached(true)]).


output_(Id-Msg) :-
  Msg = msg(Server, Code, Params, Text),
  format("~s: ~s ~s ~w ~s~n", [Id, Server, Code, Params, Text]).

