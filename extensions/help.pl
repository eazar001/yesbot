
:- module(help, [help/1]).

:- use_module(dispatch).

chan("##prolog").

help(Msg) :-
  Msg = msg(_Prefix, "PRIVMSG", [Chan], Rest),
  append(`?help `, Q0, Rest),
  string_codes(Q, Q0),
  normalize_space(string(Query), Q).



%--------------------------------------------------------------------------------%
% Knowledge Base
%--------------------------------------------------------------------------------%




