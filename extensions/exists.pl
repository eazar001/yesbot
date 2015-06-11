
:- module(exists, [exists/1]).

:- use_module(dispatch).
:- use_module(info).
:- use_module(submodules/utils).


target("##prolog", "yesbot").

exists(Msg) :-
  Msg = msg(_Prefix, "PRIVMSG", _, Text),
  append(`?`, Q, Text),
  atom_codes(Query, Q),
  determine_recipient(exists, Msg, Recipient),
  extensions(Es, _),
  (  memberchk(Query, Es)
  -> true
  ;  send_msg(priv_msg, "Not loaded.", Recipient)
  ).


