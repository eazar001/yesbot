
:- module(exists, [exists/1]).

:- use_module(dispatch).
:- use_module(submodules/utils).


target("##prolog", "yesbot").

exists(Msg) :-
  Msg = msg(_Prefix, "PRIVMSG", _, Text),
  append(`?`, Q, Text),
  atom_codes(Query, Q),
  determine_recipient(exists, Msg, Recipient),
  core:extensions(Es, _),
  (  memberchk(Query, Es)
  -> send_msg(priv_msg, "Loaded.", Recipient)
  ;  send_msg(priv_msg, "Not loaded.", Recipient)
  ).


