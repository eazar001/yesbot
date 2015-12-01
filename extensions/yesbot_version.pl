:- module(yesbot_version, [yesbot_version/1]).

:- use_module(library(irc_client)).
:- use_module(submodules/utils).

:- dynamic yesbot_vsn/1.

target("#testeazarbot", "eazarbot").

yesbot_version(Msg) :-
  thread_create(ignore(yesbot_version_(Msg)), _, [detached(true)]).

yesbot_version_(Me-Msg) :-
  Msg = msg(_Prefix, "PRIVMSG", _, Codes),
  atom_codes(Atom, Codes),
  normalize_space(string("?version"), Atom),
  determine_recipient(yesbot_version, Msg, Recipient),
  yesbot_vsn(Vsn),
  format(string(Report), "yesbot version ~a", [Vsn]),
  priv_msg(Me, Report, Recipient).
