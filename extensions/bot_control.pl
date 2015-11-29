:- module(bot_control, [bot_control/1]).

:- use_module(library(irc_client)).
:- use_module(config).


chan("##prolog").


bot_control(Msg) :-
  thread_create(ignore(bot_control_(Msg)), _, [detached(true)]).

bot_control_(Id-Msg) :-
  chan(Chan),
  Msg = msg(_Prefix, "PRIVMSG", _, Codes),
  string_codes(String, Codes),
  normalize_space(atom(Text), String),
  atom_concat(Cmd, Arg, Text),
  control(Id-Chan, Cmd, Arg).


control(Me-Chan, '?stop ', Arg) :-
  (  Arg \== bot_control,
     (  extensions(Current, _)
     ;  sync_extensions(Current, _)
     ),
     memberchk(Arg, Current),
     valid_extensions([Arg])
  -> true
  ;  priv_msg(Me, "I'm afraid I can't do that, Dave", Chan),
     !, fail
  ),
  remove_extensions([Arg]),
  format(string(Report), "Okay, stopping ~s", [Arg]),
  priv_msg(Me, Report, Chan).

control(Me-Chan, '?start ', Arg) :-
  (  Arg \== bot_control,
     valid_extensions([Arg]),
     (  extensions(Current, _)
     ;  sync_extensions(Current, _)
     ),
     \+memberchk(Arg, Current)
  -> true
  ;  priv_msg(Me, "I'm afraid I can't do that, Dave", Chan),
     !, fail
  ),
  add_new_extensions([Arg]),
  format(string(Report), "Okay, starting ~s", [Arg]),
  priv_msg(Me, Report, Chan).

control(Me-Chan, '?reboot', '') :-
  priv_msg(Me, "Okay, rebooting.", Chan),
  thread_signal(irc, throw(abort)).

control(Me-Chan, '?off', '') :-
  priv_msg(Me, "Okay, shutting down.", Chan),
  thread_signal(irc, halt).
