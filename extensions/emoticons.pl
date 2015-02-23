:- module(emoticons, [emoticons/1]).

:- use_module(dispatch).
:- use_module(submodules/emoticon_lib).

chan("##prolog").

emoticons(Msg) :-
  chan(Chan),
  Msg = msg(_Prefix, "PRIVMSG", [Chan], [63,115,109,105,108,101,121,32|Rest]),
  string_codes(Str, Rest),
  normalize_space(codes(C), Str),
  once(emote(Eng, C, [])),
  send_msg(priv_msg, Eng, Chan).


