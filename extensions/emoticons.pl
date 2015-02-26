:- module(emoticons, [emoticons/1]).

:- use_module(dispatch).
:- use_module(submodules/emoticon_lib).

chan("##prolog").

emoticons(Msg) :-
  chan(Chan),
  Msg = msg(_Prefix, "PRIVMSG", [Chan], [63,101,109,111,116,101,32|Rest]),
  string_codes(Str, Rest),
  normalize_space(codes(C), Str),
  once(emote(Eng, C, [])),
  send_msg(priv_msg, Eng, Chan).


