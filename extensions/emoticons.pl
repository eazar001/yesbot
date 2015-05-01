:- module(emoticons, [emoticons/1]).

:- use_module(dispatch).
:- use_module(submodules/emoticon_lib).
:- use_module(submodules/utils).


target("##prolog", "yesbot").

emoticons(Msg) :-
  Msg = msg(_Prefix, "PRIVMSG", _, [63,101,109,111,116,101,32|Rest]),
  determine_recipient(emoticons, Msg, Rec),
  string_codes(Str, Rest),
  normalize_space(codes(C), Str),
  once(emote(Eng, C, [])),
  send_msg(priv_msg, Eng, Rec).


