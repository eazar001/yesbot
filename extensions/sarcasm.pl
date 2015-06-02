
:- module(sarcasm, [sarcasm/1]).

:- use_module(dispatch).


target("##prolog").

whatsup([ "A preposition denoting a position or direction of superior altitude."
	 ,"Your blood pressure."
	 ,"The sky."
	 ,"Yo!"
	 ,"Nothin' Much."
	 ,"NOT my patience." ]).

public_action([ "reads a book." ]).


% CTCP ACTION
action([1,65,67,84,73,79,78,32|_]).

sarcasm(Msg) :-
  ignore(once(sarcasm_(Msg))).


sarcasm_(Msg) :-
  target(Chan),
  Msg = msg(_Prefix, "PRIVMSG", [Chan], Rest),
  string_codes(Str, Rest),
  normalize_space(string(Norm), Str),
  Norm = "?whatsup",
  (  random_member(0, [0,1])
  -> public_action, fail
  ;  true
  ),
  whatsup(Rs),
  random_member(R, Rs),
  send_msg(priv_msg, R, Chan).


public_action :-
  target(Chan),
  public_action(As),
  random_member(R0, As),
  format(string(Response), `~cACTION ~s~c`, [1,R0,1]),
  send_msg(priv_msg, Response, Chan).


