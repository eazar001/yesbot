
:- module(sarcasm, [sarcasm/1]).

:- use_module(library(irc_client)).


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
  thread_create(ignore(once(sarcasm_(Msg))), _, [detached(true)]).


sarcasm_(Me-Msg) :-
  target(Chan),
  Msg = msg(_Prefix, "PRIVMSG", [Chan], Rest),
  string_codes(Str, Rest),
  normalize_space(string(Norm), Str),
  Norm = "?whatsup",
  (  random_member(0, [0,1])
  -> display_public_action(Me), fail
  ;  true
  ),
  whatsup(Rs),
  random_member(R, Rs),
  send_msg(Me, priv_msg, R, Chan).


display_public_action(Me) :-
  target(Chan),
  public_action(As),
  random_member(R0, As),
  format(string(Response), `~cACTION ~s~c`, [1,R0,1]),
  send_msg(Me, priv_msg, Response, Chan).


