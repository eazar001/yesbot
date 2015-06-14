
:- module(greet, [greet/1]).

:- use_module(dispatch).
:- use_module(utilities).
:- use_module(parser).
:- use_module(library(persistency)).
:- use_module(library(list_util)).
:- use_module(library(lambda)).

:- dynamic status/1.
:- dynamic stored/1.
:- dynamic in_channel/1.

greet_limit(5).



% TBD: Repurpose this module for use with 'state.pl' and similar consructs
% TBD: Add comments to this code!

:- persistent
     greeted(nick:atom).

:- persistent
     current_name(name:atom).


chan("##prolog").

greeting("Howdy partner! It appears that noone has spoken to you in \c
  <ROBOTIC VOICE> ONE </ROBOTIC VOICE> minute(s)!!! \c
  I just wanted to let you know, this is a slow moving \c
  channel. Leave the window open for 30 minutes or so and somebody is \c
  likely to notice and come around. Happy prologgin'!").


%% greet(+Msg) is det.
%
% Evaluate the entire goal with a lock on the database.
greet(Msg) :-
  with_mutex(greet_db, receive(Msg)).


%--------------------------------------------------------------------------------% 
% Reception
%--------------------------------------------------------------------------------%

%% handle joins
receive(Msg) :-
  \+status(initialized),
  db_attach('extensions/greets.db', []),
  store_names(Msg),
  chan(Chan),
  % Listen for joins
  valid_join(Chan, Msg),
  % If the greeting time hasn't been intialized, do so.
  initialize(Msg).

%% handle privmsgs
receive(Msg) :-
  chan(Chan),
  status(initialized),
  valid_speech(Chan, Msg),
  transmit(die),
  in_channel(Nick),
  assert_current_name(Nick),
  retractall(status(_)),
  retractall(in_channel(_)).

%% handle parts
receive(Msg) :-
  chan(Chan),
  status(initialized),
  valid_part(Chan, Msg),
  transmit(die),
  retractall(in_channel(_)),
  retractall(status(_)).
  
  

%--------------------------------------------------------------------------------%
% Transmission and Initialization
%--------------------------------------------------------------------------------%


initialize(msg(Prefix, _, _)) :-
  prefix_id(Prefix, Nick, _, _),
  thread_create(wait(Nick), _Id, [alias(wait), detached(true)]),
  asserta(status(initialized)).


transmit(die) :-
  thread_signal(wait, throw(abort)).


%--------------------------------------------------------------------------------%
% Validation
%--------------------------------------------------------------------------------%


valid_join(Chan, msg(Prefix, "JOIN", [Chan])) :-
  prefix_id(Prefix, Nick, _, _),
  atom_string(N, Nick),
  \+current_name(N),
  \+in_channel(_),
  asserta(in_channel(N)).


valid_speech(Chan, msg(Prefix, "PRIVMSG", [Chan], _)) :-
  in_channel(Nick),
  prefix_id(Prefix, T, _, _),
  atom_string(Talker, T),
  % The newcomer who joined and the subsequent talker are not the same
  % The talker must also be a regular
  Nick \= Talker,
  current_name(Talker).

%% be sure that this won't cause a bug

valid_part(Chan, msg(Prefix, "PART", [Chan], _)) :-
  prefix_id(Prefix, Nick, _, _),
  atom_string(N, Nick),
  in_channel(N).


%--------------------------------------------------------------------------------%
% Waiting/Procedures
%--------------------------------------------------------------------------------%


%  Kill idle waiting process if someone speaks, and the speaker is a current name.
wait(Nick) :-
  greet_limit(Limit),
  alarm(Limit, greet_and_kill(Nick), _),
  thread_get_message(_).
    

greet_and_kill(Nick) :-
  atom_string(N, Nick),
  \+current_name(N),
  chan(Chan),
  greeting(Greeting),
  priv_msg(Greeting, Chan),
  assert_current_name(N),
  db_sync(gc),
  retractall(status(_)),
  retractall(in_channel(_)),
  throw(abort).


store_names(Msg) :-
  (
     \+stored(true)
  ->
     % 353 is code for users present on the channel
     Msg = msg(_Prefix, "353", _, Text),
     split(Text, 32, Names),
     maplist(add_name, Names),
     asserta(stored(true))
  ;
     true
  ).


add_name(Codes) :-
  atom_codes(Atom, Codes),
  (  \+current_name(Atom)
  -> assert_current_name(Atom)
  ;  true
  ).


%% nick_exists(+Nick:string) is semidet.
%
% Takes a string input for a Nick and determines if this name is in the db.
nick_exists(Nick) :-
  atom_string(Name, Nick),
  current_name(Name).

