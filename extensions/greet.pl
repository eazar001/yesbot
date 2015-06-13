
:- module(greet, [greet/1]).

:- use_module(dispatch).
:- use_module(utilities).
:- use_module(parser).
:- use_module(library(persistency)).
:- use_module(library(list_util)).
:- use_module(library(lambda)).

:- dynamic status/1.
:- dynamic stored/1.

greet_limit(60).


:- persistent
     greeted(nick:atom).

:- persistent
     current_name(name:atom).


chan("##prolog").

greeting("Howdy partner! It appears that noone has spoken to you in \c
  <ROBOTIC VOICE> 1 </ROBOTIC VOICE> minute(s)!!! \c
  I just wanted to let you know, this is a slow moving \c
  channel. Leave the window open for 30 minutes or so and somebody is \c
  likely to notice and come around. Happy prologgin'!").


%% greet(+Msg) is det.
%
% Evaluate the entire goal with a lock on the database.
greet(Msg) :-
  with_mutex(greet_db, greet_(Msg)).


greet_(Msg) :-
  db_attach('extensions/greets.db', []),
  store_names(Msg),
  chan(Chan),
  % Listen for joins
  Msg = msg(Prefix, "JOIN", [Chan]),
  prefix_id(Prefix, Nick, _, _),
  % If the greeting time hasn't been intialized, do so.
  \+status(initialized),
  thread_create(idle(Nick), _Id, [alias(idle), detached(true)]),
  asserta(status(initialized)).

% If the timer has already been initialized, pass the message to idle/1
greet_(Msg) :-
  status(initialized),
  Msg = msg(Prefix, "JOIN", [Chan]),
  prefix_id(Prefix, Nick, _, _),
  thread_send_message(idle, Msg, []).


%  Kill idle waiting process if someone speaks, and the speaker is a current name.
idle(Nick) :-
  chan(Chan),
  greet_limit(Limit),
  alarm(Limit, greet_and_kill(Nick), _),
  repeat,
    thread_get_message(Msg),
    % If anyone other than the newcomer speaks cancel the greeting directive
    Msg = msg(Prefix, "PRIVMSG", [Chan], _),
    prefix_id(Prefix, Name, _, _),
    Name \= Nick,
    current_name(Name),
    retractall(status(_)), !.
    

greet_and_kill(Nick) :-
  atom_string(N, Nick),
  \+current_name(N),
  chan(Chan),
  greeting(Greeting),
  priv_msg(Greeting, Chan),
  assert_current_name(N),
  db_sync(gc),
  retractall(status(_)),
  throw(abort).


store_names(Msg) :-
  (
     \+stored(true)
  ->
     % 353 is code for users present on the channel
     Msg = msg(Prefix, "353", _, Text),
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


