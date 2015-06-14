
:- module(greet, [greet/1]).

:- use_module(dispatch).
:- use_module(utilities).
:- use_module(parser).
:- use_module(library(persistency)).
:- use_module(library(list_util)).
:- use_module(library(lambda)).

:- dynamic status/1.
:- dynamic in_channel/1.

:- persistent
     current_name(name:atom).

greet_limit(60).

chan("##prolog").

greeting("Howdy partner! It appears that noone has spoken to you in \c
  <ROBOTIC VOICE> ONE </ROBOTIC VOICE> minute(s)!!! \c
  I just wanted to let you know, this is a slow moving \c
  channel. Leave the window open for 30 minutes or so and somebody is \c
  likely to notice and come around. Happy prologgin'!").


% TBD: Repurpose this module for use with 'state.pl' and similar consructs
% TBD: Add comments to this code!


%% greet(+Msg) is det.
%
% Evaluate the entire goal with a lock on the database. This is probably an
% pessimistic lock, but it's easy and guaranteed. Hence, I'll keep it this way
% unless a change is necessary.

greet(Msg) :-
  with_mutex(greet_db, receive(Msg)).


%--------------------------------------------------------------------------------% 
% Reception
%--------------------------------------------------------------------------------%


%% receive(+Msg:compound) is nondet.
%
% Receives message from IRC server and handles them accordingly.


% Here and unconditional attempt to store names of all currently in the channel
% will be attempted. This goal will always succeed whether or not this attempt
% actually succeeded. Then it will be determined whether or not the waiting 
% process has been initalized. If not this process will be initialized if a new
% user has joined and has spoken.

receive(Msg) :-
  ignore(store_names(Msg)), % Deterministic call of store_names/1
  \+status(initialized),
  chan(Chan),
  % Listen for joins
  valid_join(Chan, Msg),
  % If the greeting time hasn't been intialized, do so.
  initialize(Msg).

% If a recognized user has spoken while the waiting process is still extant,
% then the waiting process will be terminated, and the user associated with the
% waiting process will be added to the recognized users database.

receive(Msg) :-
  chan(Chan),
  status(initialized),
  valid_speech(Chan, Msg),
  transmit(die),
  in_channel(Nick),
  assert_current_name(Nick),
  retractall(status(_)),
  retractall(in_channel(_)).

% If the user parts before the waiting process' timer goes off and gets a chance
% to fire a greeting, then it will be terminated. If the user attempts to join
% again, the timing process will be re-initialized.

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


%% initialize(+Msg:compound) is semidet.
%
% Initialize the waiting process.
initialize(msg(Prefix, _, _, _)) :-
  prefix_id(Prefix, Nick, _, _),
  thread_create(wait(Nick), _Id, [alias(wait), detached(true)]),
  asserta(status(initialized)).

%% transmit(+Command:atom) is det.
%
% Kill the waiting process.
transmit(die) :-
  thread_signal(wait, throw(abort)).


%--------------------------------------------------------------------------------%
% Validation
%--------------------------------------------------------------------------------%


%% valid_join(+Chan:string, +Msg:compound) is semidet.
%
% Considered a valid "join" if a new unrecognized user joins the channel and
% speaks publicly. User will be asserted as "in-channel"

valid_join(Chan, msg(Prefix, "PRIVMSG", [Chan], _)) :-
  prefix_id(Prefix, Nick, _, _),
  atom_string(N, Nick),
  \+current_name(N),
  \+in_channel(_),
  asserta(in_channel(N)).


%% valid_speech(+Chan:string, +Msg:compound) is semidet.
%
% Considered valid "speech" if a newcomer is already marked as "in-channel" and
% the speaker is a recognized user, that of course would imply that the speaker
% is not the newcomer.

valid_speech(Chan, msg(Prefix, "PRIVMSG", [Chan], _)) :-
  in_channel(Nick),
  prefix_id(Prefix, T, _, _),
  atom_string(Talker, T),
  % The newcomer who joined and the subsequent talker are not the same
  % The talker must also be a regular
  Nick \= Talker,
  current_name(Talker).


%% valid_part(+Chan:string, +Msg:compound) is semidet.
%
% True if a newcomer marked as "in-channel" has just left the channel.
valid_part(Chan, msg(Prefix, "PART", [Chan], _)) :-
  prefix_id(Prefix, Nick, _, _),
  atom_string(N, Nick),
  in_channel(N).


%--------------------------------------------------------------------------------%
% Waiting/Procedures
%--------------------------------------------------------------------------------%


%% wait(+Nick:string)
%
% Initiate an idle waiting process that will fire a greeting after a time limit,
% and then abort the actual process as well. If the proccess receives a message
% to kill the process prior to executing the greeting procedure, then the process
% will die prematurely.

wait(Nick) :-
  greet_limit(Limit),
  alarm(Limit, greet_and_kill(Nick), _),
  thread_get_message(_).


%% greet_and_kill(+Nick:string)
%
% Greet nick and abort process.
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


%% store_names(Msg) is semidet.
%
% Log current users in channel if 353 message is delivered to client via server.
store_names(Msg) :-
  db_attach('extensions/greets.db', []),
  % 353 is code for users present on the channel
  Msg = msg(_Prefix, "353", _, Text),
  split(Text, 32, Names),
  maplist(add_name, Names).


%% add_name(+Codes) is det.
%
% Add a nick/name as a user in the database if not already present. Always true.
add_name(Codes) :-
  atom_codes(Atom, Codes),
  (  \+current_name(Atom)
  -> assert_current_name(Atom)
  ;  true
  ).


