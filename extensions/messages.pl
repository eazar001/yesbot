
:- module(messages, [messages/1]).

:- use_module(dispatch).
:- use_module(parser).
:- use_module(library(csv)).


chan("#testeazarbot").


%% messages is nondet.
%
% This is the main extension interface. messages/0 will listen for JOIN commands
% and prompt that user if his nick is mapped to a recorded message. messages/0
% will also listen for any private messages that are logged a ?tell <nick> <msg>.
% After a ?tell command, yesbot will attempt to log the sender, nick, and
% corresponding message so that the next time the the person with the appropriate
% nick logs on, yesbot will prompt that user to play his/her messages.

messages(Msg) :-
  thread_create(ignore(messages_(Msg)), _Id, [detached(true)]).

% See if a joining user has any messages in the database.
messages_(Msg) :-
  chan(Chan),
  Msg = msg(Prefix, "JOIN", [Chan]),
  messages_(Msg).
  prefix_id(Prefix, Nick, _, _),
  recording(Nick, Sender, Text).

% See if a user is trying to record a message for another user.
messages_(Msg) :-
  chan(Chan),
  Msg = msg(_Prefix, "PRIVMSG", [Chan]),
  

%% read_db is semidet.
%
% Read from a csv file that is delimited by white spaces (32). The file will be
% created if one doesn't exist. The format for each row in the database is as
% follows: <Sender> <Recipient> <Message>

read_db :-
  (
     exists_file('messages.db')
  ->
     populate_db
  ;
     create_db,
     populate_db
  ).


%% create_db is semidet.
%
% Attempt to create messages.db

create_db :-
  open_db_with(true).


%% populate_db is semidet.
%
% Read in messages.db and populate db with facts of the following structure:
% recording(Sender, Recipient, Message) [recording/3].

populate_db :-
  csv_read_file('messages.db', Rows,
    [functor(recording), arity(3), separator(32)]),
  maplist(asserta, Rows).


%% open_db_with(:Goal) is semidet.
%
% Attempts to open up messages.db file with a goal and then close Fstream.
% Unification of arguments isn't extant here.

open_db_with(Goal) :-
  setup_call_cleanup(
    open('messages.db', append, Fstream),
    Goal,
    close(Fstream)
  ).


