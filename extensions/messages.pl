
:- module(messages, [messages/1]).

:- use_module(dispatch).
:- use_module(parser).
:- use_module(library(csv)).

:- dynamic recording/3.


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
  prefix_id(Prefix, Nick, _, _),
  read_db,
  recording(N, S, T),
  atom_string(N, Nick),
  atom_string(S, Sender),
  atom_string(T, Text),
  format(string(Greet), 'Hello ~s, ~s has left you a message.', [Nick, Sender]),
  retract(recording(N,S,T)),
  send_msg(priv_msg, Greet, Chan),
  send_msg(priv_msg, Text, Chan).

% See if a user is trying to record a message for another user.
messages_(Msg) :-
  chan(Chan),
  Msg = msg(_Prefix, "PRIVMSG", [Chan], Rest),
  append(`?tell `, R0, Rest),
  string_codes(R, R0),
  normalize_space(string(Request), R),
  update_db(Request).

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
  open_db_with(_Fstream, true).


%% populate_db is semidet.
%
% Read in messages.db and populate db with facts of the following structure:
% recording(Sender, Recipient, Message) [recording/3].

populate_db :-
  csv_read_file('messages.db', Rows,
    [functor(recording), arity(3), separator(44)]),
  maplist(asserta, Rows).


update_db(Row) :-
  open_db_with(Fstream, format(Fstream, '~s~n', [Row])).


%% open_db_with(-Fstream, :Goal) is semidet.
%
% Attempts to open up messages.db file with a goal and then close Fstream.
% Unification of arguments isn't extant here.

open_db_with(Fstream, Goal) :-
  setup_call_cleanup(
    open('messages.db', append, Fstream),
    Goal,
    close(Fstream)
  ).


