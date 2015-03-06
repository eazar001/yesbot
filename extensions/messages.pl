
:- module(messages, [messages/1]).

:- use_module(dispatch).
:- use_module(parser).
:- use_module(library(csv)).

:- dynamic recording/3.
:- dynamic loaded/1.

chan("#testeazarbot").


%--------------------------------------------------------------------------------%
% Main Interface
%--------------------------------------------------------------------------------%


%% messages(+Msg) is semidet.
%
% This is the main extension interface. messages/0 will listen for JOIN commands
% and prompt that user if his nick is mapped to a recorded message. messages/0
% will also listen for any private messages that are logged a ?tell <nick> <msg>.
% After a ?tell command, yesbot will attempt to log the sender, nick, and
% corresponding message so that the next time the the person with the appropriate
% nick logs on, yesbot will prompt that user to play his/her messages.

messages(Msg) :-
  setup_call_cleanup(
    working_directory(_, extensions),
    ignore((messages_(Msg), fail)),
    working_directory(_, '../')
  ).
		    
% See if a joining user has any messages in the database.
messages_(Msg) :-
  chan(Chan),
  Msg = msg(Prefix, "JOIN", [Chan]),
  prefix_id(Prefix, Nick, _, _),
  load_kb,
  recording(N,S,T),
  atom_string(T, Text),
  maplist(normalize_atom_string, [N,S], [Nick, Sender]),
  format(string(Greet), 'Hello ~s, ~s has left you a message.',
    [Nick, Sender, Text]),
  retract(recording(N,S,T)),
  cleanup_routine,
  retractall(loaded(_)),
  send_msg(priv_msg, Greet, Chan),
  send_msg(priv_msg, Text, Chan).

% See if a user is trying to record a message for another user.
messages_(Msg) :-
  chan(Chan),
  Msg = msg(_Prefix, "PRIVMSG", [Chan], Rest),
  append(`?tell `, R0, Rest),
  string_codes(Request, R0),
  update_db(Request).


%--------------------------------------------------------------------------------%
% DB Operations
%--------------------------------------------------------------------------------%


% load_kb is det.
%
% If database is already loaded do nothing. Otherwise, read from the messages.db
% and load it into the KB.

load_kb :-
  (
     loaded(true)
  ->
     true
  ;
     read_db,
     asserta(loaded(true))
  ).


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
  open_db_with(_Fstream, write, true).


%% populate_db is semidet.
%
% Read in messages.db and populate db with facts of the following structure:
% recording(Sender, Recipient, Message) [recording/3].

populate_db :-
  csv_read_file('messages.db', Rows,
    [functor(recording), arity(3), separator(44)]),
  maplist(asserta, Rows).


%% update_db(+Row) is semidet.
%
% Add a new record to message.db.

update_db(Row) :-
  open_db_with(Fstream, append, format(Fstream, '~s~n', [Row])).


%% open_db_with(-Fstream, :Goal) is semidet.
%
% Attempts to open up messages.db file with a goal and then close Fstream.
% Unification of arguments isn't extant here.

open_db_with(Fstream, Mode, Goal) :-
  setup_call_cleanup(
    open('messages.db', Mode, Fstream),
    Goal,
    close(Fstream)
  ).


%--------------------------------------------------------------------------------%
% Utilities
%--------------------------------------------------------------------------------%


%% normalize_atom_string(+Atom, -Normalized) is det.
%
% normalize_atom_string(Atom, Normalized) is true if Normalized is a normalized
% string conversion of Atom.

normalize_atom_string(Atom, Normalized) :-
  atom_string(Atom, String),
  normalize_space(string(Normalized), String).


%% cleanup_routine is det.
%
% The purpose of cleanup_routine/0 is to find all the knowledge of recordings in
% the knowledge base and write them to a new file, delete the original
% messages.db file, and rename the new file as messages.db. This is essentially
% and updating and persistence routine all in one.

cleanup_routine :-
  findall(R, (recording(S,N,T), R = recording(S,N,T)), Rs),
  csv_write_file('temp.db', Rs, [separator(44)]),
  mv('temp.db', 'messages.db').


