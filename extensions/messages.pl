
:- module(messages, [messages/1]).

:- use_module(dispatch).
:- use_module(parser).
:- use_module(library(persistency)).

:- persistent
     recording(sender:atom, nick:atom, text:atom).

chan("##prolog").


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
  db_attach('messages.db', []),
  prefix_id(Prefix, Nick, _, _),
  recording(N,S,T),
  atom_string(T, Text),
  maplist(normalize_atom_string, [N,S], [Nick, Sender]),
  format(string(Greet), 'Hello ~s, ~s has left you a message.',
    [Nick, Sender, Text]),
  retract_recording(N,S,T),
  send_msg(priv_msg, Greet, Chan),
  send_msg(priv_msg, Text, Chan).

% See if a user is trying to record a message for another user.
messages_(Msg) :-
  chan(Chan),
  Msg = msg(_Prefix, "PRIVMSG", [Chan], Rest),
  append(`?tell `, R0, Rest),
  string_codes(Request, R0),
  term_string(recording(S,N,T), Request),
  assert_recording(S,N,T).


%% normalize_atom_string(+Atom, -Normalized) is det.
%
% normalize_atom_string(Atom, Normalized) is true if Normalized is a normalized
% string conversion of Atom.

normalize_atom_string(Atom, Normalized) :-
  atom_string(Atom, String),
  normalize_space(string(Normalized), String).


