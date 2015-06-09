
:- module(messages, [messages/1]).

:- use_module(dispatch).
:- use_module(parser).
:- use_module(library(persistency)).
:- use_module(submodules/utils).

:- persistent
     message(sender:atom, nick:atom, text:string).


target("##prolog", "yesbot").


%--------------------------------------------------------------------------------%
% Main Interface
%--------------------------------------------------------------------------------%


%% messages(+Msg) is semidet.
%
% This is the main extension interface. messages/0 will listen for JOIN commands
% and prompt that user if his nick is mapped to a recorded message. messages/0
% format :?record message(<nick>, "<msg>")

messages(Msg) :-
  messages_access(Msg).


messages_access(Msg) :-
  with_mutex(messages_db,
    (  db_attach('extensions/messages.db', []),
       ignore(messages_(Msg))
    )
  ).


% See if a joining user has any messages in the database.
messages_(Msg) :-
  target(Chan, _),
  Msg = msg(Prefix, "JOIN", [Chan]),
  prefix_id(Prefix, Nick, _, _),
  atom_string(N, Nick),
  aggregate_all(count, message(_, N, _), Count),
  Count > 0,
  format(string(Greet),
    "Hello ~s, you have ~d pending message(s).", [Nick, Count]),
  send_msg(priv_msg, Greet, Chan),
  send_msg(priv_msg,
    "You can play a message by typing ?play \c
    (you can also do this in private if you want)", Chan), !.

% See if a user who has messages is requesting ?play
messages_(Msg) :-
  Msg = msg(Prefix, "PRIVMSG", _Target, Rest),
  prefix_id(Prefix, Nick, _, _),
  determine_recipient(messages, Msg, Recipient),
  atom_codes(R, Rest),
  normalize_space(atom('?play'), R),
  (
      message(S,N,T),
      atom_string(T, Text),
      maplist(normalize_atom_string, [N,S], [Nick, Sender])
  *->
      format(string(From), "~s says:", [Sender]),
      send_msg(priv_msg, From, Recipient),
      send_msg(priv_msg, Text, Recipient),
      send_msg(priv_msg, "You can type ?play again to play more messages \c
        in your queue", Recipient),
      retract_message(S,N,T), !
  ;
      send_msg(priv_msg, "You have no messages!", Recipient)
  ).

% See if a user is trying to record a message for another user.
messages_(Msg) :-
  Msg = msg(Prefix, "PRIVMSG", _, Rest),
  prefix_id(Prefix, Sender, _, _),
  determine_recipient(messages, Msg, Recipient),
  atom_string(S, Sender),
  append(`?record `, R0, Rest),
  string_codes(Request, R0),
  term_string(message(N0,T), Request),
  normalize_space(atom(N), N0),
  assert_message(S,N,T),
  send_msg(priv_msg, "Done.", Recipient),
  db_sync(gc).


%% normalize_atom_string(+Atom:atom, -Normalized:string) is det.

normalize_atom_string(Atom, Normalized) :-
  atom_string(Atom, String),
  normalize_space(string(Normalized), String).


