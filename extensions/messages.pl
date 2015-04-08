
:- module(messages, [messages/1]).

:- use_module(dispatch).
:- use_module(parser).
:- use_module(library(persistency)).

:- persistent
     message(sender:atom, nick:atom, text:string).

chan("yesbot").
chan("##prolog").


%--------------------------------------------------------------------------------%
% Main Interface
%--------------------------------------------------------------------------------%


%% messages(+Msg) is semidet.
%
% This is the main extension interface. messages/0 will listen for JOIN commands
% and prompt that user if his nick is mapped to a recorded message. messages/0
% format :?record message(<nick>, "<msg>")

messages(Msg) :-
  thread_create(messages_access(Msg), _Id, [detached(true)]).


messages_access(Msg) :-
  with_mutex(db,
    (
       db_attach('extensions/messages.db', []),
       ignore(messages_(Msg))
    )
  ).


% See if a joining user has any messages in the database.
messages_(Msg) :-
  chan(Chan),
  Msg = msg(Prefix, "JOIN", [Chan]),
  prefix_id(Prefix, Nick, _, _),
  message(_,N,_),
  maplist(normalize_atom_string, [N], [Nick]),
  findall(_, message(_,N,_), C),
  length(C, Count),
  format(string(Greet), 'Hello ~s, you have ~d pending message(s).', [Nick, Count]),
  send_msg(priv_msg, Greet, Chan),
  send_msg(priv_msg, "You can play a message by typing ?play", Chan), !.

% See if a user who has messages is requesting ?play
messages_(Msg) :-
  chan(Chan),
  Msg = msg(Prefix, "PRIVMSG", [Chan], Rest),
  prefix_id(Prefix, Nick, _, _),
  atom_codes(R, Rest),
  normalize_space(atom('?play'), R),
  (
      message(S,N,T),
      atom_string(T, Text),
      maplist(normalize_atom_string, [N,S], [Nick, Sender])
  *->
      format(string(From), '~s says:', [Sender]),
      (
         Chan = "yesbot"
      ->
         % For handling message interaction for recipients in private
         send_msg(priv_msg, From, Nick),
         send_msg(priv_msg, Text, Nick),
         send_msg(priv_msg, "You can type ?play again to play more messages \c
	   in your queue", Nick)
      ;
	 % For handling message interaction for recipients in the channel
         send_msg(priv_msg, From, Chan),
         send_msg(priv_msg, Text, Chan),
         send_msg(priv_msg, "You can type ?play again to play more messages \c
	   in your queue", Chan)
      ),
      retract_message(S,N,T),
      db_sync(reload), !
  ;
     (  Chan = "yesbot"
     -> send_msg(priv_msg, "You have no messages!", Nick)
     ;  send_msg(priv_msg, "You have no messages!", Chan)
     )
  ).

% See if a user is trying to record a message for another user.
messages_(Msg) :-
  chan(Chan),
  Msg = msg(Prefix, "PRIVMSG", [Chan], Rest),
  prefix_id(Prefix, Sender, _, _),
  atom_string(S, Sender),
  append(`?record `, R0, Rest),
  string_codes(Request, R0),
  term_string(message(N,T), Request),
  assert_message(S,N,T),
  (  Chan = "yesbot"
  -> send_msg(priv_msg, "Done.", Sender)
  ;  send_msg(priv_msg, "Done.", Chan)
  ),
  db_sync(reload),
  db_sync(gc).


%% normalize_atom_string(+Atom, -Normalized) is det.
%
% normalize_atom_string(Atom, Normalized) is true if Normalized is a normalized
% string conversion of Atom.

normalize_atom_string(Atom, Normalized) :-
  atom_string(Atom, String),
  normalize_space(string(Normalized), String).


