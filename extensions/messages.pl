
:- module(messages, [messages/1]).

:- use_module(dispatch).
:- use_module(parser).
:- use_module(library(lambda)).
:- use_module(library(dcg/basics)).
:- use_module(library(persistency)).
:- use_module(submodules/utils).

:- persistent
     message(sender:atom, nick:atom, text:string).

:- dynamic session/1.
:- dynamic session/2.

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
      maplist(\Atom^Normalized^(atom_string(Atom, String),
        normalize_space(string(Normalized), String)), [N,S], [Nick, Sender])
  *->
      format(string(From), "~s says:", [Sender]),
      priv_msg(From, Recipient),
      priv_msg(Text, Recipient),
      priv_msg("You can type ?play again to play more messages \c
        in your queue", Recipient),
      retract_message(S,N,T), !
  ;
      priv_msg("You have no messages!", Recipient)
  ).

% See if a user is trying to record a message for another user.
% This is for recording quick messages with the format:
% ?record message('recipient', "message")
messages_(Msg) :-
  Msg = msg(Prefix, "PRIVMSG", _, Rest),
  prefix_id(Prefix, Sender, _, _),
  determine_recipient(messages, Msg, Recipient),
  atom_string(S, Sender),
  \+session(S), !, % No simple recordings accepted in the middle of session
  append(`?record `, R0, Rest),
  string_codes(Request, R0),
  term_string(message(N0,T), Request),
  normalize_space(atom(N), N0),
  N \= yesbot,  % No messages will be accepted for yesbot
  assert_message(S,N,T),
  priv_msg("Done.", Recipient),
  db_sync(gc).

% See if a user is trying to record a message for another user.
% This is for recording continuous messages:
% ex:
% >first line
% >second line
% done (say anything without the input char to stop recording)
messages_(Msg) :-
  Msg = msg(Prefix, "PRIVMSG", _, Request),
  prefix_id(Prefix, Sender, _, _),
  atom_string(S, Sender),
  determine_recipient(messages, Msg, Recipient),
  nonblanks(`?record`, Request, Rest),
  phrase(blanks, Rest),
  (  new_session(S), !  % open new session if new sender
  ;
     % Open session
     Request = [62|Line], % first char is '>' (recoding a line)
     with_output_to(codes(Codes, Diff), write(Line)),
     (  session(S, In-Rest)
     ->	Rest = Codes,
	retractall(session(S, _)),
	asserta(session(S, In-Diff))
     ;
	asserta(session(S, Codes-Diff))
     ), !
  ;
     % Closing an open session and completing recording
     session(S, In-_),
     retractall(session(S, _)),
     asserta(session(S, In-[])),
     priv_msg("Who is this message for? \c
       (enclose the nick in single quotes)", Recipient)
  ).
     


%% new_session(+Sender:atom) is semidet.
%
% Open a new session with the sender if one doesn't already exist.
new_session(Sender) :-
  \+session(Sender),
  asserta(session(Sender)).


