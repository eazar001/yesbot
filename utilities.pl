
%% Utilities module for anything that might be generally useful for Yesbot


:- module(utilities,
     [ run_det/1
      ,run_det/2
      ,run_det_sync/3
      ,init_timer/1
      ,is_sync/1
      ,priv_msg/2
      ,priv_msg/3 ]).

:- use_module(config).
:- use_module(info).
:- use_module(library(func)).
:- use_module(library(dcg/basics)).
:- use_module(library(predicate_options)).
:- use_module(library(list_util)).


%--------------------------------------------------------------------------------%
% Concurrency
%--------------------------------------------------------------------------------%


%% run_det(+Msg:compound, :Extension) is det.
%
% Used to find all solutions to an extensionized goal and evaluate it as
% deterministic. This predicate should only be used for extensions that are
% intended to be dispatched asynchronously. Extensions that are desired
% synchronous should be prefixed with 'sync-', and be dispatched in a set of
% blocking extensions via concurrent/3.

run_det(Msg, Extension) :-
  E = ignore((call(Extension:Extension, Msg), fail)),
  thread_create(E, _Id, [detached(true)]).


%% run_set_sync(+Msg:compound, :Extension, :E) is det.
%
% Convert an extensionized goal into a ready form for usage with concurrent/3.
% This predicate is intended to only be used with synchronous extensions.

run_det_sync(Msg, Extension, E) :-
  E = ignore((call(Extension:Extension, Msg), fail)).


%% run_det(:Goal) is det.
%
% Find all the solutions to an extensionized goal in order to precipitate the
% result as an unevaluated deterministic result. Used here for deterministically
% evaluating a possibly nondet or semidet prediciate concurrently.

run_det(Goal) :-
  ignore((Goal, fail)).


%% is_sync(+Name:atom) is semidet.
%
% True if the extension name is prefixed with 'sync_'. (synchronous)
is_sync(Name) :-
  is_sync_(atom_codes $ Name, _Rest).

is_sync_ --> `sync_`.


%--------------------------------------------------------------------------------%
% Connectivity/Timing
%--------------------------------------------------------------------------------%


%% init_timer(-Id:integer) is semidet.
%
% Initialize a message queue that stores one thread which acts as a timer that
% checks connectivity of the bot when established interval has passed.

init_timer(Id) :-
  message_queue_create(Id, [alias(tq)]),
  thread_create(check_pings(Id), _, [alias(ping_checker)]).


%% check_pings(+Id:integer) is failure.
% If Limit seconds has passed, then signal the connection threat to abort. If a
% ping has been detected and the corresponding message is sent before the time
% limit expires, then the goal will succeed and so will the rest of the predicate.
% The thread will then return to its queue, reset its timer, and wait for another
% ping signal.

check_pings(Id) :-
  time_limit(Limit),
  repeat,
    (  thread_get_message(Id, Goal, [timeout(Limit)])
    -> Goal
    ;  thread_signal(ct, throw(abort))
    ),
    fail.


%--------------------------------------------------------------------------------%
% Sending Messages
%--------------------------------------------------------------------------------%


:- predicate_options(priv_msg/3, 3,
     [ auto_nl(boolean)
      ,at_most(nonneg)
      ,encoding(encoding) ]).


%% priv_message(+Text:string, +Recipient:string) is det.
%
% This is a convenience predicate for sending private messages to recipients on
% IRC channels. If there are any newlines they will be converted into individual
% messages (i.e. paragraph style handling).

priv_msg(Text, Recipient) :-
  priv_msg(Text, Recipient, [auto_nl(true)]).


priv_msg(Text, Recipient, Options) :-
  Send_msg = (\Msg^send_msg(priv_msg, Msg, Recipient)),
  option(encoding(Encoding), Options, utf8),
  get_irc_write_stream(Stream),
  set_stream(Stream, encoding(Encoding)),
  priv_msg_auto_nl(Text, Paragraph),
  (
     option(auto_nl(true), Options, true)
  ->
     option(at_most(Limit), Options, length $ Paragraph),  % auto-nl
     take(Paragraph, Limit, P),
     maplist(Send_msg, P)
  ;
     maplist(Send_msg, Paragraph) % no auto-nl
  ),
  (  stream_property(Stream, encoding(utf8))
  -> true
  ;  set_stream(Stream, encoding(utf8))
  ).


priv_msg_auto_nl(Text, Paragraph) :-
  min_msg_len(Min),
  Length is 512 - Min,
  insert_nl_at(Length, string_codes $ Text, Formatted),
  split_string(Formatted, "\n", "", Paragraph).



insert_nl_at(Num, Codes, Formatted) :-
  insert_nl_at(Codes, F, Num, Num),
  string_codes(Formatted, F).

insert_nl_at([], [], _, _).
insert_nl_at([X|Xs], [X|Ys], N, N0) :-
  N0 > 1, !,
  N1 is N0-1,
  insert_nl_at(Xs, Ys, N, N1).

insert_nl_at([X|Xs], [X,10|Ys], N, 1) :-
  insert_nl_at(Xs, Ys, N, N).
  

