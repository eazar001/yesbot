
%% Utilities module for anything that might be generally useful for Yesbot


:- module(utilities,
     [ run_det/1
      ,run_det/3
      ,init_timer/1 ]).

:- use_module(config).

%--------------------------------------------------------------------------------%
% Concurrency
%--------------------------------------------------------------------------------%


%% run_det(+Msg, +Extension, -E) is det.
%
% Concurrently call a list of extension predicates on the current message.
% The extension predicates can possibly be nondet, but will still execute
% concurrently without undue interruption.

run_det(Msg, Extension, E) :-
  E = ignore(((call(Extension:Extension, Msg), fail))).


%% run_det(+Goal) is det.
%
% Find all the solutions to an extensionized goal in order to precipitate the
% result as an unevaluated deterministic result. Used here for deterministically
% evaluating a possibly nondet or semidet prediciate concurrently.

run_det(Goal) :-
  ignore((Goal, fail)).


%--------------------------------------------------------------------------------%
% Connectivity/Timing
%--------------------------------------------------------------------------------%


%% init_timer(-Id) is semidet.
%
% Initialize a message queue that stores one thread which acts as a timer that
% checks connectivity of the bot when established interval has passed.

init_timer(Id) :-
  message_queue_create(Id, [alias(tq)]),
  thread_create(check_pings(Id), _, [alias(ping_checker)]).


%% check_pings(+Id) is nondet.
% If Limit seconds has passed, then signal the connection threat to abort. If a
% ping has been detected and the corresponding message is sent before the time
% limit expires, then the goal will succeed and so will the rest of the predicate.
% The thread will then return to its queue, reset its timer, and wait for another
% ping signal.

check_pings(Id) :-
  time_limit(Limit),
  repeat,
    (
       thread_get_message(Id, Goal, [timeout(Limit)])
    ->
       Goal
    ;
       thread_signal(ct, throw(abort))
    ),
    fail.


