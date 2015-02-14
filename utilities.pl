
%% Utilities module for anything that might be generally useful for Yesbot


:- module(utilities,
     [ run_det/1
      ,run_det/3 ]).


%--------------------------------------------------------------------------------%
% Concurrency
%--------------------------------------------------------------------------------%


%% run_det(+Msg, +Extension, -E) is det.
%
% Concurrently call a list of extension predicates on the current message.
% The extension predicates can possibly be nondet, but will still execute
% concurrently without undue interruption.

run_det(Msg, Extension, E) :-
  E = findall(_, call(Extension:Extension, Msg), _).


%% run_det(+Goal) is det.
%
% Find all the solutions to an extensionized goal in order to precipitate the
% result as an unevaluated deterministic result. Used here for deterministically
% evaluating a possibly nondet or semidet prediciate concurrently.

run_det(Goal) :-
  findall(_, Goal, _).


%--------------------------------------------------------------------------------%
% Connectivity/Timing
%--------------------------------------------------------------------------------%


check_connection(T0, T1, Limit, Pinged, Status) :-
  repeat,
    check_delta(T0, T1, Limit, Pinged, Status),
    sleep(0.002).


%% check_delta(+T0, +T1, +Limit, -Pinged, -Status) is det.
%
% Take inputs containing two time points with which the interval shall be
% calculated. If the interval length exceeds the value of Limit and the server
% has not pinged the client at this point in time, the Status will be changed
% to "abort"; status is "continue" otherwise.

check_delta(T0, T1, Limit, Pinged, Status) :-
  (
     Delta is T1 - T0,
     Delta > Limit,
     Pinged = false
  ->
     Status = abort
  ;
     Status = continue
  ).




