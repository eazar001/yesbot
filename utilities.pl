
%% Utilities module for anything that might be generally useful for Yesbot


:- module(utilities,
     [ run_det/1
      ,run_det/3 ]).


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

