
%% Run yesbot here

:- initialization(run).

:- use_module(core).
:- use_module(dispatch).
:- use_module(operator).


% TBD: Look into interactor solutions that support readline; commmand-line editing
% would be nice here.

run :-
  interactor,
  connect.


