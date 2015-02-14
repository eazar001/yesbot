
%% Run yesbot here

:- initialization(run).

:- use_module(core).

run :-
  thread_create(core:connect, _Id, [detached(true)]).