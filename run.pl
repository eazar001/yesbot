
%% Run yesbot here

:- initialization(run).

:- use_module(core).

run :-
  thread_create(core:connect, Run, []),
  thread_join(Run, _Status).