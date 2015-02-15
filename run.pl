
%% Run yesbot here

:- initialization(run).

:- use_module(core).
:- use_module(dispatch).
:- use_module(operator).


run :-
  thread_create(connect, Connect, []),
  thread_signal(Connect, attach_console),
  thread_detach(Connect).


