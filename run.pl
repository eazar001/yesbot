
%% Run yesbot here

:- initialization(run).

:- use_module(core).
:- use_module(dispatch).


% TBD: Run a separate interactive console parallel to the debugging messages.

run :-
  connect.
  %thread_create(connect, Connect, [detached(true)]),
  %thread_signal(Connect, attach_console),
  %thread_detach(Connect).