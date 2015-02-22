
%% Run yesbot here

:- initialization(run).

:- use_module(core).
:- use_module(dispatch).
:- use_module(operator).

% TBD: Currently extensions that are independent of message lines relayed by the
% server are possible, but not explicitly supported with a convenient user
% interface. These types of extensions are possible by coding scripts and calling
% them via the interactor or a top level goal, however that is somewhat of a hack.
% So at some point in time an interface that explicitly supports this should be
% built.

run :-
  thread_create(connect, Connect, [alias(ct)]),
  thread_signal(Connect, attach_console).


