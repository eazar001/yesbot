
%% Run yesbot here

:- initialization(run).

:- use_module(core).
:- use_module(dispatch).
:- use_module(operator).

% TBD: Currently extensions that are independent of message lines relayed by the
% server are possible, but must be done by building them as regular extensions
% that spawn detached threads independent of server messages. It is up to the user
% how to further handle the life cycle of the extension. For example, one can
% remove it from the extension list after execution and keep it persistently
% running in the background or even run it as a one-shot extension. Nonetheless
% this is a bit of a hack and could benefit from a convenient interface or some
% more intuitive abstractions. Some extra documentation would also help.

%% run is det.

run :-
  thread_create(connect, Connect, [alias(ct)]),
  thread_signal(Connect, attach_console).


