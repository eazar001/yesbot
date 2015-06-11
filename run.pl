
%% Run yesbot here

:- initialization(run).

:- use_module(core).
:- use_module(config).
:- use_module(utilities).
:- use_module(dispatch).
:- use_module(operator).
:- use_module(library(lambda)).
:- use_module(library(func)).
:- use_module(library(mavis)).


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
  desired_extensions(Es),
  catch(check_valid_extensions(Es), _Error, safety),
  thread_create(connect, Connect, [alias(ct)]),
  thread_signal(Connect, attach_console).

safety :-
  writeln('WARNING: The set of extensions you have provided \c
    has been determined to be invalid. Your extension list \c
    has been emptied and will be running nothing if you do \c
    not take action. Please update this setting to a valid \c
    one.'),
  retractall(info:extensions(_,_)),
  retractall(info:sync_extensions(_,_)),
  set_setting(config:extensions, []),
  save_settings.


