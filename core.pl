%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                %
%                                                                                %
% Author: Ebrahim Azarisooreh                                                    %
% E-mail: ebrahim.azarisooreh@gmail.com                                          %
% IRC Nick: eazar001                                                             %
% Title: Yes-Bot                                                                 %
% Description: IRC Bot                                                           %
%                                                                                %
%                                                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(core,
     [ connect/0
      ,goals_to_concurrent/2 ]).

:- use_module(library(irc_client)).
:- use_module(library(socket)).
:- use_module(library(func)).
:- use_module(library(lambda)).
:- use_module(config).

:- initialization assert_handlers.


connect :-
  thread_create(join_prolog, _, [detached(true), alias(irc)]).


join_prolog :-
  host(Host),
  port(Port),
  pass(Pass),
  nick(Nick),
  chans(Chans),
  bot_hostname(Hn),
  bot_servername(Sn),
  bot_realname(Rn),
  Names = [Hn,Sn,Rn],
  connect(Host, Port, Pass, Nick, Names, Chans).


assert_handlers :-
  init_extensions.
  

init_extensions :-
  Import_extension_module = (\Extension^use_module(extensions/Extension)),
  Qualify = (\X^X^Q^(Q = X:X)),
  desired_extensions(Extensions),
  partition(is_sync, Extensions, Sync, Async),
  length(Sync, N0),
  length(Async, N1),
  asserta(sync_extensions(Sync, N0)),
  asserta(extensions(Async, N1)),
  maplist(Import_extension_module, Extensions),
  maplist(Qualify, Sync, Sync, SyncHandlers),
  maplist(Qualify, Async, Async, AsyncHandlers),
  append(AsyncHandlers, [goals_to_concurrent(SyncHandlers)], Handlers),
  assert_handlers(irc, Handlers).



%% is_sync(+Name:atom) is semidet.
%
%  True if the extension name is prefixed with 'sync_'. (synchronous)
is_sync(Name) :-
  is_sync_(atom_codes $ Name, _Rest).

is_sync_ --> `sync_`.


goals_to_concurrent(Goals, Msg) :-
  sync_extensions(_, N),
  (  N > 0
  -> goals_to_calls(Goals, Calls),
     maplist(call_with_msg(Msg), Calls, RunCalls),
     concurrent(N, RunCalls, [])
  ;  true
  ).


call_with_msg(Msg, Call, call(Call, Msg)).

goals_to_calls(Goals, Calls) :-
  maplist(goal_to_call, Goals, Calls).

goal_to_call(Goal, Call) :-
  Call = (\Msg^call(Goal,Msg)).


