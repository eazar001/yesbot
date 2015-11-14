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

:- module(core, [ connect/0 ]).

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
  desired_extensions(Extensions),
  partition(is_sync, Extensions, Sync, Async),
  length(Sync, N0),
  length(Async, N1),
  asserta(sync_extensions(Sync, N0)),
  asserta(extensions(Async, N1)),
  maplist(Import_extension_module, Extensions),
  maplist(qualify, Async, Async, AsyncHandlers),
  assert_handlers(irc, AsyncHandlers).


qualify(X, X, X:X).

%% is_sync(+Name:atom) is semidet.
%
%  True if the extension name is prefixed with 'sync_'. (synchronous)
is_sync(Name) :-
  is_sync_(atom_codes $ Name, _Rest).

is_sync_ --> `sync_`.






%--------------------------------------------------------------------------------%
% Handle Incoming Server Messages
%--------------------------------------------------------------------------------%


%% process_msg(+Msg:compound) is nondet.
%
%  All extensions that deal specifically with handling messages should be
%  implemented dynamically in this section. The extensions will be plugged into
%  an execution list that follows a successful parse of a private message.

/*
process_msg(Msg) :-
  extensions(Async, N),
  (  N > 0
  -> maplist(run_det(Msg), Async)
  ;  true
  ).
*/


