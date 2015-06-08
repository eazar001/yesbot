
%% The info module handle bookeeping for dynamic top level information access

:- module(info,
     [ info_cleanup/0
      ,init_extensions/0
      ,known/1
      ,get_irc_server/1
      ,get_irc_stream/1
      ,connection/6
      ,extensions/2
      ,sync_extensions/2 ]).

:- use_module(library(lambda)).

:- dynamic known/1.
:- dynamic get_irc_server/1.
:- dynamic get_irc_stream/1.
:- dynamic connection/6.
:- dynamic extensions/2.
:- dynamic sync_extensions/2.


%--------------------------------------------------------------------------------%
% Extension Loading
%--------------------------------------------------------------------------------%


%% init_extensions is semidet.
%
% Assert the extensions along with its length at the top level for access.
% Import all the modules afterwards. By default extensions will be considered
% asynchronous unless demarcated as the contrary.

init_extensions :-
  Import_extension_module = (\Extension^use_module(extensions/Extension)),
  desired_extensions(Extensions),
  partition(is_sync, Extensions, Sync, Async),
  length(Sync, N0),
  length(Async, N1),
  asserta(sync_extensions(Sync, N0)),
  asserta(extensions(Async, N1)),
  maplist(Import_extension_module, Extensions).


%--------------------------------------------------------------------------------%
% Cleanup
%--------------------------------------------------------------------------------%


%% info_cleanup is det.
%
% Retract all obsolete facts from info module.
info_cleanup :-
  maplist(retractall,
    [ get_irc_stream(_)
     ,connection(_,_,_,_,_,_)
     ,extensions(_,_)
     ,sync_extensions(_,_)
     ,get_irc_server(_)
     ,known(_) ]).


