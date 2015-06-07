
:- module(info, [info_cleanup/0]).

:- dynamic known/1.
:- dynamic get_irc_server/1.
:- dynamic get_irc_stream/1.
:- dynamic connection/6.


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
      known(_) ]).


