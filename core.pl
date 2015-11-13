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

:- assert_handlers(_, []).

%--------------------------------------------------------------------------------%
% Connection Details
%--------------------------------------------------------------------------------%


%% connect is nondet.
%
%  Open socket on host, port, nick, user, hostname, and servername that will all
%  be specified in the bot_config module. The socket stream that is established
%  will be asserted at the top level for access from anywhere in the program.


connect :-
  thread_create(connect_1, _, [detached(true),alias(ct1)]).



connect_1 :-
  connect('chat.freenode.net', 6667, pass, eazarbot, ['#testeazarbot']).





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


