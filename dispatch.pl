
%% Message dispatching module
%
% This is a switchboard for routing message types to the correct message
% templates. Once the message template and respective substitution list is
% unified with the type, the process is consummated by dispatching the
% message through the stream.

:- module(dispatch,
	  [ send_msg/1,
	    send_msg/2,
	    send_msg/3 ]).

:- use_module(operator).


%--------------------------------------------------------------------------------%
% Command Routing
%--------------------------------------------------------------------------------%


% NOTE : Not all message types are accounted for yet
% NOTE : Not all message types below are implemented in the dispatch module
% TODO : Consider giving these predicates more meaningful names.


%% send_msg(+Type, +Target) is nondet.
%
% Send message of Type to a specified Target.

send_msg(Type, Target) :-
  cmd(Type, Msg),
  (
     Type = ping
  ;
     Type = pong,
     dbg(pong, Debug),
     format(Debug, [Target])
  ),
  core:get_irc_stream(Stream),
  format(Stream, Msg, [Target]),
  flush_output(Stream).


%% send_msg(+Type, +Str, +Target) is nondet.
%
% send a Str of Type to a specified Target.

send_msg(Type, Str, Target) :-
  cmd(Type, Msg),
  (
     Type = priv_msg
  ;
     Type = notice
  ),
  core:get_irc_stream(Stream),
  format(Stream, Msg, [Target, Str]),
  flush_output(Stream).


%% send_msg(+Type, +Chan, +Target) is nondet.
%
% Send a message of Type to Target in Chan.

send_msg(Type, Chan, Target) :-
  cmd(Type, Msg),
  core:get_irc_stream(Stream),
  (
     Type = kick,
     format(Stream, Msg, [Chan, Target])
  ;
     Type = invite,
     format(Stream, Msg, [Target, Chan])
  ).


%% send_msg(+Type) is nondet.
%
% Send a message of Type.

send_msg(Type) :-
  cmd(Type, Msg),
  core:get_irc_stream(Stream),
  core:get_irc_server(Server),
  core:connection(Nick, Pass, Chans, HostName, ServerName, RealName),
  (
     Type = pass,
     format(Stream, Msg, [Pass])
  ;
     Type = user,
     format(Stream, Msg, [Nick, HostName, ServerName, RealName])
  ;
     Type = nick,
     format(Stream, Msg, [Nick])
  ;
     Type = join,
     maplist(format(Stream, Msg), Chans)
  ;
     Type = quit,
     write(Stream, Msg)
  ;
     Type = time,
     format(Stream, Msg, [Server])
  ),
  flush_output(Stream).


