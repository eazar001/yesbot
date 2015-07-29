
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
:- use_module(info).

%--------------------------------------------------------------------------------%
% Command Routing
%--------------------------------------------------------------------------------%


% FIXME: Not all message types from operator are implemented yet.

%% return_server(-Server:string) is det.
%
% If the server is known get the value from the core. If not, then the server is
% 'unknown'.

return_server(Server) :-
  (  known(irc_server)
  -> get_irc_server(Server)
  ;  Server = unknown
  ).


%% cmd_params(+Type, -N) is semidet.
%
% True if N is the number of paramteres in Type's template.
cmd_params(Type, N) :-
  cmd(Type, Template),
  split_string(Template, "~", "\r~n", [_|Params]),
  length(Params, N).


:- discontiguous dispatch:send_msg/3.

%% send_msg(+Type:atom) is semidet.
%
% Send a message of Type.
send_msg(Type) :-
  cmd(Type, Msg),
  get_irc_stream(Stream),
  cmd_params(Type, 0),
  write(Stream, Msg),
  flush_output(Stream),
  thread_send_message(tq, true).

% This clause will deal with deal with message types that are possibly
% timer-independent
send_msg(Type) :-
  cmd(Type, Msg),
  get_irc_stream(Stream),
  connection(Nick, Pass, Chans, HostName, ServerName, RealName),
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
  ), !,
  flush_output(Stream),
  (  known(tq)
  -> thread_send_message(tq, true)
  ;  true
  ).


%% send_msg(+Type:atom, +Param:string) is semidet.
%
% Send message of Type with attention to some parameter Param.
send_msg(Type, Param) :-
  cmd(Type, Msg),
  cmd_params(Type, 1),
  (
     Type = pong
  ->
     dbg(pong, Debug),
     format(Debug, [Param])
  ;
     true
  ), !,
  get_irc_stream(Stream),
  format(Stream, Msg, [Param]),
  flush_output(Stream),
  thread_send_message(tq, true).


%% send_msg(+Type:atom, +Str:text, +Target:string) is semidet.
%
% Send a Str of Type to a specified Target.
send_msg(Type, Str, Target) :-
  cmd(Type, Msg),
  cmd_params(Type, 2),
  \+member(Type, [kick, invite]), !,
  get_irc_stream(Stream),
  format(Stream, Msg, [Target, Str]),
  flush_output(Stream),
  thread_send_message(tq, true).

%% send_msg(+Type:atom, +Chan:text, +Target:string) is semidet.
%
% Send a message of Type to Target in Chan.
send_msg(Type, Chan, Target) :-
  cmd(Type, Msg),
  get_irc_stream(Stream),
  (
     Type = kick,
     format(Stream, Msg, [Chan, Target])
  ;
     Type = invite,
     format(Stream, Msg, [Target, Chan])
  ), !,
  flush_output(Stream),
  thread_send_message(tq, true).


