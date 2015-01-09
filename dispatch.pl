
%% Message dispatching module


:- module(dispatch, [send_msg/1, send_msg/3]).

:- use_module(operator).


%--------------------------------------------------------------------------------%
% command routing
%--------------------------------------------------------------------------------%


% XXX TODO : implement response/error retrieval
% XXX NOTE : not all message types are accounted for yet

%% send_msg(+Pong, +Nick, +Origin) is det.
%
% Send pong private message back to a specified origin.

send_msg(pong, Nick, Origin) :-
  cmd(pong, Msg),
  dbg(pong, Debug),
  core:get_irc_stream(Stream),
  format(Stream, Msg, [Nick, Origin]),
  format(Debug, [Nick, Origin]),
  flush_output(Stream).

%% send_msg(+Type, +Str, +Target) is det.
%
% send a private message or notice in the form of a string to a specified target.

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

%% send_msg(+Type) is semidet.
%
% This is a switchboard for routing message types to the correct message
% templates. Once the message template and respective substitution list is
% unified with the type, the process is consummated by dispatching the
% message through the stream.

send_msg(Type) :-
  cmd(Type, Msg),
  core:get_irc_stream(Stream),
  core:connection(Nick, Pass, Chan, HostName, ServerName, RealName),
  (
     Type = pass,
     format(Stream, Msg, [Pass, Chan])
  ;
     Type = user,
     format(Stream, Msg, [Nick, HostName, ServerName, RealName])
  ;
     Type = nick,
     format(Stream, Msg, [Nick])
  ;
     Type = join,
     format(Stream, Msg, [Chan])
  ;
     Type = quit,
     write(Stream, Msg)
  ),
  flush_output(Stream).


