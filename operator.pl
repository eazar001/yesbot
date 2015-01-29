
%% Operator module that determines appropriate messaging templates for specific
%% atoms which connote particular irc operations.


:- module(operator,
     [ cmd/2
      ,dbg/2 ]).


%--------------------------------------------------------------------------------%
% Commands
%--------------------------------------------------------------------------------%


% Convenience commands for specific message relays.
% XXX NOTE : Not all message types are implemented yet.

% NOTICE [Target, Notice]
cmd(notice, 'NOTICE ~s :~s\r~n').

% PRIVMSG [Target, PrivMsg]
cmd(priv_msg, 'PRIVMSG ~s :~s\r~n').

% TIME [Server]
cmd(time, 'TIME ~a\r~n').

% KICK [Chan, Target]
cmd(kick, 'KICK ~a ~a\r~n').

% INVITE [Target, Chan]
cmd(invite, 'INVITE ~a ~a\r~n').

% NAMES [CSV] (a csv of channels)
cmd(names, 'NAMES ~a\r~n').

% LIST [CSV] (a csv of channels)
cmd(list, 'LIST ~a\r~n').

% TOPIC [Chan, Topic]
cmd(topic, 'TOPIC ~a ~a\r~n').

% MODE [+/-, Mode, NICK]
cmd(user_mode, 'MODE ~a~a ~a\r~n').

% PART [CSV] (a csv of channels)
cmd(part, 'PART ~a\r~n').

% PING [Target]
cmd(ping, 'PING ~a\r~n').

% PONG [Target]
cmd(pong, 'PONG ~s\r~n').

% PASS [Pass]
cmd(pass, 'PASS ~a\r~n').

% USER [Nick, HostName, ServerName, RealName]
cmd(user, 'USER ~a ~a ~a :~a\r~n').

% NICK [Nick]
cmd(nick, 'NICK ~a\r~n').

% JOIN [Chan]
cmd(join, 'JOIN ~a\r~n').

% QUIT
cmd(quit, 'QUIT :disconnect\r\n').


%--------------------------------------------------------------------------------%
% Debugging
%--------------------------------------------------------------------------------%


% PONG [Origin]
dbg(pong, 'PONG ~s~n').

