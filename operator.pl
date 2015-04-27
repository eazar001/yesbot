
%% Operator module that determines appropriate messaging templates for specific
%% atoms which connote particular irc operations.


:- module(operator,
     [ cmd/2
      ,dbg/2 ]).


%--------------------------------------------------------------------------------%
% Commands
%--------------------------------------------------------------------------------%


% Convenience commands for specific message relays.

% FIXME: Not all message types are implemented yet.

%% cmd(+Type:atom, -Template:atom) is semidet.
%
% Attempt to unify with a type and a corresponding string template. The string
% template will be fed to dispatch in order to send a message.

% NOTICE [Target, Notice]
cmd(notice, 'NOTICE ~s :~s\r~n').

% PRIVMSG [Target, PrivMsg]
cmd(priv_msg, 'PRIVMSG ~s :~s\r~n').

% TIME [Server]
cmd(time, 'TIME ~s\r~n').

% KICK [Chan, Target]
cmd(kick, 'KICK ~s ~s\r~n').

% INVITE [Target, Chan]
cmd(invite, 'INVITE ~s ~s\r~n').

% NAMES [Chan]
cmd(names, 'NAMES ~s\r~n').

% LIST [CSV] (a csv of channels)
cmd(list, 'LIST ~s\r~n').

% TOPIC [Chan, Topic]
cmd(topic, 'TOPIC ~s ~s\r~n').

% MODE [+/-, Mode, NICK]
cmd(user_mode, 'MODE ~s~s ~s\r~n').

% PART [CSV] (a csv of channels)
cmd(part, 'PART ~s\r~n').

% PING [Target]
cmd(ping, 'PING ~s\r~n').

% PONG [Target]
cmd(pong, 'PONG ~s\r~n').

% PASS [Pass]
cmd(pass, 'PASS ~s\r~n').

% USER [Nick, HostName, ServerName, RealName]
cmd(user, 'USER ~s ~s ~s :~s\r~n').

% NICK [Nick]
cmd(nick, 'NICK ~s\r~n').

% JOIN [Chan]
cmd(join, 'JOIN ~s\r~n').

% QUIT
cmd(quit, 'QUIT :disconnect\r\n').


%--------------------------------------------------------------------------------%
% Debugging
%--------------------------------------------------------------------------------%

%% dbg(+Type:atom, -Template:atom) is semidet.
%
% This is used to debug pings, to ensure the client that pongs are being sent.

% PONG [Origin]
dbg(pong, 'PONG ~s~n').


