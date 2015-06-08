
:- module(config,
     [ host/1
      ,port/1
      ,nick/1
      ,pass/1
      ,chans/1
      ,bot_hostname/1
      ,bot_servername/1
      ,bot_realname/1
      ,desired_extensions/1
      ,set_extensions/1
      ,time_limit/1
      ,add_new_extensions/1
      ,load_new_extensions/1
      ,is_script/1
      ,valid_extensions/1
      ,check_valid_extensions/1 ]).

:- use_module(library(settings)).
:- use_module(library(dcg/basics)).
:- use_module(library(func)).
:- use_module(info).
:- use_module(utilities).


%--------------------------------------------------------------------------------%
% Connection Constants
%--------------------------------------------------------------------------------%

% DO NOT modify this file to set up a bot. When you
% check in code it'll publish your settings, including your
% password, and it'll always be a merge problem, and you'll have
% to set each time this file changes.
%
% Instead, alter the
% settings by querying set_setting/2 followed by save_settings/0. e.g.
% to change the port do
%
% ?- set_setting(config:port, 9999), save_settings.
%
% a file settings.db will appear in your working
% directory.
%
% DO NOT check in your
% settings.db file
%
% SEE THE BOTTOM OF THIS FILE FOR SOME EXTRA DOCUMENTATION WITH RESPECT TO A FEW
% OF THESE SETTINGS


% Constants for general server connection specs
:- setting(host, atom, 'chat.freenode.net', 'IRC host to connect to').
:- setting(port, between(1,0x7FFF), 6667, 'Port to connect to').
:- setting(nick, text, mybot, 'Bots nick (name) on IRC').
:- setting(pass, text, notpassword, 'Bots password.').
:- setting(chans, list(text), [ '##prolog', '##math' ],
     'List of channels to connect to').

:- setting(extensions, list(atom), [link_shortener, chat_log],
     'list of extensions to load').

% Constants for user registration specs
:- setting(bot_hostname, atom, hostname,
     'The user''s hostname (usually ignored).').

:- setting(bot_servername, atom, servername,
     'The user''s servername (usually ignored).').

:- setting(realname, atom, anonymous, 'Bot owner''s real name').

% Time interval for checking pings (connectivity safeguard)
:- setting(ping_interval, positive_integer, 300,
     'Time interval (in seconds) to check for pings and handle reconnection.').

:- initialization load_settings('settings.db').

host(Host) :-
  setting(host, Host).

port(Port) :-
  setting(port, Port).

nick(Nick) :-
  setting(nick, Nick).

pass(Pass) :-
  setting(pass, Pass).

chans(Chans) :-
  setting(chans, Chans).

bot_hostname(Name) :-
  setting(bot_hostname, BotHostName),
  (  BotHostName = use_host
  -> setting(host, Name)
  ;  Name = BotHostName
  ).

bot_servername(Name) :-
  setting(bot_servername, BotServerName),
  (  BotServerName = use_host
  -> setting(host, Name)
  ;  Name = BotServerName
  ).

bot_realname(Name) :-
  setting(realname, Name).

desired_extensions(List) :-
  setting(extensions, List).

time_limit(Limit) :-
  setting(ping_interval, Limit).


%--------------------------------------------------------------------------------%
% Extensions/Hot loading
%--------------------------------------------------------------------------------%


%% set_extensions(:Extensions:list(atom)) is det.
%
% Set the extensions to be loaded on startup. (Sanity checked)
set_extensions(Extensions) :-
  check_valid_extensions(Extensions),
  set_setting(config:extensions, Extensions).


%% add_new_extensions(+New:list(atom)) is semidet.
%
% Adds new extensions on top of whatever extensions are currently loaded in the
% the bot at the moment. These settings will not persist on restart; persisting
% these settings must be done by preceding a save_settings/0 call with this call.

add_new_extensions(New) :-
  setting(config:extensions, Es),
  append(New, Es, Extensions),
  load_new_extensions(Extensions).


%% load_new_extensions(+Es:list(atom)) is semidet.
%
% Load a new set of extensions and initalize them into the current bot session.
% This will not save these settings on restart. To make them persistent, this
% predicate call must precede a call to save_settings/0.

load_new_extensions(Es) :-
  check_valid_extensions(Es),
  set_setting(config:extensions, Es),
  retractall(info:extensions(_,_)),
  retractall(info:sync_extensions(_,_)),
  init_extensions.


%%%%%%%%%%%%%%%%%
% Sanity Checks %
%%%%%%%%%%%%%%%%%


%% script_extension(+File:atom, Without:atom) is semidet.
%
% True if File ends in `.pl` and Without is an atom devoid of this ending.
script_extension(File, Without) :-
  string_without(`.`, WO, atom_codes $ File, `.pl`),
  atom_codes(Without, WO).


%% is_script(+File:atom) is semidet.
%
% True if File is a prolog script file (ending in `.pl`).
is_script(File) :-
  script_extension(File, _).


%% check_valid_extensions(Es) is det.
%
% True iff Es is a valid subset of extensions. An existence error will be thrown
% otherwise.

check_valid_extensions(Es) :-
  (  valid_extensions(Es)
  -> true
  ;  existence_error(invalid_subset, Es)
  ).


%% valid_extensions(+Extensions:list(atom)) is semidet.
%
% True if Extensions is a subset of extensions available to yesbot.
valid_extensions(Extensions) :-
  % Get all files from extensions directory
  directory_files(extensions, Files),
  
  % Filter all files by pl scripts
  include(is_script, Files, A),
  
  % Transform all scripts into extension names
  maplist(script_extension, A, B),
  subset(Extensions, B).


%--------------------------------------------------------------------------------%
% Information from IRC RFC
%--------------------------------------------------------------------------------%


/*
  For those of you whom are curious about the IRC user registration process:
  These are relevant excerpts from Section 4.1.3 of the IRC RFC.
  (This document is available at: https://tools.ietf.org/html/rfc1459)
  
  Command: USER
  Parameters: <username> <hostname> <servername> <realname>

  The USER message is used at the beginning of connection to specify
  the username, hostname, servername and realname of a new user.  It is
  also used in communication between servers to indicate new user
  arriving on IRC, since only after both USER and NICK have been
  received from a client does a user become registered.

  Between servers USER must to be prefixed with client's NICKname.
  Note that hostname and servername are normally ignored by the IRC
  server when the USER command comes from a directly connected client
  (for security reasons), but they are used in server to server
  communication.  This means that a NICK must always be sent to a
  remote server when a new user is being introduced to the rest of the
  network before the accompanying USER is sent.

  It must be noted that realname parameter must be the last parameter,
  because it may contain space characters and must be prefixed with a
  colon (':') to make sure this is recognised as such.

  Since it is easy for a client to lie about its username by relying
  solely on the USER message, the use of an "Identity Server" is
  recommended.  If the host which a user connects from has such a
  server enabled the username is set to that as in the reply from the
  "Identity Server".

  Numeric Replies:

  ERR_NEEDMOREPARAMS              ERR_ALREADYREGISTRED

  Examples:
  
  USER guest tolmoon tolsun :Ronnie Reagan
*/


