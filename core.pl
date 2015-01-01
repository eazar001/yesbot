%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                %
%                                                                                %
% Author: Ebrahim Azarisooreh                                                    %
% E-mail: ebrahim.azarisooreh@gmail.com                                          %
% IRC Nick: eazar001                                                             %
% Title: Yes-Bot                                                                 %
% Descripton: IRC Bot                                                            %
%                                                                                %
%                                                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- module(core, []).

:- use_module(config).
:- use_module(parser).
:- use_module(dispatch).
:- use_module(extensions/chat_log).
:- use_module(extensions/link_shortener).
:- use_module(library(socket)).

%--------------------------------------------------------------------------------%
% connection details
%--------------------------------------------------------------------------------%


%% connect is nondet.
%
% Open socket on host, port, nick, user, hostname, and servername that will all
% be specified in the bot_config module. The socket stream that is established
% will be asserted at the top level for access by anywhere in the program.

connect :-
  debug,
  host(Host),
  port(Port),
  nick(Nick),
  pass(Pass),
  chan(Chan),
  setup_call_cleanup(
    (
       init_structs(Nick, Pass, Chan),
       tcp_socket(Socket),
       tcp_connect(Socket, Host:Port, Stream),
       asserta(get_irc_stream(Stream)),
       register_and_join
    ),
    read_server_loop(_Reply),
    disconnect).


%--------------------------------------------------------------------------------%


%% register_and_join is det.
%
% Present credentials and register user on the irc server.

register_and_join :-
  send_msg(pass),
  send_msg(user),
  send_msg(nick),
  send_msg(join).


%--------------------------------------------------------------------------------%


%% init_structs(+Nick, +Pass, +Chan) is det.
%
% Assert the 'connection' structure at the top level so that access to important
% user information is available at the top level throughout the program. All of
% this information should be specified in the bot_config module.

init_structs(Nick, Pass, Chan) :-
  bot_hostname(HostName),
  bot_servername(ServerName),
  bot_realname(RealName),
  Connect = connection(
    Nick
   ,Pass
   ,Chan
   ,HostName
   ,ServerName
   ,RealName),
  asserta(Connect).


init_extensions(Extensions) :-
  directory_files(extensions, Ms),
  exclude(call(core:non_file), Ms, Modules),
  maplist(core:remove_suffix, Modules, Extensions).

non_file('.').
non_file('..').

remove_suffix(File, Extension) :-
  once(sub_atom(File, _, _, 3, Extension)).


%--------------------------------------------------------------------------------%
% server routing
%--------------------------------------------------------------------------------%


% XXX TODO : Implement extension support in this section.

%% read_server_loop(-Reply) is nondet.
%
% Read the server output one line at a time. Each line will be sent directly
% to a predicate that is responsible for handling the output that it receives.
% The program will terminate successfully if EOF is reached.

read_server_loop(Reply) :-
  get_irc_stream(Stream),
  repeat,
  read_server(Reply, Stream),
  Reply = end_of_file, !.


% XXX TODO : Implement async and sync processing here. User must have a choice.

%% read_server(-Reply, +Stream) is det.
%
% Translate server line to codes. If the codes are equivalent to EOF then succeed
% and go back to the main loop for termination. If not then then display the
% contents of the server message and process the reply.

read_server(Reply, Stream) :-
  read_line_to_codes(Stream, Reply),
  (
     Reply = end_of_file, !
  ;
     thread_create(read_server_handle(Reply), _, [])
  ).

read_server_handle(Reply) :-
  concurrent(2,
    [ run_det(process_server(Reply))
     ,run_det(format('~s~n', [Reply])) ], []),
  throw(done).

%--------------------------------------------------------------------------------%


% XXX TODO : add server processing extension zone here as well

%% process_server(+Reply) is nondet.
%
% All processing of server message will be handled here. Pings will be handled by
% responding with a pong to keep the connection alive. Anything else will be
% processed as a private message. Finally, all server messages will be displayed
% in raw form via stdout. Further server processing extensions should be
% implemented dynamically in this section.

process_server(Reply) :-
  connection(Nick, _Pass, _Chan,
    _Hname, _Sname, _Rname),
  (
     ping_from(Reply, Origin) ->
       send_msg(pong, Nick, Origin)
     ;
       process_priv_msg(Reply)
  ).


%--------------------------------------------------------------------------------%
% handle incoming private messages
%--------------------------------------------------------------------------------%


%% process_priv_msg(+Msg) is nondet.
%
% All extensions that deal specifically with handling private messages should be
% implemented dynamically in this section. The extensions will be plugged into
% a disjunctive swith that follows a successful parse of a private message.

process_priv_msg(Msg) :-
  priv_msg(Nick, Recip_, Msg, Rest),
  atom_codes(Recip, Recip_),
  connection(_Nick, _Pass, Chan,
    _Hname, _Sname, _Rname),
  get_irc_stream(Stream),
  concurrent(2,
    [ run_det(link_shortener(Stream, Recip, Chan, Rest))
     ,run_det(chat_log(Rest, Nick, Recip, Chan)) ], []).


%--------------------------------------------------------------------------------%


%% run_det(+Goal) is det.
%
% find all the solutions to an extensionized goal in order to precipitate the
% result as unevaluated deterministic result. Used here for making extension
% work concurrent.

run_det(Goal) :-
  findall(_, Goal, _).


%--------------------------------------------------------------------------------%
% cleanup/termination
%--------------------------------------------------------------------------------%


%% disconnect is det.
%
% Clean up top level information access structures, issue a disconnect command
% to the irc server, and close the socket stream pair.

disconnect :-
  nodebug,
  get_irc_stream(Stream),
  send_msg(quit),
  retractall(get_irc_stream(_)),
  retractall(connection(_,_,_,_,_,_)),
  close(Stream).


