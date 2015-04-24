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

:- module(core,
     [ connect/0
      ,disconnect/0 ]).

:- use_module(config).
:- use_module(parser).
:- use_module(dispatch).
:- use_module(utilities).
:- use_module(library(socket)).
:- use_module(library(mavis)).

:- dynamic known/1.
:- dynamic get_irc_server/1.
:- dynamic get_irc_stream/1.
:- dynamic get_tcp_socket/1.
:- dynamic connection/6.

%--------------------------------------------------------------------------------%
% Connection Details
%--------------------------------------------------------------------------------%


%% connect is nondet.
%
% Open socket on host, port, nick, user, hostname, and servername that will all
% be specified in the bot_config module. The socket stream that is established
% will be asserted at the top level for access from anywhere in the program.

connect :-
  host(Host),
  port(Port),
  setup_call_cleanup(
    (
       init_extensions,
       init_structs,
       tcp_socket(Socket),
       tcp_connect(Socket, Host:Port, Stream),
       stream_pair(Stream, Read, Write),
       set_stream(Write, encoding(utf8)),
       asserta(get_tcp_socket(Socket)),
       asserta(get_irc_stream(Stream)),
       register_and_join
    ),
    read_server_loop(_Reply),
    reconnect
  ).


%% register_and_join is semidet.
%
% Present credentials and register user on the irc server.

register_and_join :-
  maplist(send_msg, [pass, user, nick, join]).


%% init_structs is det.
%
% Assert the 'connection' structure at the top level so that access to important
% user information is available at the top level throughout the program. All of
% this information should be specified in the bot_config module.

init_structs :-
  nick(N_),
  pass(P_),
  chans(Chans_),
  maplist(atom_string, Chans_, Chans),
  bot_hostname(Hn_),
  bot_servername(Sn_),
  bot_realname(Rn_),
  maplist(atom_string, [N_, P_, Hn_, Sn_, Rn_], Strs),
  Strs = [N, P, Hn, Sn, Rn],
  Connection =.. [connection, N, P, Chans, Hn, Sn, Rn],
  asserta(Connection).


%--------------------------------------------------------------------------------%
% Extension Loading
%--------------------------------------------------------------------------------%


:- dynamic extensions/2.

%% init_extensions is semidet.
%
% Assert the extensions along with its length at the top level for access.
% Import all the modules afterwards.

init_extensions :-
  desired_extensions(Extensions),
  length(Extensions, N),
  asserta(extensions(Extensions, N)),
  maplist(import_extension_module, Extensions).


%% import_extension_module(+Extensions:atom) is semidet.

%% Load an extension from the 'extensions' directory
import_extension_module(Extension) :-
  use_module(extensions/Extension).


%--------------------------------------------------------------------------------%
% Server Routing
%--------------------------------------------------------------------------------%


%% read_server_loop(-Reply:codes) is nondet.
%
% Read the server output one line at a time. Each line will be sent directly
% to a predicate that is responsible for handling the output that it receives.
% The program will terminate successfully if EOF is reached.

read_server_loop(Reply) :-
  get_irc_stream(Stream),
  init_queue(_MQ),
  init_timer(_TQ),
  asserta(known(tq)),
  repeat,
    read_server(Reply, Stream),
    Reply = end_of_file, !.


%% read_server(-Reply, +Stream:codes) is nondet.
%
% Translate server line to codes. If the codes are equivalent to EOF then succeed
% and go back to the main loop for termination. If not then then display the
% contents of the server message and process the reply.

read_server(Reply, Stream) :-
  read_line_to_codes(Stream, Reply),
  (  Reply = end_of_file
  -> true
  ;  thread_send_message(mq, read_server_handle(Reply))
  ).


%% read_server_handle(+Reply) is det.
%
% Concurrently process server lines via loaded extensions and output the server
% line to stdout for debugging.

read_server_handle(Reply) :-
  concurrent(2,
    [ run_det(core:process_server(Reply))
     ,format('~s~n', [Reply]) ], []).


%% process_server(+Reply) is nondet.
%
% All processing of server message will be handled here. Pings will be handled by
% responding with a pong to keep the connection alive. If the message is "001"
% or a server "welcome", then a successful connection to a server will be
% assumed. In this case, all instances of get_irc_server/1 will be retracted,
% and the new server will be asserted for use. It is important that this is
% serialized with respect to process_msg/1 so as to avoid race conditions.
% Anything else will be processed as an incoming message.

process_server(Line) :-
  parse_line(Line, Msg),
  thread_send_message(tq, true),
  (
     Msg = msg("PING", [], Origin),
     send_msg(pong, Origin)
  ;
     Msg = msg(Server, "001", _, _),
     retractall(get_irc_server(_)),
     asserta(get_irc_server(Server)),
     asserta(known(irc_server))
  ;
     process_msg(Msg)
  ).


%--------------------------------------------------------------------------------%


%% init_queue(-Id:integer) is semidet.
%
% Initialize a message queue to store server lines to be processed in the future.
% Server lines will be processed sequentially.

init_queue(Id) :-
  message_queue_create(Id, [alias(mq)]),
  thread_create(start_job(Id), _, [alias(msg_handler)]).


%% start_job(+Id:integer) is failure.
%
% Wait for any messages directed to the Id of the message queue. Fetch the
% message from the thread and call Goal. Catch any errors and print the messages.
% Keep thread alive to watch for new jobs to execute.

start_job(Id) :-
  repeat,
    thread_get_message(Id, Goal),
    (  catch(Goal, E, print_message(error, E))
    -> true
    ;  print_message(error, goal_failed(Goal, worker(Id)))
    ),
    fail.


%--------------------------------------------------------------------------------%
% Handle Incoming Server Messages
%--------------------------------------------------------------------------------%


%% process_msg(+Msg:acyclic) is nondet.
%
% All extensions that deal specifically with handling messages should be
% implemented dynamically in this section. The extensions will be plugged into
% an execution list that follows a successful parse of a private message.

process_msg(Msg) :-
  extensions(Es, N),
  (
     N > 0
  ->
     maplist(run_det(Msg), Es, Extensions),
     concurrent(N, Extensions, [])
  ;
     true
  ).


%--------------------------------------------------------------------------------%
% Cleanup/Termination
%--------------------------------------------------------------------------------%


%% reconnect is semidet.
%
% Disconnect from the server, run cleanup routine, and attempt to reconnect.

reconnect :-
  disconnect,
  connect.


%% disconnect is semidet.
%
% Clean up top level information access structures, issue a disconnect command
% to the irc server, close the socket stream pair, and attempt to reconnect.

disconnect :-
  get_irc_stream(Stream),
  send_msg(quit),
  retractall(get_irc_stream(_)),
  retractall(connection(_,_,_,_,_,_)),
  retractall(extensions(_,_)),
  retractall(get_irc_server(_)),
  retractall(known(_)),
  message_queue_destroy(mq),
  message_queue_destroy(tq),
  thread_join(ping_checker, _),
  thread_join(msg_handler, _),
  get_tcp_socket(Socket),
  tcp_close_socket(Socket),
  retractall(get_socket(_)),
  close(Stream).


