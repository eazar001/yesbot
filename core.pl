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

:- module(core, []).

:- use_module(config).
:- use_module(parser).
:- use_module(dispatch).
:- use_module(utilities).
:- use_module(library(socket)).
:- use_module(library(aggregate)).

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
  nick(Nick),
  pass(Pass),
  findall(Chan, chan(Chan), Chans),
  setup_call_cleanup(
    (
       init_extensions,
       init_structs(Nick, Pass, Chans),
       tcp_socket(Socket),
       tcp_connect(Socket, Host:Port, Stream),
       asserta(get_irc_stream(Stream)),
       register_and_join
    ),
    read_server_loop(_Reply),
    disconnect
  ).


%% register_and_join is det.
%
% Present credentials and register user on the irc server.

register_and_join :-
  send_msg(pass),
  send_msg(user),
  send_msg(nick),
  send_msg(join).


%% init_structs(+Nick, +Pass, +Chans) is det.
%
% Assert the 'connection' structure at the top level so that access to important
% user information is available at the top level throughout the program. All of
% this information should be specified in the bot_config module.

init_structs(Nick, Pass, Chans) :-
  bot_hostname(HostName),
  bot_servername(ServerName),
  bot_realname(RealName),
  Connect = connection(
    Nick
   ,Pass
   ,Chans
   ,HostName
   ,ServerName
   ,RealName),
  asserta(Connect).


%--------------------------------------------------------------------------------%
% Extension loading
%--------------------------------------------------------------------------------%


%% preload_exists(-Extensions) is det.
%
% Check whether or not the preload setting is enabled in the configuration file.
% If it is, then the extensions list will be the one predetermined by preload/1.
% If not, the extensions directory is sourced and the user is prompted for action.

preload_exists(Extensions) :-
  current_predicate(config:preload/1) ->
    config:preload(Extensions)
  ;
    directory_files(extensions, Ms),
    include(core:is_extension, Ms, Modules),
    maplist(core:make_goal, Modules, Es),
    prompt_ext(Es, Extensions).


%% init_extensions is det.
%
% Assert the extensions along with its length at the top level for access.
% Import all the modules afterwards.

init_extensions :-
  preload_exists(Extensions),
  length(Extensions, N),
  asserta(extensions(Extensions, N)),
  maplist(import_extension_module, Extensions).


%% Load an extension from the 'extensions' directory
import_extension_module(Extension) :-
  use_module(extensions/Extension).


%% All extension candidates end in '.pl'
is_extension(X) :-
  atom_codes(X, Codes),
  is_extension(Codes, []).

is_extension --> `.pl`.
is_extension --> [_], is_extension.


%% transform files into goals per the rules of the system
make_goal(File, Goal) :-
  once(sub_atom(File, _, _, 3, F)),
  Goal =.. [F].


%% prompt_ext(+Es, -Ms) is det.
%
% Store all the modules that the user decided to load.

prompt_ext([], _) :-
  writeln('Warning : You have no extensions loaded.').

prompt_ext([E|Es], Ms) :-
  findall(M, prompt_ext_([E|Es], M), Ms).

prompt_ext_(Es, Module) :-
  writeln('Select the extensions you want to load.'),
  format('Enter "y." for yes and "n." for no (without the quotes).~n', []),
  member(Module, Es),
  format('Load "~a" extension? > ', [Module]),
  read(y).

   
%--------------------------------------------------------------------------------%
% Server Routing
%--------------------------------------------------------------------------------%


%% read_server_loop(-Reply) is nondet.
%
% Read the server output one line at a time. Each line will be sent directly
% to a predicate that is responsible for handling the output that it receives.
% The program will terminate successfully if EOF is reached.

read_server_loop(Reply) :-
  get_irc_stream(Stream),
  init_queue(_MQ),
  get_server_name(Stream),
  repeat,
  read_server(Reply, Stream),
  Reply = end_of_file, !.


%% get_server_name(+Stream) is nondet.
%
% Print the first 5 lines from the server. On the 5th line, get the server
% hostname (should be on the line that is code 001).

get_server_name(Stream) :-
  between(1, 5, X),
  read_line_to_codes(Stream, Reply),
  format('~s~n', [Reply]),
  X = 5,
  parse_line(Reply, msg(Server,_,_,_)),
  asserta(get_irc_server(Server)).


%% read_server(-Reply, +Stream) is semidet.
%
% Translate server line to codes. If the codes are equivalent to EOF then succeed
% and go back to the main loop for termination. If not then then display the
% contents of the server message and process the reply.

read_server(Reply, Stream) :-
  read_line_to_codes(Stream, Reply),
  (
     Reply = end_of_file ->
       true
     ;
       thread_send_message(mq, read_server_handle(Reply))
  ).


%% read_server_handle(+Reply) is semidet.
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
% responding with a pong to keep the connection alive. Anything else will be
% processed as an incoming message. Further server processing extensions should
% be implemented dynamically in this section.

process_server(Reply) :-
  parse_line(Reply, Msg),
  (
     Msg = msg("PING", [], Origin) ->
       send_msg(pong, Origin)
     ;
       process_msg(Msg)
  ).


%--------------------------------------------------------------------------------%


%% init_queue(+Id) is det.
%
% Initialize a message queue to store server lines to be processed in the future.
% Server lines will be processed sequentially.

init_queue(Id) :-
  message_queue_create(Id, [alias(mq)]),
  thread_create(start_job(Id), _, [alias(msg_handler)]).


%% start_job(+Id) is nondet.
%
% Wait for any messages directed to the Id of the message queue. Fetch the
% message from the thread and call Goal. Catch any errors and print the messages.
% Keep thread alive to watch for new jobs to execute.

start_job(Id) :-
  repeat,
  thread_get_message(Id, Goal),
  (
     catch(Goal, E, print_message(error, E)) ->
       true
     ;
       print_message(error, goal_failed(Goal, worker(Id)))
  ),
  fail.


%--------------------------------------------------------------------------------%
% Handle Incoming Server Messages
%--------------------------------------------------------------------------------%


%% process_msg(+Msg) is nondet.
%
% All extensions that deal specifically with handling messages should be
% implemented dynamically in this section. The extensions will be plugged into
% an execution list that follows a successful parse of a private message.

process_msg(Msg) :-
  extensions(Es, N),
  N > 0 ->
    maplist(run_det(Msg), Es, Extensions),
    concurrent(N, Extensions, [])
  ;
    true.


%--------------------------------------------------------------------------------%
% Cleanup/Termination
%--------------------------------------------------------------------------------%


%% disconnect is det.
%
% Clean up top level information access structures, issue a disconnect command
% to the irc server, and close the socket stream pair.

disconnect :-
  get_irc_stream(Stream),
  send_msg(quit),
  retractall(get_irc_stream(_)),
  retractall(connection(_,_,_,_,_,_)),
  retractall(extensions(_,_)),
  thread_signal(msg_handler, throw(thread_quit)),
  message_queue_destroy(mq),
  close(Stream).


