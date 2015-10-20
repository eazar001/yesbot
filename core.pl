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

:- module(core, [ connect/0
		 ,disconnect/0 ]).

:- use_module(config).
:- use_module(parser).
:- use_module(dispatch).
:- use_module(utilities).
:- use_module(info).
:- use_module(library(socket)).
:- use_module(library(func)).
:- use_module(library(mavis)).


%--------------------------------------------------------------------------------%
% Connection Details
%--------------------------------------------------------------------------------%


%% connect is nondet.
%
%  Open socket on host, port, nick, user, hostname, and servername that will all
%  be specified in the bot_config module. The socket stream that is established
%  will be asserted at the top level for access from anywhere in the program.

connect :-
  host(Host),
  port(Port),
  setup_call_cleanup(
    (
       init_extensions,
       init_structs,
       tcp_socket(Socket),
       tcp_connect(Socket, Host:Port, Stream),
       stream_pair(Stream, _Read, Write),
       asserta(info:get_irc_write_stream(Write)),
       set_stream(Write, encoding(utf8)),
       asserta(info:get_tcp_socket(Socket)),
       asserta(info:get_irc_stream(Stream)),
       register_and_join
    ),
    read_server_loop(_Reply),
    reconnect
  ).


%% register_and_join is semidet.
%
%  Present credentials and register user on the irc server.
register_and_join :-
  maplist(send_msg, [pass, user, nick, join]).


%% init_structs is det.
%
%  Assert the 'connection' structure at the top level so that access to important
%  user information is available at the top level throughout the program. All of
%  this information should be specified in the bot_config module.

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
  asserta(info:Connection).


%--------------------------------------------------------------------------------%
% Server Routing
%--------------------------------------------------------------------------------%


%% read_server_loop(-Reply:codes) is nondet.
%
%  Read the server output one line at a time. Each line will be sent directly
%  to a predicate that is responsible for handling the output that it receives.
%  The program will terminate successfully if EOF is reached.

read_server_loop(Reply) :-
  get_irc_stream(Stream),
  init_timer(_TQ),
  init_smq(_SMQ),
  asserta(known(tq)),
  repeat,
    read_server(Reply, Stream), !.


%% read_server(-Reply:codes, +Stream) is nondet.
%
%  Translate server line to codes. If the codes are equivalent to EOF then succeed
%  and go back to the main loop for termination. If not then then display the
%  contents of the server message and process the reply.

read_server(Reply, Stream) :-
  read_line_to_codes(Stream, Reply),
  (  Reply = end_of_file
  -> true
  ;  read_server_handle(Reply),
     fail
  ).


%% read_server_handle(+Reply:codes) is det.
%
%  Concurrently process server lines via loaded extensions and output the server
%  line to stdout for debugging.

read_server_handle(Reply) :-
  parse_line(Reply, Msg),
  thread_create(run_det(core:process_server(Msg)), _Id, [detached(true)]),
  thread_send_message(smq, Msg),
  format('~s~n', [Reply]).


%% process_server(+Msg:compound) is nondet.
%
%  All processing of server message will be handled here. Pings will be handled by
%  responding with a pong to keep the connection alive. If the message is "001"
%  or a server "welcome", then a successful connection to a server will be
%  assumed. In this case, all instances of get_irc_server/1 will be retracted,
%  and the new server will be asserted for use. It is important that this is
%  serialized with respect to process_msg/1 so as to avoid race conditions.
%  Anything else will be processed as an incoming message.

process_server(Msg) :-
  thread_send_message(tq, true),
  (  % Handle pings
     Msg = msg("PING", [], O),
     string_codes(Origin, O),
     send_msg(pong, Origin)
  ;  % Get irc server and assert info
     Msg = msg(Server, "001", _, _),
     retractall(get_irc_server(_)),
     asserta(get_irc_server(Server)),
     asserta(known(irc_server)),
     % Request own user info
     nick(Nick),
     send_msg(who, atom_string $ Nick)
  ;  % Get own host and nick info
     Msg = msg(_Server, "352", Params, _),
     nick(N),
     atom_string(N, Nick),
     Params = [_Asker, _Chan, H, Host, _, Nick| _],
     % Calculate the minimum length for a private message and assert info
     format(string(Template), ':~s!~s@~s PRIVMSG :\r\n ', [Nick,H,Host]),
     asserta(info:min_msg_len(string_length $ Template))
  ;  % Run extensions
     process_msg(Msg)
  ).


%--------------------------------------------------------------------------------%
% Handle Incoming Server Messages
%--------------------------------------------------------------------------------%


%% process_msg(+Msg:compound) is nondet.
%
%  All extensions that deal specifically with handling messages should be
%  implemented dynamically in this section. The extensions will be plugged into
%  an execution list that follows a successful parse of a private message.

process_msg(Msg) :-
  extensions(Async, N),
  (  N > 0
  -> maplist(run_det(Msg), Async)
  ;  true
  ).


%--------------------------------------------------------------------------------%
% Cleanup/Termination
%--------------------------------------------------------------------------------%


%% reconnect is semidet.
%
%  Disconnect from the server, run cleanup routine, and attempt to reconnect.
reconnect :-
  disconnect,
  repeat,
    writeln("Connection lost, attempting to reconnect ..."),
    (  catch(connect, _E, fail)
    -> !
    ;  sleep(30),
       fail
    ).


%% disconnect is semidet.
%
%  Clean up top level information access structures, issue a disconnect command
%  to the irc server, close the socket stream pair, and attempt to reconnect.

disconnect :-
  get_irc_stream(Stream),
  send_msg(quit),
  info_cleanup,
  message_queue_destroy(tq),
  message_queue_destroy(smq),
  thread_join(ping_checker, _),
  thread_join(sync_worker, _),
  close(Stream).


