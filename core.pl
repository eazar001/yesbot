
:- module(core,
     [ connect/0
      ,main/0
      ,assert_handlers/0 ]).

:- use_module(library(irc_client)).
:- use_module(library(socket)).
:- use_module(library(func)).
:- use_module(library(lambda)).
:- use_module(config).

:- reexport(config, [goals_to_concurrent/2]).


% FIXME: On reconnect, there are a bunch of httpd workers that are left
% to be garbage collected. They seem to be related to the doc server,
% after the corresponding message queue has been destroyed. Find a way
% to deal with the existence errors that are causing problems here.

main :-
  thread_create(connect, _, [detached(true),alias(conn)]),
  thread_signal(conn, attach_console).


connect :-
  setup_call_cleanup(
    (  assert_handlers,
       thread_create(join_prolog, _, [alias(irc)])
    ),
    thread_join(irc, _),
    disconnect(irc)
  ),
  join_threads,
  message_queue_create(_),
  connect.


join_prolog :-
  host(Host),
  port(Port),
  pass(Pass),
  nick(Nick),
  chans(Chans),
  bot_hostname(Hn),
  bot_servername(Sn),
  bot_realname(Rn),
  Names = [Hn,Sn,Rn],
  connect(Host, Port, Pass, Nick, Names, Chans).


assert_handlers :-
  init_extensions.


