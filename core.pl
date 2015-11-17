
:- module(core,
     [ connect/0
      ,main/1 ]).

:- use_module(library(irc_client)).
:- use_module(library(socket)).
:- use_module(library(func)).
:- use_module(library(lambda)).
:- use_module(config).

:- reexport(config, [goals_to_concurrent/2]).


main(Doc) :-
  asserta(swi_object_search:doc_port(Doc)),
  thread_create(connect, _, [detached(true),alias(conn)]).


connect :-
  setup_call_cleanup(
    (  init_extensions,
       thread_create(join_prolog, _, [alias(irc)])
    ),
    thread_join(irc, _),
    disconnect(irc)
  ),
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


