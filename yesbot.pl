:- module(yesbot,
     [ connect/0
      ,main/1 ]).

:- use_module(library(irc_client)).
:- use_module(library(socket)).
:- use_module(library(func)).
:- use_module(library(lambda)).
:- use_module(config).

:- reexport(config, [goals_to_concurrent/2]).

/** <module> yesbot
IRC bot written in prolog.

@author Ebrahim Azarisooreh
@license MIT

@tbd Add support for multiple chat servers

*/

  
%% main(+Doc) is det.
%
%  Start a connection and join the ##prolog channel on freenode. Doc is a port
%  number of a SWI prolog documentation server; this argument is of interest
%  only if the swi_object_search extension is enabled. The server _must_ be run
%  on a separate instance of SWI prolog listener.

main(Doc) :-
  asserta(swi_object_search:doc_port(Doc)),
  thread_create(connect, _, [detached(true),alias(conn)]).


%% connect
%
%  Intializes the extensions the user has chosen after consulting config.pl.
%  Then a connection is spawned on a separate thread and joined on termination.
%  cleanup routines are run with respect to that connection. The connection is
%  persistent and will attempt to reconnect after 2 minutes.

connect :-
  setup_call_cleanup(
    (  init_extensions,
       thread_create(join_channels, _, [alias(irc)])
    ),
    thread_join(irc, _),
    disconnect(irc)
  ),
  writeln("Connection lost, attempting to reconnect ..."),
  sleep(120),
  connect.


%% join_channels is semidet.
%
%  Calls the specs necessary for establishing a connection (from config.pl). Then
%  an attempt is made to join the server and channels (if specified). Defaults
%  are available in config.pl.

join_channels :-
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
