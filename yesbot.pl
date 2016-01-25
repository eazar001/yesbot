:- module(yesbot,
     [ connect/0
      ,main/1
      ,yesbot_vsn/1 ]).

:- use_module(library(irc_client)).
:- use_module(library(socket)).
:- use_module(library(func)).
:- use_module(library(lambda)).
:- use_module(config).

:- reexport(config, [goals_to_concurrent/2]).

:- initialization reload_version.

yesbot_vsn('1.4.6').


/** <module> Yesbot IRC bot
Extensible IRC bot written in Prolog.

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
  thread_create(connect, _, [detached(true), alias(conn)]).


%% connect is failure.
%
%  Intializes the extensions the user has chosen after consulting config.pl.
%  Then a connection is spawned on a separate thread and joined on termination.
%  cleanup routines are run with respect to that connection. The connection is
%  persistent and will attempt to reconnect after 2 minutes.

connect :-
  repeat,
    init_extensions,
    catch(
      thread_create(join_channels, _, [alias(irc), at_exit(disconnect(irc))]),
      Err,
      print_message(error, Err)
    ),
    thread_join(irc, _),
    writeln("Connection lost, attempting to reconnect ..."),
    sleep(120),
    fail.


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


reload_version :-
  ['extensions/yesbot_version'],
  (  yesbot_version:yesbot_vsn(_)
  -> retractall(yesbot_version:yesbot_vsn(_))
  ;  true
  ),
  yesbot_vsn(Vsn),
  asserta(yesbot_version:yesbot_vsn(Vsn)).
