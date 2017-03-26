:- object(yesbot).

	:- info([
		version is 2.0,
		author is 'Ebrahim Azarisooreh',
		date is 2017/3/25,
		comment is 'The yesbot IRC bot.'
	]).

	:- public(main/1).
	:- mode(main(+positive_integer), one).
	:- info(main/1, [
		comment is 'Connects to freenode and joins the ##prolog channel',
		arguments is [
			'Port' - 'The port that the documentation server is on. Used for the swi_object_search extension.'
		]
	]).

	:- public(connect/0).
	:- mode(connect, zero).
	:- info(connect/0, [
		comment is 'Connects to all the IRC channels specified by the user.',
		remarks is [
			'Extensions' - 'Set of extensions chosen by the user in config.lgt',
			'Connection' - 'Connections to IRC servers are spawned on a separate thread and joined on termination.',
			'Cleanup' - 'Cleanup routines are run for each connection'
			'Reconnections' - 'Reconnections after lost links are attempted after 2 minutes.'
		]
	]).

	:- public(yesbot_vsn/1).
	:- mode(yesbot_vsn(+atom), one).
	:- info(yesbot_vsn/1, [
		comment is 'Unifies with current version of yesbot',
		argnames is ['Version']
	]).

	:- use_module(library(irc_client)).
	:- use_module(library(socket)).

	:- uses(config, [
		host/1,
		port/1,
		nick/1,
		pass/1,
		chans/1,
		bot_hostname/1,
		bot_servername/1,
		bot_realname/1
	]).

	main(Port) :-
		swi_object_search::asserta(doc_port(Port)),
		thread_create(connect, _, [detached(true), alias(conn)]).

	connect :-
		repeat,
			init_extensions,
			catch(
				thread_create(join_channels, _, [alias(irc), at_exit(disconnect(irc))]),
				Err,
				print_message(error, Err)
			),
			thread_join(irc, _),
			writeln('Connection lost, attempting to reconnect ...'),
			sleep(120),
			fail.

	join_channels :-
		host(Host),
		port(Port),
		pass(Pass),
		nick(Nick),
		chans(Chans),
		bot_hostname(Hn),
		bot_servername(Sn),
		bot_realname(Rn),
		Names = [Hn, Sn, Rn],
		connect(Host, Port, Pass, Nick, Names, Chans).

	reload_version :-
		yesbot_vsn(Vsn),
		(	yesbot_version::yesbot_vsn(_)
		->	yesbot_version::retractall(yesbot_vsn(_)),
			yesbot_version::asserta(yesbot_vsn(Vsn))
		;	yesbot_version::asserta(yesbot_vsn(Vsn))
		).

:- end_object.
