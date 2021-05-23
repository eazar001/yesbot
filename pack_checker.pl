:- module(pack_checker, [install_deps/0]).

:- use_module(library(prolog_pack)).

pack_deps([
	irc_client,
	func,
	lambda,
	list_util
       %, plchatscript removed til plchatscript gets its act together
]).


install_deps :-
	search(Installs),
	install_packs(Installs).


list_packs(Packs) :-
	findall(Pack, prolog_pack:current_pack(Pack), Packs).


search(Installs) :-
	list_packs(Packs),
	pack_deps(Deps),
	subtract(Deps, Packs, Installs).


install_packs([]) :-
	writeln('All dependencies are met.').

install_packs([P|_]) :-
	pack_install(P),
	search(Remaining),
	install_packs(Remaining).
