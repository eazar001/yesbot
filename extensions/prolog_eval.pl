:- module(prolog_eval, [prolog_eval/1]).

:- use_module(library(irc_client)).
:- use_module(library(solution_sequences)).
:- use_module(library(yall)).
:- use_module(submodules/utils).
:- use_module(library(sandbox)).


target("##prolog", "yesbot").

prolog_eval(Msg) :-
	thread_create(ignore(prolog_eval_(Msg)), _, [detached(true)]).


prolog_eval_(Me-Msg) :-
	Msg = msg(_Prefix, "PRIVMSG", _, Text),
	append(`?-`, Rest, Text),
	string_codes(String, Rest),
	term_string(Goal, String, [variable_names(Vars)]),
	evaluate_with_errors(Me-Msg, safe_goal(Goal)),
	evaluate_with_errors(Me-Msg, call_with_time_limit(10, findall(Vars, limit(7, Goal), Sols))),
	call_with_time_limit(10, findall(Vars, limit(7, Goal), Sols)),
	determine_recipient(prolog_eval, Msg, Recip),
	maplist(include(nonvar_binding), Sols, Nonvars),
	evaluate(Me, Recip, Nonvars).


evaluate(Me, Recip, []) :-
	priv_msg(Me, "no.", Recip).

evaluate(Me, Recip, [[]]) :-
	priv_msg(Me, "yes.", Recip).

evaluate(Me, Recip, Sols) :-
	maplist(term_string, Sols, Strings),
	maplist(string_codes, Strings, Codes),
	maplist([Code, F]>>( Code=[_|R], append(F, [_], R) ), Codes, FormattedCodes),
	maplist([Msg]>>( priv_msg(Me, Msg, Recip), sleep(1) ), FormattedCodes).

nonvar_binding(_=Binding) :-
	nonvar(Binding).

evaluate_with_errors(Me-Msg, Goal) :-
	catch(
		Goal,
		Error,
		(	message_to_string(Error, ErrorString),
			determine_recipient(prolog_eval, Msg, Recip),
			priv_msg(Me, ErrorString, Recip),
			fail
		)
	).
