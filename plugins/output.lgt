:- object(output).

	:- info([
		version is 1.0,
		author is 'Ebrahim Azarisooreh',
		date is 2017/3/26,
		comment is 'Output messages from IRC communications.'
	]).

	:- public(output/1).
	:- mode(output(+compound), one).
	:- info(output/1, [
		comment is 'Description',
		argnames is ['Msg']
	]).

	output(Msg) :-
		thread_create(output_(Msg), _, [detached(true)]).

	output_(Id-msg(Prefix, Cmd, Params, Trailer)) :-
		format('~s: ~s ~s ~w ~s~n', [Id, Prefix, Cmd, Params, Trailer]).

	output_(Id-msg(Prefix, Cmd, Params)) :-
		\+ is_list(Cmd),
		!,
		format('~s: ~s ~s ~w~n', [Id, Prefix, Cmd, Params]).

	output_(Id-msg(Cmd, Params, Trailer)) :-
		format('~s: ~s ~w ~s~n', [Id, Cmd, Params, Trailer]).

	output_(Id-msg(Cmd, Params)) :-
		format('~s: ~s ~w~n', [Id, Cmd, Params]).

:- end_object.
