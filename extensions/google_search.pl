
:- module(google_search, [google_search/1]).

:- use_module(dispatch).
:- use_module(submodules/html).
:- use_module(library(sgml)).
:- use_module(library(http/http_open)).
:- use_module(library(xpath)).
:- use_module(library(uri)).


% TBD: Add support for other google search features such as google conversion,
% currency, wiki, translate, calculator, etc.

chan("##prolog").
google_start(`http://www.google.com/search?hl=en&q=`).

google_end(`&btnI=I\'m+Feeling+Lucky`).

google_search(Msg) :-
  thread_create(ignore(google_search_(Msg)), _Id, [detached(true)]).


google_search_(Msg) :-
  chan(Chan),
  Msg = msg(_Prefix, "PRIVMSG", [Chan], Text),
  append(`?google `, Q0, Text),
  atom_codes(Atom, Q0),
  uri_encoded(query_value, Atom, Encoded),
  atom_codes(Encoded, Q),
  append(Q, Diff, Query),
  google_start(Start),
  google_end(End),
  append(Start, Query, L),
  Diff = End,
  string_codes(Link, L),
  setup_call_cleanup(
    http_open(Link, Stream,
      [ final_url(URL)
       ,header('Content-Type', Type)
       ,cert_verify_hook(cert_verify)
       ,timeout(20) ]),
    (
     
       (
          atom_concat('text/html', _, Type),
	  Opts = [max_errors(-1)]
       ;
	  Type = '',
	  Opts = [max_errors(50)]
       )
    ->
       load_html(Stream, Structure, Opts),
       xpath_chk(Structure, //title, Tstruct),
       Tstruct = element(title, _, [T0]), string_codes(T0, T),
       maplist(change, T, Title),
       send_msg(priv_msg, Title, Chan),
       send_msg(priv_msg, URL, Chan)
    ;
       true
    ),
    close(Stream)
  ).

cert_verify(_SSL, _ProblemCert, _AllCerts, _FirstCert, _Error) :-
  format(user_error, 'Accepting certificate~n', []).

