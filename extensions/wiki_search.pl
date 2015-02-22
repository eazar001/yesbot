
:- module(wiki_search, [wiki_search/1]).

:- use_module(library(sgml)).
:- use_module(library(http/http_open)).
:- use_module(library(xpath)).
:- use_module(library(uri)).
:- use_module(dispatch).
:- use_module(submodules/html).


chan("##prolog").
wiki_start(`http://www.wikipedia.org/search-redirect.php?family=wikipedia&search=`).
wiki_end(`&language=en&go=Go`).


wiki_search(Msg) :-
  thread_create(ignore(wiki_search_(Msg)), _Id, [detached(true)]).


wiki_search_(Msg) :-
  chan(Chan),
  Msg = msg(_Prefix, "PRIVMSG", [Chan], Text),
  append(`?wiki `, Q0, Text),
  atom_codes(Atom, Q0),
  uri_encoded(query_value, Atom, Encoded),
  atom_codes(Encoded, Q),
  append(Q, Diff, Query),
  wiki_start(Start),
  wiki_end(End),
  append(Start, Query, L),
  Diff = End,
  string_codes(Link, L),
  setup_call_cleanup(
    http_open(Link, Stream, [timeout(20), final_url(URL)]),
    (
       load_html(Stream, Content, []),
       xpath_chk(Content, //p(normalize_space), P0),
       atom_codes(P0, P),
       maplist(change, P, Paragraph),
       send_msg(priv_msg, URL, Chan),
       send_msg(priv_msg, Paragraph, Chan)
    ),
    close(Stream)
  ).


