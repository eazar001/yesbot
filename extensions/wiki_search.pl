
:- module(wiki_search, [wiki_search/1]).

:- use_module(library(sgml)).
:- use_module(library(http/http_open)).
:- use_module(library(xpath)).
:- use_module(library(uri)).
:- use_module(dispatch).
:- use_module(submodules/html).
:- use_module(submodules/utils).


target("##prolog", "yesbot").
wiki_start(`http://www.wikipedia.org/search-redirect.php?family=wikipedia&search=`).
wiki_end(`&language=en&go=Go`).


wiki_search(Msg) :-
  ignore(wiki_search_(Msg)).


wiki_search_(Msg) :-
  Msg = msg(_Prefix, "PRIVMSG", _, Text),
  append(`?wiki `, Q0, Text),
  atom_codes(Atom, Q0),
  append(atom_codes $ uri_encoded(query_value) $ Atom, Diff, Query),
  wiki_start(Start),
  wiki_end(End),
  append(Start, Query, L),
  Diff = End,
  string_codes(Link, L),
  setup_call_cleanup(
    http_open(Link, Stream, [timeout(20), final_url(URL)]),
    found(Msg, Stream, URL),
    close(Stream)
  ).


found(Msg, Stream, URL) :-
  determine_recipient(wiki_search, Msg, Rec),
  load_html(Stream, Content, []),
  (
     xpath_chk(Content, //table(@id=disambigbox), _Rest),
     xpath_chk(Content, //li(//a(@title=T)), _),
     atom_codes(T, Title),
     append(`?wiki `, Title, New),
     Paragraph = "Disambiguating with first match",
     send_msg(priv_msg, Paragraph, Rec),
     target(Chan, Bot),
     (
        Rec = Chan
     ->
        wiki_search_(msg(_, "PRIVMSG", [Rec], New))
     ;
	Msg = msg(Prefix, _, _, _),
	NewMsg = msg(Prefix, "PRIVMSG", [Bot], New),
	wiki_search_(NewMsg)
     )
  ;
     xpath_chk(Content, //a(@title='This is a special page which you cannot edit'), _),
     Paragraph = "Page does not exist",
     send_msg(priv_msg, Paragraph, Rec), !
  ;
     xpath_chk(Content, //p(normalize_space), P0),
     clean_sequence(atom_codes $ P0, Paragraph),
     (  Paragraph \= `There were no results matching the query.`
     -> send_msg(priv_msg, URL, Rec),
	send_msg(priv_msg, Paragraph, Rec)
     ;  send_msg(priv_msg, Paragraph, Rec)
     )
  ).

		    
