
:- module(help, [help/1]).

:- use_module(dispatch).

chan("##prolog").

help(Msg) :-
  chan(Chan),
  Msg = msg(_Prefix, "PRIVMSG", [Chan], Rest),
  (
     append(`?help `, Q0, Rest),
     string_codes(Q, Q0),
     normalize_space(string(Ext), Q),
     once(ext_help(Ext, Response)),
     send_msg(priv_msg, Response, Chan), !
  ;
     Rest = `?help`,
     help_msg(Response),
     send_msg(priv_msg, Response, Chan)
  ).


%--------------------------------------------------------------------------------%
% Knowledge Base
%--------------------------------------------------------------------------------%


ext_help(Query, Msg) :-
  (
     Query = "google",
     google(Msg)
  ;
     Query = "",
     help_msg(Msg)
  ;
     Query = "wiki",
     wiki(Msg)
  ;
     Query = "dict",
     dict(Msg)
  ;
     Query = "emoticons",
     emoticons(Msg)
  ;
     Query = "search",
     search(Msg)
  ;
     Query = "yesbot",
     yesbot(Msg)
  ;
     Query = "keywords",
     keywords(Msg)
  ).


google("?google <search term> will perform an ""I'm feeling lucky search""").
wiki("?wiki <search term> will attempt a wikipedia lookup").
dict("?dict <search term> will attempt to look up a word in dictionary.com").
emoticons("?emote <emoticon> will attempt to define wacky emoticons").
search("?search <predicate/N> OR <f(function/N)> to search for some predicate \c
  or function with N arity in the swi object database/documentation (use \c
  ?search -q1 for quiet output OR ?search -q2 for quieter)").
help_msg("?help <option> where your options are keywords, search, google, \c
  wiki, dict, emoticons, yesbot").
yesbot("Do I have a bug? Want to request a feature? Want to hack me? You can \c
  pursue all this at: https://github.com/eazar001/yesbot/tree/prolog-irc").
keywords("?<keyword> where currently available keywords are: faq, modules, \c
  tutorials, dcg, webapps, clpfd, swipl-devel, lpn, amzi, yesbot").

