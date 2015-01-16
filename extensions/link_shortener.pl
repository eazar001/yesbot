%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                                %
%                                                                                %
% Author: Ebrahim Azarisooreh                                                    %
% E-mail: ebrahim.azarisooreh@gmail.com                                          %
% IRC Nick: eazar001                                                             %
% Title: Yes-Bot Link Shortener Extension                                        %
% Description: Tinyurl API Link Shortener for the Yes-Bot IRC Bot                %
%                                                                                %
%                                                                                %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


:- module(link_shortener, [link_shortener/1]).

:- use_module(dispatch).
:- use_module(parser).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).


%--------------------------------------------------------------------------------%
% Link Shortening
%--------------------------------------------------------------------------------%

% tiny-url api
tiny_form("http://tinyurl.com/api-create.php?url=").

%% make_tiny(+Link, -Title, -Tiny) is semidet.
%
% Attempt to extract the title of the link. If a title is extracted, then send the
% link to tinyurl to short the link if the link is determined to be valid and
% greater than or equal to 100 characters in length.

make_tiny(Link, Title, Tiny) :-
  url_get_line(Link, Title),
  tiny_form(F),
  string_concat(F, Link, Full),
  visit_url(Full, Tiny).


%% NOTE : Should really handle headers that don't report content-type properly

url_get_line(Link, Title) :-
  setup_call_cleanup(
    http_open(Link, Stream,
      [header('Content-Type', Type), cert_verify_hook(cert_verify)]),
    (
       (atom_concat('text/html', _, Type) ; Type = '') ->
         repeat,
         read_line_to_codes(Stream, Line),
         (
            Line = end_of_file ->
	      Title = []
	    ;
              get_title(Title, Line, _), !
         )
       ;
         Title = []
    ),
    close(Stream)
  ).


cert_verify(_SSL, _ProblemCert, _AllCerts, _FirstCert, _Error) :-
  format(user_error, 'Accepting certificate~n', []).


%% visit_url(+Link, -Reply) is semidet.
%
% Visit link and extract the reply.

visit_url(Link, Reply) :-
  setup_call_catcher_cleanup(
    http_open(Link, Stream, []),
    read_stream_to_codes(Stream, Reply),
    E = no_error,
    close(Stream)),
  (
     % no problems here, succeed here
     E = no_error, !
  ;
     % could not connect, fail here
     E = error(socket_error('Host not found'), _), !, fail
  ;
     % can connect, but insufficient reply, so succeed
     Reply = none
  ).


%--------------------------------------------------------------------------------%
% IRC/Private Mesages
%--------------------------------------------------------------------------------%


has_link(http, [104,116,116,112,58,47,47|L]) -->
  `http://`, get_link(http, L), !.

has_link(https, [104,116,116,112,115,58,47,47|L]) -->
  `https://`, get_link(https, L), !.
  
has_link(Protocol, L) -->
  [_], has_link(Protocol, L).

get_link(_, []) --> [32|_], !.

get_link(Protocol, [C|L]) -->
  [C], get_link(Protocol, L).

get_link(_, []) --> [].


%--------------------------------------------------------------------------------%
% html
%--------------------------------------------------------------------------------%


%% opening and closing title tag variations

title_open(`<title>`).
title_open(`<TITLE>`).
title_open(`<Title>`).

title_close(`</title>`).
title_close(`</TITLE>`).
title_close(`</Title>`).


%% get_title(-D, +S0, -S) is semidet.
%
% Any sequence that contains the opening 'title' tag is consumed. The following
% characters are parsed as the title until the closing tag is reached. The
% characters are stored as a list as they are parsed.

get_title(T) -->
  {title_open(Tag)},
  Tag, title(T), !.

get_title(T) -->
  [_], get_title(T).

%% title(-D, +S0, -S) is semidet.
%
% Consume any sequence containing the closing title tag and succeed by closing
% the list of stored contents that reside in the tag.

title([]) -->
  {title_close(Tag)},
  Tag, !.

title([C|T]) -->
  [C], title(T).


%--------------------------------------------------------------------------------%
% Extension
%--------------------------------------------------------------------------------%


%% link_shortener(+Msg) is semidet.
%
% This link shortener extension will first parse a message to determine whether
% or not this message contains a link. If a link exists, it will determine to
% whether it is http or https. It will accordingly attempt to open the URL and
% extract a title from the from the link. If the link is 100 or more characters
% in length, then the link will be sent to tinyurl for shortening. The shortened
% URL will be sent to the channel publicly for general consumption.
%
% This predicate will only succeed if Recip is identical to Chan

link_shortener(Msg) :-
  Msg = msg(_Prefix, "PRIVMSG", [Chan], M),
  core:connection(_Nick, _Pass, Ch, _Hostname, _Servername, _Realname),
  atom_string(Ch, Chan),
  has_link(_, L, M, _),
  atom_codes(Link, L),
  length(L, N),
  (
     % shorten link if 100 or more characters in length
     N >= 100 ->
       make_tiny(Link, Title, Tiny),
       (
          Title = [] ->
            true
          ;
            send_msg(priv_msg, Title, Chan)
       ),
       send_msg(priv_msg, Tiny, Chan)
     ;
       url_get_line(Link, Title) ->
       (
          Title = [] ->
	    true
          ;
	    send_msg(priv_msg, Title, Chan)
       )
  ),
  core:get_irc_stream(Stream),
  flush_output(Stream).


