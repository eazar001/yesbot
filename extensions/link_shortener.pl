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
:- use_module(utilities).
:- use_module(submodules/html).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).


%--------------------------------------------------------------------------------%
% Main Extension
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
  thread_create(ignore(link_shortener_(Msg)), _Id, [detached(true)]).


link_shortener_(Msg) :-
  Msg = msg(_Prefix, "PRIVMSG", [Chan], M),
  core:connection(_Nick, _Pass, Chans, _Hostname, _Servername, _Realname),
  member(Chan, Chans), !,
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
	    unescape_title(Title, T),
            send_msg(priv_msg, T, Chan)
       ),
       send_msg(priv_msg, Tiny, Chan)
     ;
       url_get_line(Link, Title) ->
       (
          Title = [] ->
	    true
          ;
	    unescape_title(Title, T),
	    send_msg(priv_msg, T, Chan)
       )
  ).


%% unescape_title(+Title, -T) is det.
%
% If the Title contains escape characters then format the title for appropriate
% viewing by unescaping them. If there are no escape characters, then the
% formatted title is simply the same as the original.

unescape_title(Title, T) :-
  html_unescape(Title, T) ->
    true
  ;
    T = Title.
  

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
      [ header('Content-Type', Type)
       ,cert_verify_hook(cert_verify)
       ,timeout(20) ]),
    (
       (atom_concat('text/html', _, Type) ; Type = '') ->
         repeat,
         read_line_to_codes(Stream, Line),
         (
            Line = end_of_file ->
	      url_get_line_retry(Link, Title)
	    ;
	      get_title(Line, Title), !
         )
       ;
         Title = []
    ),
    close(Stream)
  ).


url_get_line_retry(Link, Title) :-
  url_get_all_lines(Link, Codes),
  get_title(Codes, Title).


url_get_all_lines(Link, Codes) :-
  setup_call_cleanup(
    http_open(Link, Stream,
      [ header('Content-Type', Type)
       ,cert_verify_hook(cert_verify)
       ,timeout(20) ]),
    (
       (atom_concat('text/html', _, Type) ; Type = ''),
       read_stream_to_codes(Stream, C),
       maplist(call(link_shortener:change), C, Codes)
    ),
    close(Stream)
  ).


change(10, 32).
change(X, X) :- X \= 10.



cert_verify(_SSL, _ProblemCert, _AllCerts, _FirstCert, _Error) :-
  format(user_error, 'Accepting certificate~n', []).


%% visit_url(+Link, -Reply) is semidet.
%
% Visit link and extract the reply.

visit_url(Link, Reply) :-
  setup_call_catcher_cleanup(
    http_open(Link, Stream, [timeout(20)]),
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


