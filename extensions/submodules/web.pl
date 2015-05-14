
%% Submodule for General Web/Connection related utilities


:- module(web,
     [ visit_url/2
      ,url_get_title/2
      ,content_type_opts/2
      ,cert_verify/5 ]).

:- use_module(html).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_ssl_plugin)).
:- use_module(library(xpath)).


%% url_get_title(+Link, -Title) is semidet.
%
% Open a link with the correction options and extract the title from the web page.
% If the content is not determined text/html or doesn't contain a title, then
% the title will be returned as a null value.

url_get_title(Link, Title) :-
  setup_call_cleanup(
    http_open(Link, Stream,
      [ header('Content-Type', Type)
       ,cert_verify_hook(cert_verify)
       ,timeout(20) ]),
    (
       content_type_opts(Type, Opts)
    ->
       load_html(Stream, Structure, Opts),
       xpath_chk(Structure, //title, Tstruct),
       Tstruct = element(title, _, [T0]),
       string_codes(T0, T),
       clean_sequence(T, Title)
    ;
       Title = []
    ),
    close(Stream)
  ).


%% visit_url(+Link, -Reply) is semidet.
%
% Visit link and extract the reply. If the request succeeds, there there is no
% error caught, and the Reply is unified. If there is a socket error then the
% predicate will fail. If there is an insufficent reply, but a proper connection
% was established then, the Reply will be unified with none.

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

%% content_type_opts(+Type, -Opts) is semidet.
%
% Take a Type argument and unifies a corresponding option list for appropriate
% http handling. This handles Types that are text/html, or are even possibly
% text/html. The option lists are designed to self-evidently determine validity
% by exception.

content_type_opts(Type, Opts) :-
  (
     atom_concat('text/html', _, Type),
     Opts = [max_errors(-1)]
  ;
     Type = '',
     Opts = [max_errors(50)]
  ).


% Certificate verification
cert_verify(_SSL, _ProblemCert, _AllCerts, _FirstCert, _Error) :-
  format(user_error, 'Accepting certificate~n', []).




