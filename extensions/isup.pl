
:- module(isup, [isup/1]).

:- use_module(library(http/http_json)).
:- use_module(library(http/http_open)).
:- use_module(dispatch).
:- use_module(submodules/web).
:- use_module(submodules/html).


chan("##prolog").
api(`http://isitup.org/`).

isup(Msg) :-
  thread_create(ignore(isup_(Msg)), _Id, [detached(true)]).


isup_(Msg) :-
  chan(Chan),
  Msg = msg(_Prefix, "PRIVMSG", [Chan], Rest),
  append(`?isup `, Q, Rest),
  atom_codes(Query, Q),
  normalize_space(codes(Qcodes), Query),
  api(Fst),
  append(Qcodes, Diff, Snd),
  append(Fst, Snd, Thrd),
  Diff = `.json`,
  string_codes(Site, Thrd),
  setup_call_cleanup(
    http_open(Site, Stream, []),		     
    json:json_read(Stream, json(List)),
    close(Stream)    
  ),
  memberchk(response_code=Resp, List),
  memberchk(status_code=Status, List),
  memberchk(response_time=Time, List),
  decode(Status, Resp, Time).


decode(Status, Resp, Time) :-
  chan(Chan),
  (
     Status = 1,
     send_msg(priv_msg, "Website is alive.", Chan)
  ;
     Status = 2,
     send_msg(priv_msg, "Website appears down.", Chan)
  ;
     Status = 3,
     send_msg(priv_msg, "Domain was not valid.", Chan)
  ),
  format(string(Report), 'Response code: ~a, Response time: ~a s', [Resp, Time]),
  send_msg(priv_msg, Report, Chan).


