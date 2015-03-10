
%% Parsing modules for SWI docs


:- module(docs, [sentence/3]).


sentence(Diff) -->
  fragment(R),
  sentence(Rest),
  { append(R, Rest, Diff) }, !.

sentence(Rest) -->
  fragment(R),
  ending(E),
  { append(R, E, Rest) }, !.

sentence(Rest) -->
  fragment(R),
  newline(E),
  { append(R, E, Rest) }, !.


fragment(Rest) --> abbreviation(Rest).

fragment(Rest) --> decimal(Rest).

fragment([C|Rest]) -->
  [C,A],
  {
     ending(_, [C], []),
     (
        punctuation(Rest, [A], [])
     ;
        ending(Rest, [A], [])
     )
  }.

fragment([C|_]) -->
  [C], { \+ending(_, [C], []) }.


abbreviation([101,46,103,46|_]) --> `e.g.`.
abbreviation([105,46,101,46|_]) --> `i.e.`.

punctuation([47|_]) --> `/`.
punctuation([41|_]) --> `)`.
punctuation([40|_]) --> `(`.

decimal([D0,46,D1|_]) -->
  [D0,46,D1],
  {
     (  between(48,57,D0)
     -> between(48,57,D1)
     ;  fail
     )
  }.
  

ending([46]) --> `.`.
ending(``) --> ``.
newline([10]) --> `\n`.


