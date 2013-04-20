% Max Nordlund
% A Matrix is a List of Lists

:- module(prolude,   [
      map/3,   % :Goal, +InputList, -OutputList
      filter/3,  % :Goal, +InputList, -OutputList
      any/2,   % :Goal, +List
      all/2,   % :Goal, +List
      not/2,   % :Goal, ?Input
      head/2,  % +List, -Head
      last/2,  % +List, -Last
      tail/2,  % +InputList, -OutputList
      init/2,  % +InputList, -OutputList
      null/1,  % ?List
      in/2,    % ?List, ?Element
      foldl/3,   % :Goal, +List, -Result
      foldr/3,   % :Goal, +List, -Result
      concat/2,  % +Matrix, -List
      reverse/2, % +InputList, -OutputList
      trans/2,   % +InputMatrix, -OutputMatrix
      splitBy/3, % +Delimiter, +String, -ListOfStrings
      add/3,   % +A, +B, -Result
      sub/3,   % +A, +B, -Result
      mul/3,   % +A, +B, -Result
      div/3,   % +A, +B, -Result
      lt/2,    % +A, +B
      le/2,    % +A, +B
      gt/2,    % +A, +B
      ge/2,    % +A, +B
      eq/2,    % +A, +B
      ne/2     % +A, +B
     ]).
:- meta_predicate map(2, ?, ?).
:- meta_predicate filter(1, ?, ?).
:- meta_predicate any(1, +).
:- meta_predicate all(1, +).
:- meta_predicate not(1, ?).
:- meta_predicate foldl(3, +, -).
:- meta_predicate foldr(3, +, -).

map(G, L, R) :-
  _map(G, L, [], R).
_map(_, [], R, R).
_map(G, [H|T], B, R) :-
  call(G, H, A),
  _map(G, T, [A|B], R).

filter(G, L, R) :-
  _filter(G, L, [], R).
_filter(_, [], R, R).
_filter(G, [H|T], B, R) :-
  (call(G, H) ->
    _filter(G, T, [H|B], R)
  ;
    _filter(G, T, B, R)
  ).

any(_, []).
any(G, [H|T]) :-
  call(G, H);
  any(T).

all(_, []).
all(G, [H|T]) :-
  call(G, H),
  all(G, T).

not(G, I) :-
  \+ call(G, I).

head([H|_], H).

last([R|[]], R).
last([_|T], R) :-
  last(T, R).

tail([_|T], T).

init(L, R) :-
  _init(L, [], R).
_init([], [_|L], R) :-
  reverse(L, R).
_init([H|T], L, R) :-
  _init(T, [H|L], R).

null([]).

in(L, E) :-
  member(E, L).

foldl(G, L, R) :-
  _foldl(G, L, _, R).
_foldl(_, [], R, R).
_foldl(G, [H|T], A, R) :-
  call(G, A, H, B),
  _foldl(G, T, B, R).

foldr(G, L, R) :-
  _foldr(G, L, _, R).
_foldr(_, [], R, R).
_foldr(G, [H|T], A, R) :-
  _foldr(G, T, A, B),
  call(G, B, H, R).

colon(A, B, [B|A]).

prepend([], R, R).
prepend([H|T], L, R) :-
  prepend(T, [H|L], R).

reverse(L, R) :-
  prepend(L, [], R).
  %% _reverse(L, [], R).
%% _reverse([], R, R).
%% _reverse([H|T], L, R) :-
%%   _reverse(T, [H|L], R).

concat(L, R) :-
  _concat(L, [], A),
  reverse(A, R).
_concat([], R, R).
_concat([H|T], A, R) :-
  prepend(H, A, B),
  _concat(T, B, R).

transpose(M, R) :-
  map()

transpose(L, R) :-
  _transpose(L, [], [], [], A),
  map(reverse, A, R).
_transpose([], [], Bc, Bm, [Bc|Bm]).
_transpose([], Rest, Bc, Bm, R) :-
  _transpose(Rest, [], [], [Bc|Bm], R).
_transpose([[H|[]]|T], Rest, Bc, Bm, R) :-
  _transpose(T, [], [H|Bc], Bm, R).
_transpose([[H|Tr]|T], Rest, Bc, Bm, R) :-
  append(Rest, [Tr], A)
  _transpose(T, A, [H|Bc], Bm, R).

splitBy(Sp, Str, R) :-
  _splitBy(Sp, Str, [], [], A),
  filter(not(null), A, B),
  map(reverse, L, A),
  reverse(A, R).
_splitBy(_, [], Bs, B, [Bs|B]).
_splitBy(Sp, [H|T], Bsi, Bi, R) :-
  (member(H, Sp) ->
    (Bso = [],
     Bo  = [Bsi|Bi])
  ;
    (Bso = [H|Bsi],
     Bo  = Bi)
  ),
  _splitBy(Sp, T, Bso, Bo, R).

lt(A, B) :-
  A < B.

le(A, B) :-
  A =< B.

gt(A, B) :-
  A > B.

ge(A, B) :-
  A >= B.

eq(A, B) :-
  A == B.

ne(A, B) :-
  A \== B.

add(A, B, C) :-
  C is A + B.

sub(A, B, C) :-
  C is A - B.

mul(A, B, C) :-
  C is A * B.

div(A, B, C) :-
  C is A / B.
