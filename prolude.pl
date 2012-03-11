% Max Nordlund
% A Matrix is a List of Lists

:- module(prolude,   [
          map/3,     % :Goal, +InputList, -OutputList
          filter/3,  % :Goal, +InputList, -OutputList
          any/2,     % :Goal, +List
          all/2,     % :Goal, +List
          not/2,     % :Goal, ?Input
          head/2,    % +List, -Head
          last/2,    % +List, -Last
          tail/2,    % +InputList, -OutputList
          init/2,    % +InputList, -OutputList
          null/1,    % ?List
          in/2,      % ?List, ?Element
          foldl/3,   % :Goal, +List, -Result
          foldr/3,   % :Goal, +List, -Result
          concat/2,  % +Matrix, -List
          reverse/2, % +InputList, -OutputList
          trans/2,   % +InputMatrix, -OutputMatrix
          splitBy/3, % +Delimiter, +String, -ListOfStrings
          add/3,     % +A, +B, -Result
          sub/3,     % +A, +B, -Result
          mul/3,     % +A, +B, -Result
          div/3,     % +A, +B, -Result
          lt/2,      % +A, +B
          le/2,      % +A, +B
          gt/2,      % +A, +B
          ge/2,      % +A, +B
          eq/2,      % +A, +B
          ne/2       % +A, +B
         ]).
:- meta_predicate map(2, ?, ?).
:- meta_predicate filter(1, ?, ?).
:- meta_predicate any(1, +).
:- meta_predicate all(1, +).
:- meta_predicate not(1, ?).
:- meta_predicate foldl(3, +, -).
:- meta_predicate foldr(3, +, -).

map(G, L, R) :-
    map_(G, L, [], R).
map_(_, [], R, R).
map_(G, [H|T], B, R) :-
    call(G, H, A),
    map(G, T, [A|B], R).

filter(G, L, R) :-
    filter_(G, L, [], R).
filter_(_, [], R, R).
filter_(G, [H|T], B, R) :-
    (call(G, H) ->
        filter_(G, T, [H|B], R)
    ;
        filter_(G, T, B, R)
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
    init_(L, [], R).
init_([], [_|L], R) :-
    reverse(L, R).
init_([H|T], L, R) :-
    init_(T, [H|L], R).

null([]).

in(L, E) :-
    member(E, L).

foldl(G, L, R) :-
    foldl_(G, L, _, R).
foldl_(_, [], R, R).
foldl_(G, [H|T], A, R) :-
    call(G, A, H, B),
    foldl_(G, T, B, R).

foldr(G, L, R) :-
    foldr_(G, L, _, R).
foldr_(_, [], R, R).
foldr_(G, [H|T], A, R) :-
    foldr_(G, T, A, B),
    call(G, B, H, R).

concat([], []).
concat([A|T], R) :-
    concat(T, B),
    append(A, B, R).

colon(A, B, [B|A]).

reverse(L, R) :-
    foldl(colon, L, [], R).

reverse2d(L, R) :-
	map(reverse, L, A),
	reverse(A, R).

trans(L, R) :-
    trans_(L, [], [], [], A),
    map(reverse, A, R).
trans_([], [], Bc, Bm, [Bc|Bm]).
trans_([], Rest, Bc, Bm, R) :-
    trans_(Rest, [], [], [Bc|Bm], R).
trans_([[H|Tr]|T], Rest, Bc, Bm, R) :-
    (Tr == [] ->
        A = []
    ;
        append(Rest, [Tr], A)
    ),
    trans_(T, A, [H|Bc], Bm, R).

splitBy(Sp, Str, R) :-
    splitBy_(Sp, Str, [], [], A),
    filter(not(null), A, B),
    reverse2d(B, R).
splitBy_(_, [], Bs, B, [Bs|B]).
splitBy_(Sp, [H|T], Bsi, Bi, R) :-
    (member(H, Sp) ->
        (Bso = [],
         Bo  = [Bsi|Bi])
    ;
        (Bso = [H|Bsi],
         Bo  = Bi)
    ),
    splitBy_(Sp, T, Bso, Bo, R).

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
