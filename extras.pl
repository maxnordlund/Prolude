% Max Nordlund

:- module(extras,  [
          chain/2, % :ListOfGoals, +StartValue
          chain/3, % :ListOfGoals, +StartValue, -Result
          echo/1   % +String
         ]).
:- meta_predicate chain(:, +).
:- meta_predicate chain(:, +, -).
:- ensure_loaded(prolude).

chain(M:L, I, R) :-
    chain_(M, L, I, R).
chain_(_, [], R, R).
chain_(M, [H|T], I, R) :-
	call(M:H, I, B),
	chain_(M, T, B, R).

chain(M:L, I) :-
    chain_(M, L, I).
chain_(M, [H|[]], I) :-
	call(M:H, I).
chain_(M, [H|T], I) :-
	call(M:H, I, R),
	chain_(M, T, R).

echo([]) :- nl.
echo([H|T]) :-
	format(H, []), nl,
	echo(T).