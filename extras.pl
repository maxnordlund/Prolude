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
  _chain(M, L, I, R).
_chain(_, [], R, R).
_chain(M, [H|T], I, R) :-
	call(M:H, I, B),
	_chain(M, T, B, R).

chain(M:L, I) :-
  _chain(M, L, I).
_chain(M, [H|[]], I) :-
	call(M:H, I).
_chain(M, [H|T], I) :-
	call(M:H, I, R),
	_chain(M, T, R).

echo([]) :- nl.
echo([H|T]) :-
	format(H, []), nl,
	echo(T).
