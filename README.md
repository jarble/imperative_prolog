# imperative_prolog
An interpreter for imperative algorithms in Prolog

This project includes two interpreters for imperative functions in Prolog.

```
:- use_module(imperative_interpreter).
:- initialization(main).
:- set_prolog_flag('double_quotes','chars').

main :-
	imperative_statements((
		Z = factorial(3),
		print(Z*factorial(Z)),
		M = [1,2,3],
		print(append(M,M) == [1,2,3,1,2,3]),
		print(sin(6))
	)),
	writeln(Z),
	writeln(M).

factorial(A,A1) :-
	imperative_statements((
		A1=1,
		B=0,
		while(B<A,(
			B=B+1,
			A1=A1*B
		))
	)).

print(A,true) :-
	writeln(A).
```
