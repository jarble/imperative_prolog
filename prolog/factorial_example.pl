:- use_module(imperative_interpreter).
:- initialization(main).
:- set_prolog_flag('double_quotes','chars').

main :-
	imperative_statements((
		Z = factorial(3),
		Z = Z*factorial(Z),
		_ = print(Z),
		M = [1,2,3],
		M = append(M,M),
		_ = print(M)
	)).

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
