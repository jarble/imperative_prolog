:- initialization(main).
:- set_prolog_flag('double_quotes','chars').

main :-
	set_var((
		A = increment_var(1)*2
	)),
	writeln(A).

increment_var(A,B) :-
	set_var((
		B=A,
		while(B<50,(
			B=B*2
		))
	)).

print(A,true) :-
	writeln(A).
	
imperative_statements(A) :-
	term_variables(A,Names),
	length(Names,L),
	length(Input,L),
	length(Output,L),
	set_var(A,Names,Input,Output),
	Names=Output.

set_var(A,Names,Input) :-
	length(Input,L),
	length(Output,L),
	set_var(A,Names,Input,Output),
	Names=Output.

set_var((A,B),Var_names,Input,Output) :-
		set_var(A,Var_names,Input,Output1),
		set_var(B,Var_names,Output1,Output).

set_var(Name=Value,Var_names,Input,Output) :-
	get_var(Value,Var_names,Input,Value1),
	set_var_(Name=Value1,Var_names,Input,Output).

set_var_(_,[],[],[]).
set_var_(Name=Value1,[Name1|Name2],[Var1|Var2],[Output1|Output2]) :-
	(Name==Name1,Output1=Value1;
	Name \== Name1,Output1=Var1),
	set_var_(Name=Value1,Name2,Var2,Output2).
	
set_var(while(A,B),Names,Vars,Result) :-
    get_var(A,Names,Vars,A1),
    (((A1==true)->(set_var(B,Names,Vars,Result1),set_var(while(A,B),Names,Result1,Result)));
    A1==false,Vars=Result).

get_var(Name,[Name1],[Var1],Output) :-
	var(Name),Name==Name1,
	Output=Var1.
get_var(Name,[Name1|Name2],[Var1|Var2],Output) :-
	var(Name),(Name==Name1,get_var(Name,[Name1],[Var1],Output);
	Name \== Name1,get_var(Name,Name2,Var2,Output)).

get_var([],_,_,[]).
get_var([Name1|Name2],Names,Vars,[Name3|Name4]) :-
	get_var(Name1,Names,Vars,Name3),
	get_var(Name2,Names,Vars,Name4).

get_var(Name,_,_,Name) :-
	number(Name);atom(Name).
get_var(A+B,Names,Vars,Output) :-
	get_var(A,Names,Vars,A1),
	get_var(B,Names,Vars,B1),
	Output is A1+B1.
get_var(A*B,Names,Vars,Output) :-
	get_var(A,Names,Vars,A1),
	get_var(B,Names,Vars,B1),
	Output is A1*B1.
get_var(A/B,Names,Vars,Output) :-
	get_var(A,Names,Vars,A1),
	get_var(B,Names,Vars,B1),
	Output is A1/B1.
get_var(A**B,Names,Vars,Output) :-
	get_var(A,Names,Vars,A1),
	get_var(B,Names,Vars,B1),
	Output is A1**B1.

get_var((A,B),Names,Vars,Result) :-
    get_var([A,B],Names,Vars,[A1,B1]),
    (A1,B1,Result=true;([A1,B1]=[true,false];[A1,B1]=[false,true]),Result=false).

get_var((A;B),Names,Vars,Result) :-
    (get_var(A,Names,Vars,A1),call(A1);
    get_var(B,Names,Vars,B1),call(B1)) -> (Result = true);
    (get_var(A,Names,Vars,A1),A1=false,Result=false).

get_var(A<B,Names,Vars,Result) :-
	get_var(B>A,Names,Vars,Result).
get_var(A>B,Names,Vars,Result) :-
    %comparison of variables
    get_var([A,B],Names,Vars,[A1,B1]),
    (A1>B1,Result=true;A1<B1,Result=false).
    
get_var(A==B,Names,Vars,Result) :-
    %comparison of variables
    get_var([A,B],Names,Vars,[A1,B1]),
    (A1==B1,Result=true;A1\=B1,Result=false).

get_var(Input,Names,Vars,Output1) :-
    (\+number(Input)),
    Input =.. [Name|Params],
    \+member(Name,['=',==,'->',not,'[|]',',',';',+,-,*,/,**,^,writeln]),
    length(Params,Params_length),
    Params_length > 0,
    get_var(Params,Names,Vars,Params1),
    append([Name|Params1],[Output1],Input0),
    Input1 =.. Input0,
    call(Input1).
