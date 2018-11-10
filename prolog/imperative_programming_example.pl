:- initialization(main).
:- set_prolog_flag(double_quotes,chars).

main :-
	imperative(Result,[
		r = 1,
		the_list = [3,4,5],
		sort(the_list),
		z = a_function(2+r,3),
		a1 = member(3,[3,4]),
		a2 = (true;true),
		a = 1,
		d = sin(0.1)**2 + cos(0.1)**2,
		f = \+(a = d),
		b = 2,
		z = b + 1,
		while(a =< 10,[
			appended_list = [1+1,2,3]+[1,2,3],
			writeln(appended_list),
			bool_var = (3>4),
			the_list = sorted([3,5,32,1,98,8,7]),
			list2 = [4,2,6],
			sort(list2),
			str1 = "Hello",
			str2 = "world",
			str3 = str1 + " " + str2,
			a = a + 1,
			(a mod 3 = 0) ->
				[writeln(a mod 2),
				writeln(a)],
			(a mod 5 = 0) ->
				[return = 5]
		])
	]),
	writeln(Result),!.

imperative_test(Input,Output) :-
	Input = [x:1],
	imperative(Input,Output,[
		'x' = 'x' + 1
	]),!.

while(List,Result,Condition,Statements) :-
	(member(return:_,List),List = Result);
	get_var(List,Condition_,Condition),
	(Condition_ -> (imperative(List,Result_,Statements),while(Result_,Result,Condition,Statements));
	List=Result),!.

%in-place sort
imperative_(List,Result,sort(A)) :-
	imperative(List,Result,[
		A = sorted(A)
	]),!.

imperative_(List,Result,Command) :-
	(member(A:_,List),
	replaceP(A:A1,A:A2,List,Result);Result=[A:A2|List]),
	(Command = increment(A,B), A2 is A1 + B;
	Command = increment(A), A2 is A1 + 1;
	Command = decrement(A), A2 is A1 - 1;
	Command = (A = B), get_var(List,A2,B)),!.

imperative_(List,Result,while(Condition,Statements)) :-
	while(List,Result,Condition,Statements),!.

imperative_(List,List,writeln(A)) :-
	get_var(List,A_,A),!,
	writeln(A_),!.

imperative_(List,Result,(Condition -> Statements)) :-
	get_var(List,Condition_,Condition),
	(Condition_ -> imperative(List,Result,Statements);List=Result),!.

imperative(Result,Steps) :- imperative([],Result1,Steps),memberchk(return:Result,Result1),!.

imperative(List,Result,[]) :-
	List = Result,!.
	
imperative(List,Result,[Step|Rest]) :-
	(member(return:_,List),List = Result);
	imperative_(List,List1,Step),!,imperative(List1,Result,Rest),!.

get_var(List,A2,B) :- phrase(get_var(List,B),[A2]),!.

get_var(List,get_var(A)) -->
	{get_var(List,A_,A)},[A_],!.

get_var(List,str(A)) -->
	{get_var(List,A_,A)},[A_],!.

get_var(List,sorted(A)) -->
	{get_var(List,A_,A),sort(A_,A1)},[A1],!.

get_var(List,B) -->
	{member(B:A2,List)},[A2],!.

get_var(_,B) -->
	{number(B)},[B],!.

get_var(_,[]) -->
	[[]],!.
get_var(List,[A]) -->
	{get_var(List,A_,A)},[[A_]],!.
get_var(List,[A|B]) -->
	{get_var(List,A_,A),get_var(List,B_,B)},[[A_|B_]],!.
get_var(_,B) -->
	{is_list(B)},[B],!.

get_var(List,log(A)) -->
	{get_var(List,A_,A), number(A_), A2 is log(A_)},[A2],!.

get_var(List,sin(A)) -->
	{get_var(List,A_,A), number(A_), A2 is sin(A_)},[A2],!.

get_var(List,cos(A)) -->
	{get_var(List,A_,A), number(A_), A2 is cos(A_)},[A2],!.

get_var(List,tan(A)) -->
	{get_var(List,A_,A), number(A_), A2 is tan(A_)},[A2],!.

get_var(List,asin(A)) -->
	{get_var(List,A_,A), number(A_), A2 is asin(A_)},[A2],!.

get_var(List,acos(A)) -->
	{get_var(List,A_,A), number(A_), A2 is acos(A_)},[A2],!.

get_var(List,atan(A)) -->
	{get_var(List,A_,A), number(A_), A2 is atan(A_)},[A2],!.

get_var(List,(A+B)) -->
	{get_var(List,A_,A), get_var(List,B_,B), number(A_),number(B_), A2 is A_ + B_},[A2],!.

get_var(List,(A+B)) -->
	{get_var(List,A_,A), get_var(List,B_,B), is_list(A_),is_list(B_), append(A_,B_,A2)},[A2],!.

get_var(List,(A-B)) -->
	{get_var(List,A_,A), get_var(List,B_,B), A2 is A_ - B_},[A2],!.
	
get_var(List,(A/B)) -->
	{get_var(List,A_,A), get_var(List,B_,B), A2 is A_ / B_},[A2],!.

get_var(List,(A \== B)) -->
	get_var(List,(A \= B)),!.

get_var(List,(A \= B)) -->
	{get_var(List,A_,A), get_var(List,B_,B),eval_bool((A_ \= B_),Result)},[Result],!.

get_var(List,(A*B)) -->
	{get_var(List,A_,A), get_var(List,B_,B), A2 is A_ * B_},[A2],!.

get_var(List,(A**B)) -->
	{get_var(List,A_,A), get_var(List,B_,B), A2 is A_ ** B_},[A2],!.

get_var(List,(A mod B)) -->
	{get_var(List,A_,A), get_var(List,B_,B), A2 is A_ mod B_},[A2],!.

get_var(List,(A >= B)) -->
	{get_var(List,A_,A), get_var(List,B_,B),eval_bool((A_ >= B_),Result)},[Result],!.

get_var(List,(A =< B)) -->
	{get_var(List,A_,A), get_var(List,B_,B),eval_bool((A_ =< B_),Result)},[Result],!.

get_var(List,(A>B)) -->
	{get_var(List,A_,A), get_var(List,B_,B),eval_bool((A_ > B_),Result)},[Result],!.

get_var(List,(A<B)) -->
	{get_var(List,A_,A), get_var(List,B_,B),eval_bool((A_ < B_),Result)},[Result],!.

get_var(List,(A,B)) -->
	{get_var(List,A_,A), get_var(List,B_,B),eval_bool((A_,B_),Result)},[Result],!.

get_var(List,(A==B)) -->
	get_var(List,(A=B)),!.

get_var(List,(A=B)) -->
	{get_var(List,A_,A), get_var(List,B_,B),eval_bool((A_=B_),Result)},[Result],!.

get_var(List,member(A,B)) -->
	{get_var(List,A_,A), get_var(List,B_,B), eval_bool(member(A_,B_),Result)},[Result],!.

get_var(List,imperative_factorial(Input,Output)) -->
	{imperative(Output,[
		i = 1,
		result = 1,
		while((i =< Input),[
			result = result * i,
			i = i + 1
		]),
		return = result
	])},[Output].


get_var(List,pow(A,B)) -->
	get_var(List,(A**B)),!.

get_var(List,(A^B)) -->
	get_var(List,(A**B)),!.
	
get_var(List,\+(A)) -->
	get_var(List,not(A)),!.

get_var(List,not(A)) -->
	{get_var(List,A_,A),eval_bool(not(A_),Result)},[Result],!.
	
get_var(List,(A;B)) -->
	{get_var(List,A_,A), get_var(List,B_,B),eval_bool((A_; B_),Result)},[Result],!.

get_var(_,true) -->
	[true],!.
get_var(_,false) -->
	[false],!.

get_var(List,A) -->
	{callable(A),A =.. [Name|Rest],dif(Rest,[]),get_var(List,Rest_,Rest),B =.. [Name|[Rest_,Output]],call(B)},[Output].

a_function([A,B],Output) :-
	imperative(Output,[
		z = 1,
		return = A+B+z
	]).

eval_bool(A,B) :-
	A -> B = true;B=false,!.

% code by @svick, modified to use dif/2 instead of (\=)/2
replaceP(_, _, [], []).
replaceP(O, R, [O|T], [R|T2]) :- replaceP(O, R, T, T2),!.
replaceP(O, R, [H|T], [H|T2]) :- dif(H,O), replaceP(O, R, T, T2),!.
