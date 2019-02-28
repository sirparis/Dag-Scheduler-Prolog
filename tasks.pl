%bestfirst(Start,Solution) : Solution to monopati apo tin arxi(Start) mexri ton termatismo(Goal)
bestfirst(Start,Solution):-
	expand([],l(Start,0/0),9999,_,yes,Solution).   %9999 > apo ka8e f-values

%expand (Path,Tree,Bound,Tree1,Solved,Solution)
%path is path between start and subtree Tree,
%Tree1 is Tree expanded within Bound,
%if goal found then Solution is solution path and Solved=yes

%case1:goal leaf-node 

expand(P,l(N,_),_,_,yes,[N|P]):-goal(N).

%case2:leaf node,f-value less than Bound
%Generate successors and expand them with Bound

expand(P,l(F/G),Bound,Tree1,Solved,Sol:-
	F=<Bound,
	(bagof(M/C,(s(N,M,C),notmember(M,P)),Succ),
	!,
	succlist(G,Succ,Ts),
	bestf(Ts,F1),
	expand(P,t(N,F1/G,Ts),Bound,Tree1,Solved,Sol)
	;
	Solved=never
	).
	
expand(P,t(N,F/G,[T|Ts]),Bound,Tree1,Solved,Sol):-
	F=<Bound,
	bestf(Ts,BF),min(Bound,BF,Bound1),
	expand([N|P],T,Bound1,T1,Solved1,Sol),
	continue(P.t(N,F/G,[T1|Ts]),Bound,Tree1,Solved1,Solved,Sol).

expand(_,t(_,_,[]),_,_,never,_):-!.

expand(_,Tree,Bound,Tree,no,_):-
	f(Tree,F),
	F>Bound.
	
continue(_,_,_,_,yes,yes,Sol).

continue(P,t(N,F/G,[T1|Ts]),Bound,Tree1,no,Solved,Sol):-
	insert(T1,Ts,NTs),
	bestf(Ts,F1),
	expand(P,t(N,F1/G,Ts),Bound,Tree1,Solved,Sol).
	
continue(P,t(N,F/G,[_|Ts]),Bound,Tree1,never,Solved,Sol):-
	bestf(Ts,F1),
	expand(P,t(N,F1/G,Ts),Bound,Tree1,Solved,Sol).
	
succlist(_,[],[]).

succlist(G0,[N/C|NCs],Ts):-
	G is G0+C,
	h(N,H),
	succlist(G0,NCs,Ts1),
	insert(l(N,F/G),Ts1,Ts).
	
insert(T,Ts,[T|Ts]):-
	f(T,F),bestf(Ts,F1),
	F=<F1,!.

insert(T,[T1|Ts],[T1|Ts1]):-
	insert(T,Ts,Ts1).

f(l(_,F/_),F).
f(t(_,F/_,_).F).
bestf([T|_],F):-
	f(T,F).
bestf([],9999).
	
	


s(Tasks1*[_/F|Active1]*Fin1,Tasks2*Active2*Fin2,Cost):-
	del(Task/D,Tasks1,Tasks2),
	not(member(T/_,Tasks2),before(T,Task)),
	not(member(T1/F1,Active1),F<F1,before(T1,Task)),
	Time is F+D,
	insert(Task/Time,Active1,Active2,Fin1,Fin2),
	Cost is Fin2-Fin1.

s(Tasks*[_/F|Active1]*Fin,Tasks*Active2*Fin,0):-
	insertidle(F,Active1,Active2).
	
before(T1,T2):-
	prec(T1,T2).
	
before(T1,T2):-
	prec(T,T2),
	before(T1,T).
	
insert(S/A,[T/B|L],[S/A,T/B|B],F,F):-
	A=<B,!.
	
insert(S/A,[T/B|L],[T/B|L1],F1,F2):-
	insert(S/A,L,L1,F1,F2).

insert(S/A,[],[S/A],_,A).

insertidle(A,[T/B|L],[idle/B,T/B|L]):-
	A<B,!.
	
insertidle(A,[T/B|L],[T/B|L1]):-
	insertidle(A,L,L1).
	
del(A,[A|L],L).

del(A,[B|L],[B|L1]):-
	del(A,[A|L],L).
	
goal([]*_*_).
		
h(Tasks*Processors*Fin,H):-
	totaltime(Tasks,Tottime),
	sumnum(Processors,Ftime,N),
	Finall is (Tottime+Ftime)/N,
	(Finall>Fin,!,H is Finall-Fin
	;
	H=0
	).
	
totaltime([],0).

totaltime([_/D|Tasks],T):-
	totaltime(Tasks,T1),
	T is T1+D.
	
sumnum([],0,0).

sumnum([_/T|Procs],FT,N):-
	sumnum(Procs,FT1,N1),
	N is N1+1,
	FT is FT1+1.
	
%proapaitoumena tasks

prec(t1,t2). prec(t1,t3). prec(t1,t4). prec(t1,t5). prec(t1,t6).
prec(t2,t8). prec(t2,t9). prec(t3,t7). prec(t4,t8). prec(t4,t9).
prec(t5,t9). prec(t6,t8). prec(t7,t10). prec(t8,t10). prec(t9,t10).
	
start([t1/13,t2/17,t3/15,t4/13,t5/12,t6/10,t7/11,t8/10,t9/17,t10/15]*[idle/0,idle/0,idle/0]*0).