askyn(X) :- write('[y/n]: '), read(Z), Z = X.


displayln(L) :- flatten(L,L1), !, do_displayln(L1).
do_displayln([]) :- nl, !.
do_displayln([Head|Tail]) :- write(Head), write(' '), displayln(Tail).

display(L) :- flatten(L,L1), !, do_display(L1).
do_display([]) :- !.
do_display([Head|Tail]) :- write(Head), write(' '), display(Tail).


get_input(Question,Answer) :- display(Question), read(Answer).

affirm(Question) :- display(Question), write('[y/n]: '), read(y).

dump_dt(Object) :- dt(Object,Tstart,Tend), displayln(['You know about a time interval between ',Tstart,' and ',Tend]), fail.
dump_dt(_).


dt_help_phrase(N,['any other']) :- N > 1.
dt_help_phrase(0,['a']).                

filter_same_times(Object,List,Out) :- findall(A,adjacent_times(Object,A,_),L1), findall(B,adjacent_times(Object,_,B),L2), append(L1,L2,C), subtract(List,C,Out).
                                            

