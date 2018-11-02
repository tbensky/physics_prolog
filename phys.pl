:- include('io.pl').

physics_law(Object,[x(Object,Tend),x0(Object,Tstart),v0(Object,Tstart),a(Object,Tstart,Tend),dt(Object,Tstart,Tend)],'x=x0+v0*dt+1/2*a*dt^2').
physics_law(Object,[v(Object,Tend),v0(Object,Tstart),a(Object,Tstart,Tend),dt(Object,Tstart,Tend)],'v=v0+a*dt').

physics_law(Object,[v(Object,Tend),v0(Object,Tstart)],['v(',Object,',',Tend,')=v0(',Object,',',Tstart,')']) :- adjacent_times(Object,Tend,Tstart).

physics_law(Object,[x(Object,Tend),x0(Object,Tstart)],['x(',Object,',',Tend,')=x0(',Object,',',Tstart,')']) :- adjacent_times(Object,Tend,Tstart).                                                                                                                                                                 

physics_law(Object,[v(Object,Tend),v0(Object,Tstart),a(Object,Tstart,Tend),dt_bounds(Object,Desc,A,B)],'v=v0+a*dt') :- 
                                                time_bounds(Object,Desc,A,B), same_time(Object,A,Tstart), same_time(Object,B,Tend).
                                                
physics_law(Object,[x(Object,Tend),x0(Object,Tstart),v0(Object,Tstart),a(Object,Tstart,Tend),dt_bounds(Object,Desc,A,B)],'x=x0+v0*dt+1/2*a*dt^2') :-
                                                time_bounds(Object,Desc,A,B), same_time(Object,A,Tstart), same_time(Object,B,Tend).                                                                                
                                                                                                                                                                          
question(Object,dt(Object,Tstart,Tend)) :- dump_dt(Object), count_dts(Object,N), dt_help_phrase(N,Phrase),
                                            get_input(['Do you know about ',Phrase,' time interval that involves the ',Object,'? '],y), 
                                            get_input(['What is the ',Object,' doing at the start of this time interval?'],Tstart), 
                                            get_input(['What is the ',Object,' doing at the end of this time interval?'],Tend).
                                            
question(Object,time_bounds(Object,Desc,Tstart,Tend)) :- does_this(Object,Desc), not(time_bounds(Object,Desc,_,_)),
                                           get_input(['What caused the ',Object,' to ',Desc,'? '],Tstart),
                                           get_input(['What happens to the ',Object,' because of the ',Desc,'? '],Tend).
                                          
question(Object,a(Object,Tstart,Tend)) :- maybe_dt(Object,Tstart,Tend), not(a(Object,Tstart,Tend)),
                                          get_input(['Do you know the acceleration of the ',Object,' during the time interval from ',Tstart,' to ',Tend,'? '],y).

question(Object,v0(Object,Tstart)) :- maybe_dt(Object,Tstart,_), not(v0(Object,Tstart)), affirm(['Do you know the speed of the ',Object,' when ',Tstart,'? ']).

question(Object,v(Object,Tend)) :- maybe_dt(Object,_,Tend), not(v(Object,Tend)), affirm(['Do you know the speed of the ',Object,' when ',Tend,'? ']).

question(Object,x0(Object,Tstart)) :- maybe_dt(Object,Tstart,_), not(x0(Object,Tstart)), affirm(['Do you know the position of the ',Object,' when ',Tstart,'? ']).

question(Object,x(Object,Tend)) :- maybe_dt(Object,_,Tend), not(x(Object,Tend)), affirm(['Do you know the position of the ',Object,' when ',Tend,'? ']).

question(Object,dt(Object,Tstart,Tend)) :- time_bounds(Object,_,Tstart,Tend), not(dt(Object,Tstart,Tend)),
                                            affirm(['Do you know how long it took for the ',Object,' to go from ',Tstart,' to ',Tend,'? ']),
                                            retract(does_this(Object,Desc)), retract(time_bounds(Object,Desc,_,_)).
                                           
question(Object,adjacent_times(Object,Time1,Time2)) :- 
                                                    findall(Tstart,dt(Object,Tstart,_),Ls), 
                                                    findall(Tend,dt(Object,_,Tend),Le),
                                                    findall(Estart,time_bounds(Object,_,Estart,_),Lse), 
                                                    findall(Eend,time_bounds(Object,_,_,Eend),Lee),
                                                    append(Ls,Le,Ldt),
                                                    append(Lse,Lee,Ltb),
                                                    append(Ldt,Ltb,Lbig),
                                                    filter_same_times(Object,Lbig,Lfiltered),
                                                    length(Lfiltered,N), N > 0,
                                                    displayln(['Is it possible that an event in this list is closely connected to (or is actually the same as) another event? ']),
                                                    write(Lfiltered), 
                                                    affirm([]), !, 
                                                    display(['Which event leads into the other (or comes first)? ']), read(Time1), 
                                                    display(['And which event then takes over? ']), read(Time2).
                                                    
question(Object,does_this(Object,Desc)) :-  affirm(['Using just a few words, can you say anything else that the ',Object,' does in this problem? ']),
                                            get_input(['Ok,what? '],Desc).                
                                            
dt_bounds(_,_,_,_) :- !, fail.

maybe_dt(Object,Tstart,Tend) :- same_time(Object,Tstart,A), same_time(Object,Tend,B), time_bounds(Object,_,A,B), !.
maybe_dt(Object,Tstart,Tend) :- dt(Object,Tstart,Tend).

same_time(Object,T1,T2) :- adjacent_times(Object,T1,T2) ;  adjacent_times(Object,T2,T1).
same_time(_,Tself,Tself).  

                                                                                                                                                       

check_laws(Object) :- physics_law(Object,Law,Advice), length(Law,N), check_terms(Object,Law,Failed,N), 
                      nl,
                      displayln(['You can do this next:']),
                      check_dt_bounds(Failed,Failed1),
                      displayln(['Using ',Advice,' you can find ',Failed1,' of the ',Object,' by using ']),
                      displayln(Law),
                      assert(Failed1), fail.
check_laws(_).

check_dt_bounds(dt_bounds(Object,Desc,Tstart,Tend),dt(Object,Tstart,Tend)) :-  retract(does_this(Object,Desc)), retract(time_bounds(Object,Desc,_,_)), !.
check_dt_bounds(A,A).

count_dts(Object,N) :- findall(Object,dt(Object,_,_),L), length(L,N).        
     
check_terms(_,[],_,0) :- !, fail.        
check_terms(_,[],Failed,1) :- nonvar(Failed), not(call(Failed)).
check_terms(_,[],_,_) :- !, fail.
check_terms(Object,[Head|Tail],Failed,N) :- call(Head), N1 is N-1, check_terms(Object,Tail,Failed,N1).
check_terms(Object,[Failed|Tail],Failed,N) :- check_terms(Object,Tail,Failed,N).

solve(Object) :- question(Object,Code), assert(Code), fail.
solve(Object) :- check_laws(Object).

:- dynamic(x/2), dynamic(x0/2), dynamic(v0/2), dynamic(dt/3), dynamic(a/3), dynamic(v/2), dynamic(adjacent_times/3), dynamic(does_this/2),
        dynamic(time_bounds/4).