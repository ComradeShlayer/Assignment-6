%%%at(C,L,S)    ready_for_pickup(I,L,S)     in_stock(I,L,S)
%%%picked_up(I,S)       ordered(I,L,S)     drivable(C,S)

fluents([at(_,_,_), ready_for_pickup(_,_,_), in_stock(_,_,_), picked_up(_,_), ordered(_,_,_), drivable(_,_)])
fluent(at(_,_)). fluent(picked_up(_)). fluent(in_stock(_)).
fluent(ready_for_pickup(_)). fluent(ordered(_)). fluent(drivable(_)).

%%%drive(C, L)      order(P,L)      pickup(P)   fix_car(C)      driveHome(C)

poss(drive(C,L), [], S) :- drivable(C).
poss(order(I,L), [not(ordered(I,_))], S) :- in_stock(I, L).
poss(pickup(P), [], S) :- ordered(P, L, S), instatiante([ready_for_pickup(P), at(C, L)], S).
poss(fix_car(C), [not(drivable(C))], S).
poss(driveHome(C), [], S) :- not(instatiante([ordered(I), not picked_up(I)],S)).

actionPrec(drive(C,L), [drivable(C)]).
actionPrec(order(I,L), [in_stock(I, L)]).
actionPrec(pickup(P), [ordered(P, L), at(C, L), ready_for_pickup(P)]).
actionPrec(fix_car(C), [not(drivable(C))]).
actionPrec(driveHome(C), [ordered(I), not(picked_up(I))]).
