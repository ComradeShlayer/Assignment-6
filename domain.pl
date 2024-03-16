%%%at(C,L,S)    ready_for_pickup(I,L,S)     in_stock(I,L,S)
%%%picked_up(I,S)       ordered(I,L,S)     drivable(C,S)

fluents([at(_,_,_), ready_for_pickup(_,_,_), in_stock(_,_,_), picked_up(_,_), ordered(_,_,_), drivable(_,_)])
fluent(at(_,_)). fluent(picked_up(_)). fluent(in_stock(_)).
fluent(ready_for_pickup(_)). fluent(ordered(_)). fluent(drivable(_)).

%%%drive(C, L1, L)      order(P,L)      pickup(P)   fix_car(C)      driveHome(C)

poss(drive(C,L1, L2), [], S) :- drivable(C), at(C,L1).
poss(order(I,L), [not(ordered(I,_))], S) :- in_stock(I, L).
poss(pickup(P), [], S) :- ordered(P, L, S), instatiante([ready_for_pickup(P), at(C, L)], S).
poss(fix_car(C), [not(drivable(C))], S).
poss(driveHome(C,L), [], S) :- drivable(C), at(C,L), not(instatiante([ordered(I), not picked_up(I)],S)).

actionPrec(drive(C,L1, L2), [drivable(C), at(C,L1)]).
actionPrec(order(I,L), [in_stock(I, L)]).
actionPrec(pickup(P), [ordered(P, L), at(C, L), ready_for_pickup(P)]).
actionPrec(fix_car(C), [not(drivable(C))]).
actionPrec(driveHome(C, L), [at(C,L), drivable(C), not(ordered(I), not(picked_up(I)))]).

actionAuxPrec(drive(C,L1,L2), [car(C), location(L1), location(L2)]).
actionAuxPrec(order(I,L), [item(I), location(L)]).
actionAuxPrec(pickup(P), [package(P)]).
actionAuxPrec(fix_car(C), [car(C)]).
actionAuxPrec(driveHome(C, L), [car(C), location(L)]).

posEffect(drive(C,L1,L2),S,at(C,L2)).
posEffect(order(I,L),S,ordered(I,L)).
posEffect(pickup(P),S,picked_up(I)).
posEffect(fix_car(C),S,drivable(C)).
posEffect(driveHome(C, L),S,[at(C,L), drivable(C), not(ordered(P), not picked_up(P))]). #IDK if we're allowed to use not

negEffect(drive(C,L1,L2), State, not(drivable(C)).
negEffect(drive(C,L), State, not(at(C,L1))).
negEffect(order(I,L), State, not(in_stock(I,L))).
negEffect(pickup(P), State, not(ready_for_pickup(P))).
negEffect(fix_car(C), State, not(drivable(C))).
negEffect(driveHome(C), State, [ordered(P), not picked_up(P)]).
negEffect(driveHome(C), State, not(drivable(C))).
