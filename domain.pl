%%%loc(C,L,S)    ready_for_pickup(I,L,S)     in_stock(I,L,S)
%%%picked_up(I,S)       ordered(I,L,S)     driveable(C,S)


location(home). location(mall1). location(walmart). location(winners). location(mall2).
car(c1). car(c2).
item(p1). item(p2). item(p3). item(p4).
driveable(c1).
in_stock(p1, mall1). in_stock(p2, walmart). in_stock(p3, winners). in_stock(p4, mall2). in_stock(p1, walmart). 

loc(c1, home). loc(c2, home). 
fluents([loc(_,_,_), ready_for_pickup(_,_,_), in_stock(_,_,_), picked_up(_,_), ordered(_,_,_), driveable(_,_)]).
fluent(loc(_,_)). fluent(picked_up(_)). fluent(in_stock(_)).
fluent(ready_for_pickup(_)). fluent(ordered(_)). fluent(driveable(_)).

%%%drive(C, L1, L)      order(P,L)      pickup(P)   fix_car(C)      driveHome(C)

poss(drive(C,L1, L2), [], S) :- instantiate([driveable(C), loc(C,L1)],S).
poss(order(I,L), [], S) :- instantiate([in_stock(I, L), ordered(I2)],S), I2 \= I.
poss(pickup(P), [], S) :- instantiate([ordered(P, L), ready_for_pickup(P), loc(C, L)],S).
poss(fix_car(C), [not(driveable(C))], S).
%%%poss(driveHome(C,L), [], S) :- instantiate([driveable(C), loc(C,L), not(ordered(I), not picked_up(I))]).

actionPrec(drive(C,L1, L2), [driveable(C), loc(C,L1)]).
actionPrec(order(I,L), [in_stock(I, L)]).
actionPrec(pickup(P), [ordered(P, L), loc(C, L), ready_for_pickup(P)]).
actionPrec(fix_car(C), [not(driveable(C))]).
%%%actionPrec(driveHome(C, L), [loc(C,L), driveable(C), not(ordered(I), not(picked_up(I)))]).

actionAuxPrec(drive(C,L1,L2), [car(C), location(L1), location(L2)]).
actionAuxPrec(order(I,L), [item(I), location(L)]).
actionAuxPrec(pickup(P), [item(P)]).
actionAuxPrec(fix_car(C), [car(C)]).
%%%actionAuxPrec(driveHome(C, L), [car(C), location(L)]).

posEffect(drive(C,L1,L2),S,loc(C,L2)).
posEffect(order(I,L),S,ordered(I,L)).
posEffect(pickup(P),S,picked_up(I)).
posEffect(fix_car(C),S,driveable(C)).
%%%posEffect(driveHome(C, L),S,[loc(C,L), driveable(C), not(ordered(P), not picked_up(P))]).

negEffect(drive(C,L1,L2), State, not(driveable(C))).
negEffect(drive(C,L), State, not(loc(C,L1))).
negEffect(order(I,L), State, not(in_stock(I,L))).
negEffect(pickup(P), State, not(ready_for_pickup(P))).
negEffect(fix_car(C), State, not(driveable(C))).
negEffect(driveHome(C), State, [ordered(P), not picked_up(P)]).
%%%negEffect(driveHome(C), State, not(driveable(C))).
