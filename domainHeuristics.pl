%--------------------------------------------------------------------------
%  In our domain there are 5 fluents :
%   loc(Entity,Location) holds if entity is at location
%   ready_for_pickup(Object, Location) holds if Object is currently ready for pickup at that Location
%   in_stock(Object, Location) holds if Object is currently in stock at that Location
%   picked_up(Object, Location) holds if Object is picked up
%   ordered(Object, Location) holds if Object is ordered to Location
%   drivable(Car) holds if Car is not broken
%
%   There are also 5 action terms:
%   drive(C, L1, L)   moves car C from location 1 to location
%   order(P,L)  orders package p to location L
%   pickup(P)  picksup the package p
%   fix_car(C) fixes the broken car C
%   driveHome(C) moves car C to home
%--------------------------------------------------------------------------
fluents([loc(_,_,_), ready_for_pickup(_,_,_), in_stock(_,_,_), picked_up(_,_,_), ordered(_,_,_), drivable(_,_)]).
fluent(loc(_,_)). fluent(ready_for_pickup(_,_)). fluent(in_stock(_,_)). fluent(picked_up(_,_)). fluent(ordered(_,_)). fluent(drivable(_)).
fluent(ordered(P)). fluent(picked_up(P)). fluent(in_stock(P)). fluent(ready_for_pickup(P)).
%%%drive(C, L1, L)      order(P,L)      pickup(P)   fix_car(C)      driveHome(C)

poss(drive(C,L1,L2), [neg(L1 = L2)], State) :- instantiate([location(L1), location(L2), car(C), drivable(C), loc(C,L1)], State).
poss(order(P,L), [neg(ordered(P,L)), neg(picked_up(P,L)), neg(ready_for_pickup(P,L))], State) :- instantiate([location(L), item(P), in_stock(P,L)], State).
poss(pickup(P,L), [neg(picked_up(P,L))], State) :- instantiate([location(L), item(P), car(C), ordered(P,L), ready_for_pickup(P,L), loc(C,L)], State).
poss(fix_car(C), [neg(drivable(C))], State) :- instantiate([car(C)], State).

actionPrec(drive(C, L1, L2), [drivable(C), loc(C, L1)]).
actionPrec(order(P,L), [in_stock(P,L)]).
actionPrec(pickup(P,L), [ordered(P,L), loc(C,L), ready_for_pickup(P,L)]).
actionPrec(fix_car(C), []).

actionAuxPrec(drive(C,L1,L2), [car(C), location(L1), location(L2), neg(L1 = L2)]).
actionAuxPrec(order(I,L), [item(I), location(L)]).
actionAuxPrec(pickup(P, L), [item(P), location(L)]).
actionAuxPrec(fix_car(C), [car(C)]).

posEffect(drive(C,L1,L2), State, loc(C,L2)).
posEffect(order(P,L), State, ordered(P,L)).
posEffect(order(P,L), State, ready_for_pickup(P,L)).
posEffect(fix_car(C), State, drivable(C)).
posEffect(pickup(P,L), State, picked_up(P, L)).

negEffect(drive(C,L1,L2), State, loc(C, L1)).
negEffect(order(P,L), State, in_stock(P,L)).
negEffect(order(P,L), State, picked_up(P,L)).
negEffect(pickup(P,L), State, ready_for_pickup(P,L)).
negEffect(pickup(P,L), State, ordered(P,L)).

useless(drive(C,L1,L2), List) :- member(drive(C,L2,L1), List).