location(home). location(mall1). location(walmart). location(winners). location(mall2).
car(c1). car(c2).
item(p1). item(p2). item(p3). item(p4).
drivable(c1, []).
in_stock(p1, mall1, []). in_stock(p2, walmart, []). in_stock(p3, winners, []). in_stock(p4, mall2, []). in_stock(p1, walmart, []). 
loc(c1, home, []). loc(c2, home, []). 

%%%initial_state([location(home), car(c1), item(p1), drivable(c1), in_stock(p1,mall1), loc(c1,home)]).

goalState([picked_up(p1,mall1)]).

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
poss(order(P,L), [neg(ordered(P,L))], State) :- instantiate([location(L), item(P), in_stock(P,L)], State).
poss(pickup(P,L), [neg(picked_up(P,L)), in_stock(P,L), ready_for_pickup(P,L)], State) :- instantiate([location(L), item(P)], State).
poss(fix_car(C), [neg(drivable(C))], State) :- instantiate([car(C)], State).

actionPrec(drive(C, L1, L2), [drivable(C), loc(C, L1)]).
actionPrec(order(P,L), [in_stock(P,L)]).
actionPrec(pickup(P,L), [ordered(P,L), loc(C,L), ready_for_pickup(P,L)]).
actionPrec(fix_car(C), []).

posEffect(drive(C,L1,L2), State, loc(C,L2)).
posEffect(order(P,L), State, ordered(P,L)).
posEffect(order(P,L), State, ready_for_pickup(P,L)).

posEffect(fix_car(C), State, drivable(C)).

negEffect(drive(C,L1,L2), State, loc(C, L1)).
negEffect(order(P,L), State, in_stock(P,L)).
negEffect(order(P,L), State, picked_up(P,L)).
negEffect(pickup(P,L), State, picked_up(P,L)).
negEffect(pickup(P,L), State, ready_for_pickup(P,L)).


