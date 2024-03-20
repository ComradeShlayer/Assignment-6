location(home). location(mall1). location(walmart). location(winners). location(mall2).
car(c1).
item(p1). item(p2). item(p3). item(p4).
drivable(c1, []). ordered(p1,mall1, []). ordered(p2, walmart, []). ordered(p3, winners, []). ordered(p4, mall2, []).
loc(c1, home, []). ready_for_pickup(p1,mall1, []). ready_for_pickup(p2, walmart, []). ready_for_pickup(p3, winners, []). ready_for_pickup(p4, mall2, []).

goalState([picked_up(p1,mall1), picked_up(p2, walmart), picked_up(p3, winners), picked_up(p4, mall2)]).