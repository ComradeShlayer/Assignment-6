location(home). location(mall1). location(walmart). location(winners). location(mall2).
car(c1).
item(p1). item(p2). item(p3). item(p4).
drivable(c1, []).
in_stock(p1, mall1, []). in_stock(p2, walmart, []). in_stock(p3, winners, []). in_stock(p4, mall2, []). in_stock(p1, walmart, []). 
loc(c1, home, []). loc(c2, home, []). 


goalState([ready_for_pickup(p2,walmart), picked_up(p1, walmart)]).