location(home). location(mall1). location(walmart). location(winners). location(mall2).
car(c1).
item(p1). item(p2). item(p3). item(p4).
in_stock(p1, mall1, []). in_stock(p2, mall1, []). in_stock(p4, mall1, []). in_stock(p3, mall1, []). 
loc(c1, home, []).


goalState([picked_up(p1, mall1), picked_up(p2, mall1), picked_up(p3, mall1), picked_up(p4, mall1)]).