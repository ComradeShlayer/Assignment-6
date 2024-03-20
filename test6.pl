location(home). location(mall1). location(walmart). location(winners). location(mall2).
car(c1).
item(p1). item(p2). item(p3). item(p4).
in_stock(p1, mall1, []). in_stock(p2, walmart, []). in_stock(p3, winners, []). in_stock(p4, mall2, []). in_stock(p1, walmart, []). 
loc(c1, home, []). loc(c2, home, []). 


goalState([drivable(c1), loc(c1, mall1)]).