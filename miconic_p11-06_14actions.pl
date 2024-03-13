
above(f0,f1).  above(f0,f2).  above(f0,f3).  above(f0,f4).  above(f0,f5).
above(f0,f6).  above(f0,f7).  above(f0,f8).  above(f0,f9).  above(f0,f10).

above(f1,f2).  above(f1,f3).  above(f1,f4).  above(f1,f5).
above(f1,f6).  above(f1,f7).  above(f1,f8).  above(f1,f9).  above(f1,f10).

above(f2,f3).  above(f2,f4).  above(f2,f5).  above(f2,f6).  above(f2,f7).  
above(f2,f8).  above(f2,f9).  above(f2,f10).

above(f3,f4).  above(f3,f5).  above(f3,f6).  above(f3,f7).  
above(f3,f8).  above(f3,f9).  above(f3,f10).

above(f4,f5).  above(f4,f6).  above(f4,f7).  
above(f4,f8).  above(f4,f9).  above(f4,f10).

above(f5,f6).  above(f5,f7).  above(f5,f8).  above(f5,f9).  above(f5,f10).
above(f6,f7).  above(f6,f8).  above(f6,f9).  above(f6,f10).
above(f7,f8).  above(f7,f9).  above(f7,f10).
above(f8,f9).  above(f8,f10).     above(f9,f10).

origin(p0,f2).  origin(p1,f6).  origin(p2,f2). origin(p3,f3). origin(p4,f4).  origin(p5,f7).
destin(p0,f3).  destin(p1,f10). destin(p2,f5). destin(p3,f7). destin(p4,f3).  destin(p5,f1).

lift_at(f0,[ ]).

/*%goalState([served(p0), served(p1),served(p2),served(p3),served(p4),served(p5)] ).*/
goalState( [ served(p0), served(p1),served(p2),served(p3) ] ).
/* CPS721 planner cannot solve the problem for 3 people in 10 minutes */
%goalState( [served(p0), served(p1), served(p2)] ).  %needs at least 12 actions

%goalState( [served(p0), served(p1)] ).  %can be solved with 8 actions

/*
?- solve_problem(8, Plan).
Plan = [depart(f10, p1), up(f6, f10), board(f6, p1), up(f3, f6), depart(f3, p0), up(f2, f3), board(f2, p0), up(f0, f2)]
Yes (47.09s cpu, solution 1, maybe more)
Plan = [depart(f3, p0), down(f10, f3), depart(f10, p1), up(f6, f10), board(f6, p1), up(f2, f6), board(f2, p0), up(f0, f2)]
Yes (47.74s cpu, solution 2, maybe more)
Plan = [depart(f10, p1), up(f3, f10), depart(f3, p0), down(f6, f3), board(f6, p1), up(f2, f6), board(f2, p0), up(f0, f2)]
Yes (47.75s cpu, solution 3, maybe more)
Plan = [depart(f3, p0), up(f2, f3), board(f2, p0), down(f10, f2), depart(f10, p1), up(f6, f10), board(f6, p1), up(f0, f6)]
Yes (145.12s cpu, solution 4, maybe more)
Plan = [depart(f10, p1), up(f3, f10), depart(f3, p0), up(f2, f3), board(f2, p0), down(f6, f2), board(f6, p1), up(f0, f6)]
Yes (145.74s cpu, solution 5, maybe more)
Plan = [depart(f3, p0), down(f10, f3), depart(f10, p1), up(f2, f10), board(f2, p0), down(f6, f2), board(f6, p1), up(f0, f6)]
Yes (145.76s cpu, solution 6, maybe more)
No (259.34s cpu)
*/
