
:- ensure_loaded(['situation2StateUpdate.pl']).
:- ensure_loaded(['dfsPlannerWithListOfNodes.pl']).
:- ensure_loaded(['gripperRevised.pl']).

/*-This is the initial state implemented using atomic statetements as in CPS721--*/
/* In this planning instance there are 2 rooms, 2 balls, 1 robot with 2 grippers */

at_robby(robot1,room1,[ ]).
free(robot1,rgripper1,[ ]).     free(robot1,lgripper1,[ ]).
at(ball1,room1,[ ]).    at(ball2,room2,[ ]).

/* The predicates room(RM) and ball(B) provide types for the objects. 
   They are not fluents and for this reason they have no situation argument. */
room(room1).    room(room2).
ball(ball1).    ball(ball2).

/*--The goal state is defined with a list of fluent terms that must become true--*/

goalState( [at(ball1,room2), at(ball2,room1)] ).

/* % As suual, Plan lists actions in the reversed order %
?- solve_problem(6, Plan).
Plan = [drop(robot1, ball2, room1, lgripper1), move(robot1, room2, room1), drop(robot1, ball1, room2, rgripper1), pick(robot1, ball2, room2, lgripper1), move(robot1, room1, room2), pick(robot1, ball1, room1, rgripper1)]
Yes (0.00s cpu, solution 1, maybe more)
Plan = [drop(robot1, ball2, room1, rgripper1), move(robot1, room2, room1), pick(robot1, ball2, room2, rgripper1), drop(robot1, ball1, room2, rgripper1), move(robot1, room1, room2), pick(robot1, ball1, room1, rgripper1)]
Yes (0.00s cpu, solution 2, maybe more)
Plan = [drop(robot1, ball2, room1, lgripper1), move(robot1, room2, room1), pick(robot1, ball2, room2, lgripper1), drop(robot1, ball1, room2, rgripper1), move(robot1, room1, room2), pick(robot1, ball1, room1, rgripper1)]
Yes (0.00s cpu, solution 3, maybe more)
Plan = [drop(robot1, ball2, room1, rgripper1), move(robot1, room2, room1), drop(robot1, ball1, room2, lgripper1), pick(robot1, ball2, room2, rgripper1), move(robot1, room1, room2), pick(robot1, ball1, room1, lgripper1)]
Yes (0.00s cpu, solution 4, maybe more)
Plan = [drop(robot1, ball2, room1, lgripper1), move(robot1, room2, room1), pick(robot1, ball2, room2, lgripper1), drop(robot1, ball1, room2, lgripper1), move(robot1, room1, room2), pick(robot1, ball1, room1, lgripper1)]
Yes (0.00s cpu, solution 5, maybe more)
Plan = [drop(robot1, ball2, room1, rgripper1), move(robot1, room2, room1), pick(robot1, ball2, room2, rgripper1), drop(robot1, ball1, room2, lgripper1), move(robot1, room1, room2), pick(robot1, ball1, room1, lgripper1)]
Yes (0.00s cpu, solution 6, maybe more)
No (0.00s cpu)
*/
