
%fluents( [clear(_,_),on_table(_,_),on(_,_,_)] ).
%fluent(clear(_)).   fluent(on_table(_)).    fluent(on(_,_)).

on_table(b1,[]).    on_table(b3,[]).
on_table(b4,[]).    on_table(b5,[]).
on(b2,b3,[]).   on(b6,b8,[]).
on(b7,b1,[]).   on(b8,b4,[]).
clear(b2, [ ]). clear(b5, [ ]).
clear(b6, [ ]). clear(b7, [ ]).

goalState( [on(b1,b2), on(b2,b3), on(b6,b8), on(b7,b1)] ). 

/*                                        
                                          b7
              b6          --> goal -->    b1
    b7   b2   b8                          b2    b6
    b1   b3   b4    b5                    b3    b8
   --------------------

?- solve_problem(3, Plan).
    Plan = [move_b_to_b(b7, b5, b1), move_t_to_b(b1, b2), move_b_to_b(b7, b1, b5)]
        Yes (0.01s cpu, solution 1, maybe more)
    Plan = [move_b_to_b(b7, b6, b1), move_t_to_b(b1, b2), move_b_to_b(b7, b1, b6)]
        Yes (0.01s cpu, solution 2, maybe more)
    Plan = [move_t_to_b(b7, b1), move_t_to_b(b1, b2), move_b_to_t(b7, b1)]
        Yes (0.01s cpu, solution 3, maybe more)
        No (0.01s cpu)
*/



