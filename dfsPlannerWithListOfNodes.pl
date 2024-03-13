/* 
   This is a planner that does iterative Depth-First Search (DFS) 
   to compute a plan. A similar planner is provided in Chapter 9 of
   Hector Levesque's textbook "Thinking as Computation".
   A version of this program is provided from Hector Levesque's Web page 
   "Supplementary material for Thinking as Computation" posted at
       https://www.cs.toronto.edu/~hector/tc-instructor.html
  This material is free, of course, but without support or warranty. 
*/

solve_problem(Bound,Plan)  :-  
     C0 is cputime,
     max_length(Plan,Bound),
     reachable(State,Plan),
     goal(State),
     Cf is cputime, D is Cf - C0, nl,
     write('Elapsed time (sec): '), write(D), nl.
 
max_length([],N) :- N >= 0.
max_length([First | L],N1) :- N1 > 0, N is N1 - 1, max_length(L,N).
 
reachable(S,[]) :- initial_state(S). 
reachable(NextState, [M | ListOfActions]) :- 
    reachable(State,ListOfActions),
    legal_move(M,State),
     %  The next line is optional if "useless" is implemented %
     %  not useless(M,ListOfActions),
     % NextState is progression of State through action M
     update(State,M,NextState).
 
legal_move(A, State) :- 
  poss(A, PrecondList, State), %!,     % each action has only one list of preconditions
  evaluate(PrecondList,State). %! % If preconditions fail, there is no need to update
 
initial_state(State) :- buildNodesList(State), !.
goal(State) :- goalState(List), !, evaluate(List,State). % check if all goal literals hold in State
