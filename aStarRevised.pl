%% aStarRevised.pl %%
%% Author: Mikhail Soutchanski. 
%% Contact Email:   soutchanski (at) protonmail (dot) com
%% A deductive A* planner that does forward heuristic search over the situation tree.
%% Copyright (c) 2014, 2016, 2017, 2018, 2022, 2024 Mikhail Soutchanski
%% This software is provided  under the MIT License as stated at
%%          https://opensource.org/license/MIT
%%
%% History of this planner.
%% June 2014: a preliminary version of A* planner in situation calculus with a domain specific heuristic.
%% 2016 (end of June): A* planner based on the situation calculus with regression and an external heuristic.
%% Middle of July 2017: the first domain independent A* planner based on the situation calculus with 
%%      an implementation of the Plan Graph relaxation heuristic and with progression over BinTree.
%%      The planner can work with any other domain independent heuristic.
%% January 27, 2018: an improved progression in a new BinTree version with hash-tables, minor changes in the planner.
%% May 20, 2022 version: an improved implementation of the Plan Graph heuristic from 2017 and 2018 releases.
%%      It includes term pg(Sit,State) initialization layer and term nextLayer(NewActs,NewAddEff,State, PG).
%% June 20, 2022 release: Minor changes (e.g. formatting, removing singleton variables) 
%%      made by Ryan Young; factored out heuristic predicates. Uses best support action to count reachability score.
%% November 27, 2022: new release by Mikhail Soutchanski  and Ryan Young.
%%      Updates in Plan Graph heuristic to make it more efficient; removed redundant calculations.
%% The paper:  Mikhail Soutchanski and Ryan Young  "Planning as Theorem Proving with Heuristics"
%%      https://arxiv.org/abs/2303.13638    (version 1 was uploaded in March 2023)
%% January 2024:  a new simpler data structure for representing states. Each state
%%      is a list of nodes, where each node consists of a pair with a fluent name 
%%      and the list of fluent instances that hold in the state.
%%
%% This program is provided as a handout for the students enrolled in CPS822/CP8314
%% course "Advanced AI" at the Toronto Metropolitan University. See details at
%%      https://www.cs.torontomu.ca/~mes/courses/822/info.html
%% The students cannot distribute this program without permission of the instructor.
%%
%% --------------------------------------------------------------------------

:- lib(timeout_simple).
:- ensure_loaded(['situation2StateUpdate.pl']).
:- ensure_loaded(['planningGraphHeuristicRevised.pl']).

% Computational Intelligence: a logical approach. 
% Prolog Code available at  https://www.cs.ubc.ca/~poole/ci/ci_code.html
% An efficient implementation of Priority Queues.
% Copyright (c) 1998, Poole, Mackworth, Goebel and Oxford University Press.
%   File   : pq.pl                                                            %
%   Author : David Poole, poole@cs.ubc.ca                                     %
%   Updated: 26 Septermber 1994                                               %
%   Purpose: Implement priority queues in Prolog.                             %

:- ensure_loaded(['pq.pl']).

%:- pragma(nodebug).
%:- dynamic(stophere/0).
%:- set_flag(stophere/0, spy, on).
%:- set_flag(stophere/0, leash, stop).

%stophere.

% This is an A* planner that does forward search over the situation tree. 
% Since each search node is a unique situation, the previously visited nodes
% cannot be reached by search again, and there is no need to maintain a priority
% queue of the visited nodes. Moreover, the frontier nodes cannot be reached along
% different paths since each situation represents a unique path on the situation
% tree. Therefore, there is no need to check whether new neighbors to be 
% explored have been already included in the frontier. In this planner, cost 
% of every action is considered to be 1, and the cost of a path to a node is 
% simply the length of situation representing this search node.  

% In this version, a state for current situation is represented as a list of
% nodes that consist of a fluent name and the list of tuples. This data structure
% faciliates planning in the domains with changing objects and with a potentially
% infinite state space.  

%:- discontiguous(h/5).  %heuristic function is defined elsewhere
%:- discontiguous(useless/2).  % declarative heuristics are defined elsewhere
        % Declarative heuristics can be learned from experience of
        % solving planning problems repeatedly in the same domain. 

/* buildNodesList(-State) is implemented in situation2StateUpdate.pl */
initial_state(State) :- buildNodesList(State), !.

% The predicate evaluate(List,State)  is implemented in situation2StateUpdate.pl
% In STRIPS, List defining a goal state is conjunction of ground literals.
% Therefore, these ground literals can be evaluated one-by-one.
% Predicate goalState(-ListGoalLiterals) is domain dependent: see a domain file.

goal(State) :- goalState(List), !, evaluate(List,State).

legal_move(A, State) :- 
    poss(A, PrecondList, State),       % list of preconditions is unique
    evaluate(PrecondList,State).       %NO cut to allow back-tracking

/* Predicate  progress(+InputState,+Situation, -CurrentState)  */
progress(InitState,[ ],InitState) :- !.

progress(State,[A | S],NewState) :-
     update(State,A,IntermState),
     progress(IntermState,S,NewState).

% plan(+B,+F,-S) is true if S is a sequence of actions no longer than bound B 
% leading from the initial state to a goal state. In the Situation Calculus, 
% there is the single initial state represented as the empty list of actions. 
% The bound B makes sure that search will _always_ terminate. However, due to this
% bound, search may terminate prematurely, i.e., without reaching a goal state.
% Therefore, to be useful, the bound B should overestimate the length of a plan. 
% The nodes in the search space are represented as sequences of actions from
% the initial state, but no attempt is made in this planner to check whether two
% progressions of an initial theory are same or not for two different sequences S
%    Usage: submit query ?- plan(100, graphplan, P).

plan(Bound,HFunction,S) :- 
   setval(precond_time, 0),
   setval(update_time, 0),
    setval(reach_time, 0),
%   setval(support_time, 0),
    setval(states_visited, 0),
    Start is cputime,
    initial_state(InitState), !,
    Value is Bound + 1,
    insertPQ(empty,Value,[],PQ), !,  % initial piority queue is the constant "empty"
    %If search failed, there is no plan since this planner is sound and complete
    boundedAStar(PQ, InitState, HFunction, Bound, S),
    End is cputime,
    Time is End - Start,
    length(S, Length),
   getval(precond_time, PrecondTime),
   getval(update_time, UpdateTime),
    getval(reach_time, ReachTime),
%   getval(support_time, SupportTime),
    getval(states_visited, NumStates),
    write(Length), write(', '),
    write(NumStates), write(', '),
   write(PrecondTime), write(', '), 
   write(UpdateTime), write(', '), 
   write(ReachTime), write(', '),
%   write(SupportTime), write(', '),
    write(Time), 
    nl.
 
% The predicate  boundedAStar(+PQ,+State,+F,+B, -Plan)  is true if Plan is a 
% sequence of actions to a goal state found from State by doing search with 
% a heuristic function F with the upper bound B on the depth of situation tree. 
% Search continues from the current State by taking the most promising node 
% from the frontier stored in the priority queue PQ.

boundedAStar(Frontier,InitState, _, _, Plan) :- 
    getval(states_visited, NumStatesOld),
    removePQ(Frontier,_,Plan,_), 
    NumStatesNew is NumStatesOld + 1,
    setval(states_visited, NumStatesNew),
   getval(update_time, UpdateTimeOld),
   UpdateStart is cputime,
   % reverse Plan: RS starts with the 1st action, ends with the last action
    reverse(Plan,RS),   
    term_string(InitState,TempString),  
    term_string(TempState,TempString),  
    progress(TempState,RS,CurrentState), % update initial state using RS
   UpdateEnd is cputime,
   UpdateTime is UpdateEnd - UpdateStart,
   UpdateTimeNew is UpdateTimeOld + UpdateTime,
   setval(update_time, UpdateTimeNew),
    goal(CurrentState).       

% If the goal is true in S, return Plan. Initially, we check if [] is a goal,
% but subsequently we check recursively every root of PQ whether it is a goal
% before we remove it. If the rule above fails, then the root of PQ is not
% a goal and it can be safely removed in the next rule.

boundedAStar( Frontier, InitState, HF, Bound, Plan) :- 
    removePQ( Frontier,_,Sit,PQ ),  
   getval(update_time, UpdateTimeOld),
   UpdateStart is cputime,
    reverse(Sit,RS),    
    term_string(InitState,TempString),  
    term_string(TempState,TempString),  
    progress(TempState,RS,CurState), 
   UpdateEnd is cputime,
   UpdateTime is UpdateEnd - UpdateStart,
   UpdateTimeNew is UpdateTimeOld + UpdateTime,
   setval(update_time, UpdateTimeNew),
    length(Sit,L),          % L is the cost of the path to Sit
    % If the length has not yet reached the bound, then expand Sit.
    % Expanding means exploring successor situations that follow after Sit.
    % Else, explore recursively the next priority node from the PQ.
    % When evaluating Neighbors of Sit, an heuristic function can use 
    % DepthRemain=(Bound-L) as an estimate how far to look ahead.
    % But the Bound on the depth of search remains invariant throughout search. 
    ( L < Bound -> ( DepthRemain is Bound - L, 
        successors(Sit,CurState,HF,DepthRemain,Neighbors,Estimates), ! ) ;
        ( Neighbors=[], Estimates=[] )
    ),
    updateFrontier(PQ,Neighbors,L,Estimates,NewFrontier),
    % if Neighbors=Estimates=[], then NewFrontier=PQ and we consider the next root
    boundedAStar(NewFrontier, InitState, HF, Bound, Plan).
    

% successors(+Sit,+State,+F,+Depth,-Neighbors,-Estimates) 
% Computes the list Neighbors of situations directly reachable from current Sit 
% and the corresponding list of Estimates for these situations. Each heuristic 
% estimate on the number of remaining steps is computed using a given heuristic
% function F. Because State is implicitly represented by current situation Sit,
% successors are simply determined by those actions which are possible in Sit.

successors(Sit,State,F,Depth, Successors,Values) :- 
    % find all actions executable in the current state
   getval(precond_time, PrecondTimeOld),
   PrecondStart is cputime,
    findall(A, legal_move(A,State), Actions),
   PrecondEnd is cputime,
   PrecondTime is PrecondEnd - PrecondStart,
   PrecondTimeNew is PrecondTimeOld + PrecondTime,
   setval(precond_time, PrecondTimeNew),
    removeAllDuplicates(Actions,ActionsSorted), !, % this can be done by sort/2
%   sort(Actions, ActionsSorted), !,
    expand(Sit,ActionsSorted,Successors), %compute corresponding Successor nodes
    estimate(Successors,State,F,Depth,Values), !. % and their estimates

%  expand(+SituationNow,+ActionsList, -NextSituationsList).
%  Computes the list of situations that are successors to current situation
%  after taking actions from the given list. Each successor is a list of actions
%  [A | SituationNow] that starts with one of the actions in ActionsList.

expand(_,[],[]).
expand(Sit, [A | Actions], [ [A | Sit] | NextSituations] ) :-
    expand(Sit,Actions,NextSituations).

%  The following can take advantage of domain specific predicate "useless(A,S)"
%  that filters out those actions A that are useless in current situation S.
%expand(S, [A | Actions], Neighbors ) :-
%  expand(S,Actions,NextSits),
%  ( useless(A, S) -> Neighbors = NextSits;  % ignore A, it is useless, continue
%      Neighbors = [ [A | S] | NextSits] ).  % else A is useful, keep it


%  Predicate  estimate(+Successors,+State,+HeurFunc,+Depth, -OtherValues) / 5
%  computes heuristic estimates using given HeurFunc for each of Successors in a
%  given state, where Depth is an upper bound on the number of remaining actions.

estimate([], _,_, _, []).
estimate([NextSit | OtherSuccessors],State,F,Depth, [Value | OtherValues] ) :-
    term_string(State,TempString),     
    term_string(TempState,TempString), 
    h(NextSit,TempState,F,Depth,Value),    
    garbage_collect,                %caution: save on memory, clean up garbage
    estimate(OtherSuccessors,State,F,Depth, OtherValues).


%  The predicate  updateFrontier(+OldPQ,+Successors,+L,+Estimates,-NewPQ) is true
%  if the old frontier stored in the priority queue OldPQ is updated to NewPQ 
%  by inserting into this priority queue all situations from the list of Successors
%  while taking into account the cost of the path L and heuristic Estimates.

% If Successors is empty list, there is nothing to update, return same PQ %
updateFrontier(PQ,[ ],_,[ ],PQ).   

% Take situation S in the head of Successors, and insert it into a priority queue
% with the corresponding f(S) function value that accounts for the length of 
% path (cost g(S) equals the number of actions in situation S) and for an
% heuristic estimate h(S) of the distance to a goal from S. Continue updating 
% the frontier recursively. The order of nodes in Successors does not matter
% since the priority queue makes sure that the situations in the updated 
% frontier are properly sorted according to min of their f-values.

updateFrontier(OldPQ,[S | Successors],L,[H | Estimates], NewPQ) :-
    Fvalue is L + H,
    insertPQ(OldPQ,Fvalue,S,IntermediatePQ),
    updateFrontier(IntermediatePQ,Successors,L,Estimates, NewPQ).

%%------------------------------ list utils ---------------------------%

removeAllDuplicates([],[]).
removeAllDuplicates([A | List],RelevantActions) :- member(A,List),
    removeAllDuplicates(List,RelevantActions).
removeAllDuplicates([A | List],[A | RelevantActions]) :- not member(A,List),
    removeAllDuplicates(List,RelevantActions).
     
%----------------- debug tools -------------------------%

%:- set_flag(progressRelaxed/6,start_tracing,on).
%:- set_flag(progressRelaxed/6,spy,on).

%:- set_flag(estimate/5,start_tracing,on).
%:- set_flag(estimate/5,spy,on).


