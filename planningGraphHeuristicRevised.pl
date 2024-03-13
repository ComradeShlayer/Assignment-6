%% planningGraphHeuristicRevised.pl %%
%% Copyright (c) 2024 Mikhail Soutchanski and Ryan Young
%%   Contact email: soutchanski (at) protonmail.com
%% License: this source code is provided under the MIT License as stated at
%%              https://opensource.org/license/MIT
%%
%% May 20, 2022: Original heuristic predicates by Mikhail Soutchanski
%% Modified: additions and debugging by Ryan Young. Last modified: Wedn, March 8, 2023
%% The paper:  Mikhail Soutchanski and Ryan Young  "Planning as Theorem Proving with Heuristics."
%%      https://arxiv.org/abs/2303.13638
%% Revised: January 2024 - modified to work with a new representation of states
%%         as the list of pairs: a fluent name and the list of tuples that hold.
%%
%% This program is provided as a handout for the students enrolled in CPS822/CP8314
%% course "Advanced AI" at the Toronto Metropolitan University. See details at
%%      https://www.cs.torontomu.ca/~mes/courses/822/info.html
%% The students cannot distribute this program without permission of the instructor.
%%
%% --------------------------------------------------------------------------

%:- pragma(debug).
%:- dynamic(stophere/0).
%:- set_flag(stophere/0, spy, on).
%:- set_flag(stophere/0, leash, stop).

%stophere.

%   Motivated by FF heuristic, mutual exclusions are ignored. Read details in
%   Daniel Bryce and Subbarao Kambhampati (Arizona State University)
%   "Planning Graph Based Reachability Heuristics"
%   http://rakaposhi.eas.asu.edu/pg-tutorial/
%   AI Magazine, 2007, Spring, pages 47-55.
%   The special term  pg(Sit,StateInit) initializes the planning graph to
%   currently evaluated situation Sit and initial propositional layer StateInit.
%   Out of all actions that are possible in State, we filter out the actions
%   that were previously possible, but keep only NewActions. The joint positive 
%   effects of these new actions are in the list AddEffs. These result in a new  
%           nextLayer(NewActions,AddEffs,State, PG),
%   where PG is a previously constructed planning graph. We keep AddEffs
%   to subtract easily their intersection with goal literals from the list of 
%   goal literals. This is needed when we count support actions backwards
%   from the completed plan graph that reached a goal state.


%  h(+Sit,+State,+HeurFunc,+Depth, -V)  computes heuristic value V for 
%   given situation Sit that we would like to evaluate using a function
%   HeurFunc (HF) by doing look-ahead search up to Depth from a current State
%   that represents Sit using the list of nodes.
% Depth cannot be exceeded during search. An initial bound Depth is provided by 
% an user. It is decremented with every action taken. Heuristic functions can 
% ignore the current value of Depth, or they can take it into account.
% Heuristic function can possibly be inconsistent or inadmissable. 

% We actually do not calculate heuristic from the initial state
%h([ ],State,graphplan,Depth, Value) :- 
%   planGraphHeur( pg([ ],State),State,Depth,0,[ ], Value).

h([A | Sit],_,graphplan,Depth, Value) :- 
    Depth =:= 0,     %NO depth bound left; should not happen if a solution exists
    length([A | Sit],L),  % find the length of the current situation
    Value is L + 1.   % use it as heuristic estimate to discourage search

h([A | Sit],State,graphplan,Depth, Value) :- 
    Depth > 0,      % search can take up to Depth number of steps
   getval(update_time, UpdateTimeOld),
   UpdateStart is cputime,
    update(State,A,CurrentState),
   UpdateEnd is cputime,
   UpdateTime is UpdateEnd - UpdateStart,
   UpdateTimeNew is UpdateTimeOld + UpdateTime,
   setval(update_time, UpdateTimeNew),
    length([A | Sit],L),    % find the length of the current situation
    %%%  Launch heuristic with this initial layer
    planGraphHeur(pg( [A | Sit],CurrentState), CurrentState,Depth,L,[ ],Value).

% planGraphHeur(+PG,+State,+D, +L,+OldActs, -Val)  means PG is a given planning
%   graph data structure, State is a current state, 
%   search can continue for at least D steps, L is the length of the situation
%   node that is currently evaluated, OldActs are all possible actions that have
%   been consecutively generated after initialization of plan graph, and Val is 
%   the heuristic value to compute. We expand OldActs list after each new action
%   layer. At each propositional layer, when we find all actions possible, we
%   filter out OldActs to keep only the new actions from the next action layer,
%   and then they are inserted into OldActs to update it accordingly.

planGraphHeur(PG,State,Depth,_,_, Value) :- 
    Depth >= 0,
    goal(State),      %last constructed propositional layer is a goal state
    % call the predicate that can trace PG back and count all those actions
    % which are causes for reaching the goal state from where PG has started.
    % Use this count as a PG-based reachability heuristic.
    goalState(GoalList), 
    getval(reach_time, ReachTimeOld),
    ReachStart is cputime,
       computeReachabilityHstc(PG,State,GoalList,Value), !,
    ReachEnd is cputime,
    ReachTime is ReachEnd - ReachStart,
    ReachTimeNew is ReachTimeOld + ReachTime,
    setval(reach_time, ReachTimeNew).

planGraphHeur(_,_,Depth, Length,_, Value) :- 
    Depth =< 0,     % initially Depth >= 1
    % Planning graph construction cannot be completed since the upper bound on
    % the length of plan exceeds the bound. Use the length of current situation 
    % as an heuristic estimate to drive search away.
    Value is Length + 1.
%   experiment with other penalties, e.g., Value is 1.5*(Length + Depth).


planGraphHeur(PG,State,Depth, Length,OldActions, Value) :- 
    Depth > 0,   % can build the next layer of the planning graph data structure
    not  goal(State), % this layer is not yet a goal
   getval(precond_time, PrecondTimeOld),
   PrecondStart is cputime,
    % Find new executable actions, but ignore those in OldActions
    findall(A, (legal_move(A,State), not member(A,OldActions)), AllActions), !,
    % All possible actions include actions that were executable in the previous layers. 
    % Therefore, we remove them and keep only new actions that contribute new add-effects.
   PrecondEnd is cputime,
   PrecondTime is PrecondEnd - PrecondStart,
   PrecondTimeNew is PrecondTimeOld + PrecondTime,
   setval(precond_time, PrecondTimeNew),
%        sort(AllActions, ActionsSorted), %ensure processing of actions in the same order
    removeAllDuplicates(AllActions,ActionsNew), !,  % no duplicates  */
    ( ActionsNew == [ ] -> 
        Value is Length + Depth ;  %No actions are new, not possible to continue
        % Else, there are new possible actions and ActionsNew is not empty.
        % Should not we just max out the heuristic here? - Ryan
        (
       % Filter out actions with add-effects that were already true in State, 
       % i.e., include only NEW actions which added a new positive effect
            evalplanGraphHeur(PG,State,Depth,ActionsNew,OldActions,Length,Value)
        )
    ).
    
%   computeReachabilityHstc(+PG,+State,+GoalList, -Value)
% This predicate is called from planGraphHeur() when State is a goal state 
% and PG can be term pg( [A | Sit],CurrentState) representing initial layer.
% Therefore, we have to consider a few base cases.

% If state of the initial situation is a goal state, then hrst value is 0 
computeReachabilityHstc(pg([ ],InitState),InitState,_,0). %:- stophere.

% If construction of a planning graph starts from a goal state, 
% then heuristic value of evaluated situation is 0 

computeReachabilityHstc(pg(Sit,State),State,_,0) :- 
%   stophere,
    not Sit == [ ].

%   Else we do recursion backwards in a planning graph, and there are 
%   the two base cases. First, all goal literals have been accounted for, 
%   and therefore the remaining count is 0.  

computeReachabilityHstc(pg(_,StateInit),State,[ ],0) :- 
%   stophere,
    not StateInit == State.
    
%   Otherwise, not all goal literals are supported by actions in a relaxed PG,
%   but some goal literals are already true in the state where PG starts.

computeReachabilityHstc(pg(_,StateInit),State,GList,V) :- 
%   stophere,
    not GList == [ ],
    not StateInit == State, 
    % Check how many of literals from GList are not true in StateInit
    howManyAddEffectsAreNew(StateInit,GList,RemainGoals,Total),   
    ( (RemainGoals==[ ], Total=0) -> 
        V is 0 ;
        %Else, this should never happen since there are unaccounted goal literals
        (nl, write("Error in the planning graph heuristic"), nl )
    ).  

%   Recursive rule: go backwards from later layers to earlier layers in PG and
%   count all supporting actions that caused at least one goal literal. This
%   implementation counts supporting actions from the Acts list one-by-one 
%   and removes them individually. This avoids over-counting when several
%   actions cause the same goal literal. Also, if there is an action that
%   achieves more than one goal literal, it is counted only once.

%%% CHANGED: this predicate was counting certain actions multiple times. %%%
%% In the planning graph term nextLayer(ActionList, AddEffects, State, PG),
%% AddEffects was a list of *new* positive effects of the actions in ActionList
%% this means that if an action in ActionList had a positive effect that was already 
%% present in State, this effect would not be included in AddEffects. 
%% To remedy this, I changed the predicate so that it only counts an action as 
%% achieving a goal literal if the goal literal appears in the AddEffects list. - Ryan

%% further changed to look at best supporting actions based on reachability of preconditions

computeReachabilityHstc(nextLayer(Acts,AddEffs,State,PG),_,GList,V) :-
%   stophere,
    % find the goals that were achieved in the most recent set of actions
    commonElems(AddEffs, GList, RelevGoals), !, 
%   getval(support_time, SupportTimeOld),
%   SupportStart is cputime,
% finds supporting actions, and scores them based on reachability of preconditions
    findBestSupportActions(Acts, State, PG, RelevGoals, Causes), !, 
    removeAllDuplicates(Causes, UniqCauses),
    % find preconditions for the supporting actions
    findAllPreconditions(UniqCauses, Preconds), !, 
%   SupportEnd is cputime,
%   SupportTime is SupportEnd - SupportStart,
%   SupportTimeNew is SupportTimeOld + SupportTime,
%   setval(support_time, SupportTimeNew),
    subtractLists(GList,RelevGoals,RemainingGoals), !, 
    % include preconditions into next list of goals to be achieved
    addLists(RemainingGoals, Preconds, NewGoals), !, 
    length(UniqCauses,N),     
    computeReachabilityHstc( PG,State,NewGoals,Count),
    V is N + Count.

%    findBestSupportActions(+Actions,+State,+PlanGraph,+Goals, -Causes) / 5
%  Do recursion over goals. For each goal literal find the easiest support.
%  If an action contributes to more than one goal, and it is the easiest action, 
%  then it may be returned more than once in the list Causes. Therefore,
%  duplicates have to be later removed.

findBestSupportActions(_, _, _, [], []).
findBestSupportActions(Acts, State, PG, [G|Gs], [A|As]) :-
    findBestSupportActions(Acts, State, PG, Gs, As),
    %Choose from Acts those actions which achieve G
    findRelevantActions(Acts, State, G, RelActs),
    %Select the easiest action A from RelActs: ArgMin over cost of achieving preconditions
    findBestSupportActionHelper(RelActs, State, PG, A).

%    findBestSupportActionHelper(+Acts, +State, +PG, -Best)
%  The 1st argument in Acts includes at least 1 element. Do recursion over 
%  list Acts. If is has the single element, then it is returned as an output.

findBestSupportActionHelper([A|As], State, PG, Best) :-
    actionPrec(A, Preconds),
    computeReachabilityHstc(PG, State, Preconds, Cost),
    compareSupportActions(As, State, PG, A, Cost, Best).


%     compareSupportActions(+Acts, +State, +PG, +A, +Acost, -Best) / 6
% Compare actions from the given list Acts with each other and with given 
% action A that has Acost to find Best action with the minimal cost.

compareSupportActions([], _, _, Best, _, Best) :- !.
compareSupportActions([A|As], State, PG, CurrBest, CurrMinCost, Best):-
    actionPrec(A, Preconds),
    computeReachabilityHstc(PG, State, Preconds, Cost),
    ( Cost < CurrMinCost ->
        ( NewBest = A, NewMinCost = Cost ) ;
        ( NewBest = CurrBest, NewMinCost = CurrMinCost )
    ),
    compareSupportActions(As, State, PG, NewBest, NewMinCost, Best).

%    findAllPreconditions(+Actions, -PrecondListActions)
%  Compute a combined list of all preconditions for the given Actions.

findAllPreconditions([],[]).

findAllPreconditions([A|Actions], TotalPrecondList):-
    actionPrec(A, PrecondListA),
    findAllPreconditions(Actions, PrecondListActions),
    addLists(PrecondListA, PrecondListActions, TotalPrecondList).

%   findRelevantActions(+Actions, +State, +G, -RelevActs)
%  In the given list of Actions find those RelevActs which produce a goal G 
%  as their add-effect 

findRelevantActions([], _, _, []):- !.
findRelevantActions([A|As], State, G, Rel):-
    findRelevantActions(As, State, G, Partial),
    findall(P, posEffect(A, State, P), PosEffs),
    ( member(G, PosEffs) ->
        Rel = [A|Partial] ;
        Rel = Partial
    ).

% findSupportActions(+Acts,+State,+GList, -Causes) computes a list Causes that
% contains actions from a given list Acts with at least one positive effect
% that belongs to GList

findSupportActions([ ],_,_,[ ]).

findSupportActions([ A |OtherActions], State, GoalList, Causes) :-
    findall(P, posEffect(A,State,P), PosEffs),
    commonElems(PosEffs, GoalList, Common),
    subtractLists(GoalList, Common, RemainingGoals),
    findSupportActions(OtherActions, State, RemainingGoals, OtherCauses),
    ( Common = [] ->
        Causes = OtherCauses ;
        Causes = [A|OtherCauses]
    ).


%  In a planning graph, we imagine doing at once all possible actions from 
%   the current action layer. Adding their new positive effects altogether
%   in any order leads to the next propositional layer of the planning graph.

    %  The following new version delivers about 50% improvement over BW %
    %  It filters out non-relevant add-effects that were previously true
    
evalplanGraphHeur( PG, State,Depth,NewActions,OldActs,L, V) :- 
    % ignore delete effects as usual in plan graph heuristic
    progressRelaxed(State,NewActions, NewRelevActs, NextState, NewAddEff, CountAdd),  
    (CountAdd =:= 0 -> 
        V is L;  % nothing is new in a propositional layer, i.e. reached the
        % fixed point. New state is same as before but did not reach a goal yet.
        % Is it really a good idea to approximate hrst V as L or as Depth here?
        % Try stronger penalties, e.g., V is 1.5*(L+Depth) to drive search away.
        % Shouldnt CountAdd = 0 only arise when it is impossible to find a plan?
            % i.e. we arent at a goal, or else we wouldnt have made the evalGraphPlanHeur call
            % and every possible action we can take does not get us further towards the goal
            % so this branch should be abandoned
        ( 
            D is Depth - 1, 
            append(OldActs,NewActions,UpdatedOld),
            planGraphHeur( nextLayer(NewRelevActs,NewAddEff,State,PG), NextState,D,L,UpdatedOld,V)
        ) 
    ).

% progressRelaxed(+State,+NewActs, -RelevActs,-NextState,-NewAddEff,-CountAdd) / 6
% RelevActs ate those actions from the list NewActs which produce a new add-effect
%   that was not already true in State. We add RelevActs only to the next action layer,
%   we update State with their relevant add-effects NewAddEff to produce NextState,
%   and compute CountAdd as the number of new add affects.

progressRelaxed(State,[],  _, State, [ ], 0).

progressRelaxed(State,[A | Actions], RelevActs, NextState, RelevAddEffects, TotalNew) :-
    findAllPositiveEffects(State,[A | Actions],[ListAddA | ListAddActions] ), 
    filterActionEffects([A | Actions],[ListAddA | ListAddActions],State, RelevActs, TempRelevAddEffects, TotalNew),
      removeAllDuplicates(TempRelevAddEffects,RelevAddEffects), 
    % add new positive effects only
   getval(update_time, UpdateTimeOld),
   UpdateStart is cputime,
   % adding positive effects is an operation that modifies State into NextState
     addEffects(State,RelevAddEffects,NextState), !,
   UpdateEnd is cputime,
   UpdateTime is UpdateEnd - UpdateStart,
   UpdateTimeNew is UpdateTimeOld + UpdateTime,
   setval(update_time, UpdateTimeNew).
   

%findAllPositiveEffects(+State,+Acts, -TotalAddList) 
%  recursively builds the list of lists with add-effects for all actions in Acts

% in delete relaxation only positive effects count
findAllPositiveEffects(_,[], []).

findAllPositiveEffects(State,[A | OtherActions], [AddListA | AddListActions]) :-
    findall(P, posEffect(A,State,P), AddListA),
    findAllPositiveEffects(State,OtherActions, AddListActions).

% addEffects(State, ListPositiveEffects, NewState) takes the current state
%       and modifies it into a NewState by inserting each ground fluent from
%       ListPositiveEffects into the list of ground fluents corresponding to 
%       the fluent name in the list of nodes. Recall there is one node per fluent name.
%       This predicate is implemented in situation2StateUpdate.pl


% filterActionEffects(+NewActions,+AllAddEffects,+State, -Relevant,-TotalAddEff,-Total) / 6
% This predicate takes new actions, all their add effects, and current State 
% as its inputs. It returns only Relevant actions that actually contribute 
% new effects to State, the list of their cumulative TotalAddEff and 
% the count Total of the number of relevant actions.

filterActionEffects([ ],[ ], _,  [ ],[ ],0).

filterActionEffects([Act | Actions],[ActAddL | ActionsAddL],State, Relevant,NewAddEff,Total):-
    filterActionEffects(Actions,ActionsAddL,State, Partial,ActionsRelAddEff,ActionsCount),
    howManyAddEffectsAreNew(State,ActAddL, ActRelEffects, ActNumb),
    ( ActNumb =:= 0 ->
        (Relevant = Partial, NewAddEff=ActionsRelAddEff, Total is ActionsCount) ;
        (Relevant = [Act | Partial], append(ActRelEffects,ActionsRelAddEff,NewAddEff), Total is ActNumb + ActionsCount)
    ).


howManyAddEffectsAreNew(_,[ ],  [ ],0).

howManyAddEffectsAreNew(State,[Fluent | List], ListAddEffects,Total) :-
    howManyAddEffectsAreNew(State,List,NewAddEffectsList,Number),  
    ( evaluate([Fluent], State) -> 
         ( ListAddEffects=NewAddEffectsList, Total is Number ) ;
         ( ListAddEffects= [Fluent | NewAddEffectsList], Total is Number + 1)
    ).

/*----------------------  UTILS --------------------------------*/

%% addLists(+L1, +L2, -L3)
%% L1 and L2 should not contain duplicates
%%      if this requirement is met, L3 will not contain duplicates
addLists([], L2, L2):- !.
addLists(L1, [], L1):- !.
addLists([H|L1], L2, L3):-
    addLists(L1, L2, Partial),
    ( member(H, L2) ->
        L3 = Partial ;
        L3 = [H|Partial]
    ).

%% subtractLists(+L1, +L2, -L3)
%% L3 contains all elements of L1 not present in L2
%% L1 and L2 should not contain duplicates
%%      if this requirement is met, L3 will not contain duplicates
subtractLists([], _, []):- !.
subtractLists(L1, [], L1):- !.
subtractLists([H|L1], L2, L3):-
    subtractLists(L1, L2, Partial),
    ( member(H, L2) ->
        L3 = Partial ;
        L3 = [H|Partial]
    ).

%% commonElems(+L1, +L2, -Common)
%% Common contains all the elements present in both L1 and L2
commonElems([], _, []):- !.
commonElems(_, [], []):- !.
commonElems([H|L1], L2, Common):-
    commonElems(L1, L2, Partial),
    ( member(H, L2) ->
        Common = [H|Partial] ;
        Common = Partial
    ).

