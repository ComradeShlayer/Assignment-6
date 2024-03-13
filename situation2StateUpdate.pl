%% situation2StateUpdate.pl %%
%% Copyright (c) 2024 Mikhail Soutchanski 
%% Jan 19, 2024 Author: Mikhail Soutchanski  Email: soutchanski (at) protonmail (dot) com
%% This software is available with the MIT Licence  https://opensource.org/license/MIT
%%
%% This is a revised version of program binTree.pl from January 2018. This version does not
%% require that the universe of objects is subject to the Domain Closure Assumption (DCA).
%% In this version the arguments of fluents must be constants, but not arbitrary terms.
%%
%% This program is provided as a handout for the students enrolled in CPS822/CP8314
%% course "Advanced AI" at the Toronto Metropolitan University. See details at
%%      https://www.cs.torontomu.ca/~mes/courses/822/info.html
%% 
%% --------------------------------------------------------------------------

%:- pragma(debug).  %:- [preconditions]. %:- [posEffects]. %:- [negEffects].
% Caution: "neg" is used in a few ECLiPSe 6.1 libraries. Do not load them to avoid conflicts.            
% "neg" is used in this program to represent classical negation operator.
 
%  Each domain must include a set of rules describing positive effects, i.e.,
%     fluents that become true after doing an action, a set of rules describing
%     negative effects, i.e., fluents that become false, and a set of rules
%     describing preconditions: one rule per action.    
%  The following syntax is required.     
%  - poss(+Act,+ListNegativePreconditions,+State) where Act is an action term with 
%  uninstantiate object variables, ListNegativePreconditions is a possible empty
%  list of conditions that must be false, and State is a current state that
%  corresponds to the current situation. The typical usage is
%       poss(Act,[ ], State) :- instantiate(ListPositivePreconditions], State).
%  this allows to ground action arguments at run-time according to fluents
%  that hold in State.
%  - posEffect(Act,State,FluentAsTerm) where Act is a ground action that was
%  executed in State, and FluentAsTerm represents a single effect of action.
%  In this implementation all fluents are reified as terms that may include
%  only object arguments, but no situation argument. For each fluent that becomes
%  true in the result of executing Act, there is a separate atomic statement
%  about this effect.
%  - negEffect(Act,State,FluentAsTerm) is similar but atomic statements with
%  this predicate serve to characterize invidual negative effects, i.e., fluents
%  that become false in the next state that results from executing Act in State.
%  - actionPrec(Act,ListPositivePrecondition) - similar to poss(A,NegPrec,State),
%  but these atomic statements list only positive preconditions of actions.
%
%  The initial state is characterized by atomic statements about the fluents
%  that hold in the initial situation represented by the empty list. The domain
%  file must include atomic statements fluent(Name) for each fluent term Name
%  mentioned in the domain. Also, the atomic statement  fluents(ListOfFluents) 
%  characterizes all fluent terms present in the planning domain. All other
%  auxiliary predicates that can be present in the domain are not fluents.
 
/*  
% This is a baseline planner similar to the planner described in Chapter 9 of
% Hector Levesque's textbook "Thinking as Computation".
% solve_problem(Bound,Plan)  :-  
%     C0 is cputime,
%     max_length(Plan,Bound),
%     reachable(State,Plan),
%     goal(State),
%     Cf is cputime, D is Cf - C0, nl,
%     write('Elapsed time (sec): '), write(D), nl.
% 
% max_length([],N) :- N >= 0.
% max_length([First | L],N1) :- N1 > 0, N is N1 - 1, max_length(L,N).
% 
% reachable(S,[]) :- initial_state(S). 
% reachable(NextState, [M | ListOfActions]) :- 
%     reachable(State,ListOfActions),
%     legal_move(M,State),
%     %  The next line is optional if "useless" is implemented %
%     %  not useless(M,ListOfActions),
%     % NextState is progression of State through action M
%     update(State,M,NextState).
% 
% legal_move(A, State) :- 
% poss(A, PrecondList, State), %!,     % each action has only one list of preconditions
% evaluate(PrecondList,State). %! % If preconditions fail, there is no need to update
% 
% The (unique) initial situation is translated into a list of nodes where each node
% consists of a fluent name and the list of tuples that hold in the current state.
% initial_state(State) :- buildNodesList(State), !.
% goal(State) :- goalState(List), !, evaluate(List,State). % check if all goal literals hold in State
*/
 
update(State,A,NextState) :- 
    findall(P, posEffect(A,State,P), AddList),%
    findall(N, negEffect(A,State,N), DeleteList),%
    addEffects(State,AddList,TempState),
    delEffects(TempState,DeleteList,NextState),
    % Since update modifies State desructively:
    ! . % added a cut for timing purposes, no need to backtrack

% Repeatedly match the input fluents with what holds in the current state to
% instantiate at run-time the variables in fluents with correct object arguments.
instantiate([], _).
instantiate([ Literal | Tail], State) :- 
    groundLiteral(Literal, State),
    instantiate(Tail,State).

% groundLiteral calls inVar responsible for instantiation of a fluent term
% with variables. inVar does exhaustive search in the underlying data structure 
% to try matching all currently true fluents. Otherwise, if the input argument
% is not a fluent, then try to match with the corresponding atomic statements.

groundLiteral(Literal, State) :-
    fluent(Literal) ->
    (    Literal =.. [FluentName | Args],  inVar(FluentName,Args,State) );
    Literal.

% evaluate(+LiteralsList,+State): used to evaluate ground literals in goal, and 
% to ground action arguments. The latter usage is inefficient and can be improved.
 
evaluate([], _).
evaluate([ neg(Literal) | Tail], State) :- 
    not checkLiteral(Literal, State),  %%% !
    evaluate(Tail,State). %%% !.
evaluate([ Literal | Tail], State) :-  
    Literal \= neg(_),
    checkLiteral(Literal, State), %%% !, %
    evaluate(Tail,State). %%% !.

%% checkLiteral %%
checkLiteral(Literal, State) :-
    fluent(Literal) -> 
        ( Literal =.. [FluentName | Args], checkVar(FluentName,Args,State) ) ;
    Literal.

%% checkVar: check if object arguments of a fluent are variables or not.
%% If yes, then instantiate them using inVar() by matching with State.
%% Otherwise, if arguments are constants, then check using in() whether the tuple
%% of arguments is in a list of the tuples that hold in the current State.
%% Note that FluentName is only the name of a fluent without any object arguments.
checkVar(FluentName,[Arg1 | OtherArgs],State) :-
    var(Arg1),
    inVar(FluentName,[Arg1 | OtherArgs],State).
checkVar(FluentName,[Arg1 | OtherArgs],State) :-
    nonvar(Arg1),
    in(FluentName,[Arg1 | OtherArgs],State).

in(FluentName, Args, [node(Element,ListOfTuples) | OtherNodes] ) :- 
    Element \== FluentName, !,
    in(FluentName, Args, OtherNodes).

in(FluentName, Args, [node(Element,ListOfTuples) | OtherNodes] ) :- 
    Element == FluentName, !, % do not futher traverse list if fluent is not true
        %Since we assume Literal is a ground fluent, we check whether it occurs
    Literal =.. [ FluentName | Args],
    member(Literal,ListOfTuples).  /* depends on the underlying data structure*/
 
 
% inVar(Literal,Args, [node(El,ListTuples) | Other]) is used to instantiate the
%     variables in arguments Args of the given Literal with the domain elements. 
%     Since each node is the pair of a fluent name and a list of instances
%     in the current state where this fluent is true instantiation is 
%     achieved by unifying Literal repeatedly with members in the list. 
%     This linear search cannot be accelarated since in the worst case 
%     we have to try all possible instantiations when solving the CSP related
%     to precondition of an action that we are trying to ground at run-time.

inVar(FluentName, Args, [node(Name,_) | OtherNodes] ) :- 
    FluentName \== Name,        % current node mentions a different fluent
    inVar(FluentName, Args, OtherNodes ).
    
inVar(FluentName, Args, [node(Name,ListOfTuples) | _] ) :- 
    FluentName == Name,         % current node matches an input fluent name
    Literal =.. [ FluentName | Args],
    member(Literal,ListOfTuples).  /*depends on the underlying data structure*/ 


% addEffects(+State,+ListPositiveEffects, -NextState) takes the current state 
%      and produces a new NextState by inserting each ground fluent from
%      ListPositiveEffects into the list of tuples corresponding to the fluent.

addEffects(State, [ ], State).
addEffects(InputState,[F | OtherEffects],OutputState) :- 
    fluent(F),
    F =.. [Name | Args],       %fluent(F) is true, add its tuple to node for F
    insertEffect(InputState,Name,Args,IntermState),
    addEffects(IntermState,OtherEffects,OutputState).

% delEffects(+State,+ListNegativeEffects,-NewState) takes the current state
%        and produces NewState by removing each ground fluent listed in
%        ListNegativeEffects from the list corresponding to the fluent name

delEffects(State,[ ],State).
delEffects(InputState,[F | OtherEffects],OutputState ) :- 
    fluent(F),
    F =.. [Name | Args],  % fluent(F) no longer holds, remove its tuple
    removeEffect(InputState,Name,Args,IntermState),
    delEffects(IntermState,OtherEffects,OutputState).

%% insertEffect: insert a single positive effect in the correct node %%
insertEffect( [node(Name,List) | InputNodes], FluentName, Args, [node(Name,List) | OutputNodes] ) :-
    FluentName \== Name, !, 
    insertEffect(InputNodes,FluentName,Args,OutputNodes).

%% Use the library predicate subtract(+L1,+L2,-Remainder) for efficiency?
insertEffect([node(Name,InTuples) | OtherNodes], FluentName, Args, [node(Name,OutTuples) | OtherNodes] ) :- 
        FluentName == Name, !, 
        Literal =.. [ FluentName | Args],
        ( member(Literal,InTuples) -> OutTuples=InTuples ; % Literal already occurs there
        OutTuples = [Literal | InTuples] ). % Otherwise, insert new tuple in this node

%% removeEffect: removes a single occurrence of fluent from its list of tuples %%
removeEffect( [node(Name,List) | InputNodes], FluentName, Args, [node(Name,List) | OutputNodes] ) :-
    FluentName \== Name, !, 
    removeEffect(InputNodes,FluentName,Args,OutputNodes).

%% Use the library predicate subtract(+L1,+L2,-Remainder). If elements of L2 do not
%% occur in L1, then Remainder is same as L1. This is a deterministic predicate.
removeEffect( [node(Name,InTuples) | OtherNodes], FluentName, Args, [node(Name,OutTuples) | OtherNodes] ) :- 
        FluentName == Name, !, 
        Literal =.. [ FluentName | Args],
        subtract(InTuples,[Literal],OutTuples). 

%% Build a List of Nodes Representation given fluents describing the initial situation.
buildNodesList(State) :-
    getFluents(ListFluents,NFluents,State), !, % printList(State),
    nl .


%% getFluents %%
getFluents(AllFluents,NumbFluentNames,ListFluentNodes) :-
    fluents(Fluents), 
    length(Fluents,NumbFluentNames),
    %maxArity(Fluents,Max), Arity1 is Max - 1, write("Max arity is "), write(Arity1), nl,
    %minArity(Fluents,Min), Arity2 is Min - 1, write("Min arity is "), write(Arity2), nl,
    getListNodes(Fluents, ListFluentNodes).

maxArity([F], X) :- arity(F, X), !.
maxArity([ F | OtherFluents], Max) :-
    arity(F, Number1),
    maxArity(OtherFluents, Number2),
    max(Number1,Number2,Max).

minArity([F], X) :- arity(F, X), !.
minArity([ F | OtherFluents], Min) :-
    arity(F, Number1),
    minArity(OtherFluents, Number2),
    min(Number1,Number2,Result),
    (Result =< 0 -> Min is 1 ; Min is Result).

%% getListNodes: convert all initial atomic statements into the list of nodes, %%
%% where each node is a pair that consists of a fluent name followed by the list
%% of initial instances, but withiout the last argument - the the empty list.

getListNodes([], []).

% The case when a fluent is mentioned at least once in the initial theory
getListNodes([H|T], [node(Name,ListFluentInstances) | ListOtherFluents] ) :-
    H =.. [Name| Tail],
    length(Tail,Arity), % Arity is the number of arguments in a fluent term
    current_predicate(Name/Arity),    % initial theory mentiones this fluent
    findall(H, H, FluentInitSitInstances),  % collect all initial statements about Name
    removeSit(FluentInitSitInstances, ListFluentInstances),
    getListNodes(T, ListOtherFluents).

% The case when a fluent is never mentioned in the initial theory.
% Create a node for this fluent, but use the empty list for instances.
getListNodes([H|T], [node(Name, [ ]) | ListOtherFluents] ) :-
    H =.. [Name| Tail],
    length(Tail,Arity), 
    not ( current_predicate(Name/Arity) ), %fluent with Name does not hold initially
    getListNodes(T, ListOtherFluents).

%    removeSit(+ListWithInitSit, -ListWithoutInitSit)
% ListWithInitSit is the list of all fluents true initially, where
%    each fluent has initial situation as the last argument.
%    The predicate removeSit(InpList,OutList) takes InpList
%    of fluents about initial situation and produces OutList
%    of same fluents but without the initial situation.

removeSit([],[]).
removeSit( [H | T], [FluentTerm | NewT]) :-
    H =.. [Name | Args],
    append(ArgsWithoutInitiSit, [_], Args), !, 
    FluentTerm =.. [Name | ArgsWithoutInitiSit],
    removeSit(T, NewT).

%% printList %%
printList([]) :- write(" ").
printList([Last]) :- write(Last), write(" ").
printList([ H1, H2 | T]) :- write(H1), write(", "), printList([H2 | T]).

%:- set_flag(buildNodesList/1,start_tracing,on).
%:- set_flag(buildNodesList/1,spy,on).
