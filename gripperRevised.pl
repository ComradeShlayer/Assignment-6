%%% gripper_strips.pl %%%

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
%  this predicate serve to characterize individual negative effects, i.e., fluents
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

%--------------------------------------------------------------------------
%  In the Grippers domain there are 4 fluents (implemented as terms):
%   at_robby(RobotName,Room) holds if RobotName is currently staying at Room
%   at(ObjectName, Room) holds if ObjectName is currently located at Room
%   free(Robot,Grippername) holds if one of the two GripperName (left or right) 
%                                   is available for Robot to grasp something
%   carry(R,ObjName,Gripper) holds if the robot R is carrying ObjName with Gripper
%
%   There are also 3 action terms:
%   move(R,Loc1,Loc2)   the robot R moves from Loc1 to Loc2
%   pick(R,ObjName,Room,G)  the robot R picks up ObjName in Room with its gripper G
%   drop(R,Obj,Room,G)  the robot R holding Obj with its gripper G drops Obj in Room
%--------------------------------------------------------------------------

:- set_stream(warning_output, null).

:- discontiguous(at_robby/3).
:- discontiguous(at/3).
:- discontiguous(free/3).
:- discontiguous(carry/4).

fluents([at_robby(_,_,_),at(_,_,_),free(_,_,_),carry(_,_,_,_)]).

fluent(at_robby(_,_)).   fluent(at(_,_)).  
fluent(free(_,_)).       fluent(carry(_,_,_)).

%%% Poss Predicates %%%

poss(move(R,From,To),[neg(From = To)], State):- instantiate([at_robby(R,From),room(To)],State).
poss(pick(R,Obj,Room,G),[], State):- instantiate([at(Obj,Room),at_robby(R,Room),free(R,G)],State).
poss(drop(R,Obj,Room,G),[], State):- instantiate([carry(R,Obj,G),at_robby(R,Room)],State).

%%% Action Preconditions %%%

actionPrec(move(R,From,To),[at_robby(R,From)]).
actionPrec(pick(R,Obj,Room,G),[at(Obj,Room),at_robby(R,Room),free(R,G)]).
actionPrec(drop(R,Obj,Room,G),[carry(R,Obj,G),at_robby(R,Room)]).

%%% Action Parameter Types %%%

actionAuxPrec(move(R,From,To),[room(To),neg(From = To)]).
actionAuxPrec(pick(R,Obj,Room,G),[]).
actionAuxPrec(drop(R,Obj,Room,G),[]).

%%% Positive Effects %%%

posEffect(move(R,From,To),State,at_robby(R,To)).
posEffect(pick(R,Obj,Room,G),State,carry(R,Obj,G)).
posEffect(drop(R,Obj,Room,G),State,at(Obj,Room)).
posEffect(drop(R,Obj,Room,G),State,free(R,G)).

%%% Negative Effects %%%

negEffect(move(R,From,To),State,at_robby(R,From)).
negEffect(pick(R,Obj,Room,G),State,at(Obj,Room)).
negEffect(pick(R,Obj,Room,G),State,free(R,G)).
negEffect(drop(R,Obj,Room,G),State,carry(R,Obj,G)).
