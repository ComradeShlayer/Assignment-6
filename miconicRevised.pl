%%% elevators/miconic.pl %%%

:- set_stream(warning_output, null).
%%% Object Types %%%

%subcategories(object,[passenger,my_floor]).
:- discontiguous(origin/2).
:- discontiguous(destin/2).
:- discontiguous(above/2).
:- discontiguous(boarded/2).
:- discontiguous(served/2).
:- discontiguous(lift_at/2).

fluents([ boarded(_,_),served(_,_),lift_at(_,_) ]).
fluent(boarded(_)).   fluent(served(_)).   fluent(lift_at(_)).
/* The planning instances include atomic statements with non-fluent predicates
    above(Fl1,Fl2), origin(Person,Floor),  destin(Person,Floor).
*/
%%% Poss Predicates %%%

poss(board(F,P),[], State):- instantiate([lift_at(F),origin(P,F)],State).
poss(depart(F,P),[], State):- instantiate([lift_at(F),destin(P,F),boarded(P)],State).
poss(up(F1,F2),[], State):- instantiate([lift_at(F1),above(F1,F2)],State).
poss(down(F1,F2),[], State):- instantiate([lift_at(F1),above(F2,F1)],State).

%%% Action Preconditions %%%

actionPrec(board(F,P),[lift_at(F),origin(P,F)]).
actionPrec(depart(F,P),[lift_at(F),destin(P,F),boarded(P)]).
actionPrec(up(F1,F2),[lift_at(F1),above(F1,F2)]).
actionPrec(down(F1,F2),[lift_at(F1),above(F2,F1)]).

%%% Action Parameter Types %%%

actionAuxPrec(board(F,P),[]).
actionAuxPrec(depart(F,P),[]).
actionAuxPrec(up(F1,F2),[]).
actionAuxPrec(down(F1,F2),[]).

%%% Positive Effects %%%

posEffect(board(F,P),State,boarded(P)).
posEffect(depart(F,P),State,served(P)).
posEffect(up(F1,F2),State,lift_at(F2)).
posEffect(down(F1,F2),State,lift_at(F2)).

%%% Negative Effects %%%

negEffect(depart(F,P),State,boarded(P)).
negEffect(up(F1,F2),State,lift_at(F1)).
negEffect(down(F1,F2),State,lift_at(F1)).
