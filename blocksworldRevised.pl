%%% blocksworld_bintree/blocksworld.pl %%%

:- set_stream(warning_output, null).
:- discontiguous(clear/2).
:- discontiguous(on_table/2).
:- discontiguous(on/3).

fluents( [clear(_,_),on_table(_,_),on(_,_,_)] ).
fluent(clear(_)).   fluent(on_table(_)).    fluent(on(_,_)).
%fluent(Literal):- member(Literal,[clear(_),on_table(_),on(_,_) ]), !.

%%% Poss Predicates %%%

poss(move_b_to_b(Bm,Bf,Bt),[neg(Bm = Bt)], State):- instantiate([clear(Bm),clear(Bt),on(Bm,Bf)],State).
poss(move_b_to_t(Bm,Bf),[], State):- instantiate([clear(Bm),on(Bm,Bf)],State).
poss(move_t_to_b(Bm,Bt),[neg(Bm = Bt)], State):- instantiate([clear(Bm),clear(Bt),on_table(Bm)],State).

%%% Action Preconditions %%%

actionPrec(move_b_to_b(Bm,Bf,Bt),[clear(Bm),clear(Bt),on(Bm,Bf)]).
actionPrec(move_b_to_t(Bm,Bf),[clear(Bm),on(Bm,Bf)]).
actionPrec(move_t_to_b(Bm,Bt),[clear(Bm),clear(Bt),on_table(Bm)]).

%%% Action Parameter Types %%%

actionAuxPrec(move_b_to_b(Bm,Bf,Bt),[neg(Bm = Bt)]).
actionAuxPrec(move_b_to_t(Bm,Bf),[]).
actionAuxPrec(move_t_to_b(Bm,Bt),[neg(Bm = Bt)]).

%%% Positive Effects %%%

posEffect(move_b_to_b(Bm,Bf,Bt),State,on(Bm,Bt)).
posEffect(move_b_to_b(Bm,Bf,Bt),State,clear(Bf)).
posEffect(move_b_to_t(Bm,Bf),State,on_table(Bm)).
posEffect(move_b_to_t(Bm,Bf),State,clear(Bf)).
posEffect(move_t_to_b(Bm,Bt),State,on(Bm,Bt)).

%%% Negative Effects %%%

negEffect(move_b_to_b(Bm,Bf,Bt),State,clear(Bt)).
negEffect(move_b_to_b(Bm,Bf,Bt),State,on(Bm,Bf)).
negEffect(move_b_to_t(Bm,Bf),State,on(Bm,Bf)).
negEffect(move_t_to_b(Bm,Bt),State,clear(Bt)).
negEffect(move_t_to_b(Bm,Bt),State,on_table(Bm)).

