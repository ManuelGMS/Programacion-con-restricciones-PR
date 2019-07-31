% BEGIN LICENSE BLOCK
% Version: CMPL 1.1
%
% The contents of this file are subject to the Cisco-style Mozilla Public
% License Version 1.1 (the "License"); you may not use this file except
% in compliance with the License.  You may obtain a copy of the License
% at www.eclipse-clp.org/license.
% 
% Software distributed under the License is distributed on an "AS IS"
% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.  See
% the License for the specific language governing rights and limitations
% under the License. 
% 
% The Original Code is  CPViz Constraint Visualization System
% The Initial Developer of the Original Code is  Helmut Simonis
% Portions created by the Initial Developer are
% Copyright (C) 2009-2010 Helmut Simonis
% 
% Contributor(s): 	Helmut Simonis, 4C, Univerity College Cork, Cork
%			
% 
% END LICENSE BLOCK
% ----------------------------------------------------------------------
:-module(bin_packing).

:-export(top/0).
:-export(bin_packing_alone/3).
:-export(bin_packing_alone/4).

:-lib(ic).
:-lib(ic_global).
:-lib(hash).

%:-use_module(gcc).


top:-
        hash_create(Hash),
        no_sum(Hash,[10,10,10,9,9,9,9,2,1],69,9,34,35,Result1,_,_),
        writeln(Result1),
        no_sum(Hash,[2, 2, 2, 2, 2, 2, 2, 2, 2, 2], 20, 10, 3, 4,Result2,_,_),
        writeln(Result2),
        abort,
        length(Items,6),
        Items :: 1..4,
        Sizes = [5,4,3,3,2,1],
        bin_packing(Items,Sizes,4,5),
        search(Items,0,input_order,indomain,complete,[]),
        writeln(Items).

bin_packing_alone(Items,Sizes,N,BinSize):-
        length(Bins,N),
        Bins :: 0..BinSize,
        bin_packing_alone(Items,Sizes,Bins).

bin_packing_alone(Items,Sizes,Bins):-
        collection_to_list(Items,L),
        collection_to_list(Sizes,S),
        collection_to_list(Bins,B),
        sum(S,Total),
        sumlist(B,Total),
        length(L,NrVars),
        length(B,NrBins),
        combine(L,S,ItemTerms),
        sort(2,>=,ItemTerms,Sorted),
%        writeln(Sorted),
        bin_packing01(L,S,B,NrVars,NrBins),
        bin_packing(L,S,B,Total,NrVars,NrBins,Sorted).

bin_packing01(L,S,B,NrVars,NrBins):-
        dim(Boolean,[NrVars,NrBins]),
        Boolean[1..NrVars,1..NrBins] :: 0..1,
        (foreach(X,L),
         count(I,1,NrVars),
         param(Boolean,NrBins) do
            bool_channeling(X,Boolean[I,1..NrBins],1)
        ),
        (foreach(Y,B),
         count(J,1,NrBins),
         param(Boolean,NrVars,S) do
            (for(I,1,NrVars),
             foreach(Size,S),
             fromto(0,A,A+Size*X,Term),
             param(Boolean,J) do
                subscript(Boolean,[I,J],X)
            ),
            eval(Term) #= Y
        ).


bin_packing(L,_S,B,_Total,_NrVars,NrBins,Items):-
        hash_create(Hash),
        check_bin_packing(B,NrBins,Items,Hash),
        append(L,B,AllVars),
        (ground(AllVars) ->
            true
        ;
            suspend(update_bin_packing(AllVars,B,
                                       NrBins,Items,Hash,Susp),
                    5,[AllVars->inst,
                       AllVars->ic:min,
                       AllVars->ic:max,
                       L->ic:hole],Susp)
        ).

:-demon(update_bin_packing/6).
update_bin_packing(AllVars,B,NrBins,Items,Hash,Susp):-
        check_bin_packing(B,NrBins,Items,Hash),
        (ground(AllVars) ->
            kill_suspension(Susp)
        ;
            true
        ).

check_bin_packing(Bins,NrBins,Items,Hash):-
        l2_limit(Bins,NrBins,Items),
        no_sum_reasoning(Bins,1,Items,Hash),
        true.

no_sum_reasoning([],_J,_Items,_).
no_sum_reasoning([Bin|Bins],J,Items,Hash):-
        get_bounds(Bin,BMin,BMax),
        candidates(Items,J,Variables,Candidates,0,_Possible,0,Fixed),
        %writeln(cand(J,BMin,BMax,Candidates,Fixed)),
        (Candidates = [] ->
            true
        ;
            Alpha is BMin - Fixed,
            Beta is BMax-Fixed,
            sum_cand(Candidates,0,Sum,0,N),
            no_sum(Hash,Candidates,Sum,N,Alpha,Beta,Result,_Alpha1,_Beta1),
            (Result = infeasible ->
                fail
            ;
                true
            ),
            update_lower_bound(Candidates,Sum,N,Fixed,BMin,Bin,Hash),
            update_upper_bound(Candidates,Sum,N,Fixed,BMax,Bin,Hash),
            N1 is N-1,
            internal_shave(Variables,Candidates,[],Sum,N1,Alpha,Beta,J,Hash)
        ),
        J1 is J+1,
        no_sum_reasoning(Bins,J1,Items,Hash).

internal_shave([],[],_Previous,_Sum,_N1,_Alpha,_Beta,_J,_Hash).
internal_shave([Var|Variables],[C|Candidates],Previous,Sum,N1,
               Alpha,Beta,J,Hash):-
        (is_in_domain(J,Var) ->
            append(Previous,Candidates,Current),
            Sum1 is Sum-C,
            %        writeln(current(Sum1,Current)),
            Alpha1 is Alpha-C,
            Beta1 is Beta-C,
            no_sum(Hash,Current,Sum1,N1,Alpha1,Beta1,Result,_,_),
            (Result = infeasible ->
                %            writeln(removed(Var,J)),
                Var #\= J
            ;
                true
            ),
            no_sum(Hash,Current,Sum1,N1,Alpha,Beta,Result2,_,_),
            (Result2 = infeasible ->
                %            writeln(fix(Var,J)),
                Var = J
            ;
                true
            )
        ;
            true
        ),
        append(Previous,[C],Previous1),
        internal_shave(Variables,Candidates,Previous1,Sum,N1,
                       Alpha,Beta,J,Hash).

update_lower_bound(Candidates,Sum,N,Fixed,BMin,Bin,Hash):-
        Alpha is BMin - Fixed,
        Beta is BMin-Fixed,
        no_sum(Hash,Candidates,Sum,N,Alpha,Beta,Result,_Alpha1,Beta1),
        (Result = infeasible ->
%            writeln(update_lower(Bin,Fixed,Beta1)),
            Bin #>=Fixed+Beta1
        ;
            true
        ).

update_upper_bound(Candidates,Sum,N,Fixed,BMax,Bin,Hash):-
        Alpha is BMax - Fixed,
        Beta is BMax-Fixed,
        no_sum(Hash,Candidates,Sum,N,Alpha,Beta,Result,Alpha1,_Beta1),
        (Result = infeasible ->
%            writeln(update_upper(Bin,Fixed,Alpha1)),
            Bin #=< Fixed+Alpha1
        ;
            true
        ).

no_sum(_Hash,_Candidates,Sum,_N,Alpha,Beta,feasible,nan,nan):-
        Alpha =< 0,
        Beta >= Sum,
        !.
/*
no_sum(_Hash,[],Sum,N,Alpha,Beta,feasible,nan,nan):-
        !,
        writeln(no_sum_empty(Sum,N,Alpha,Beta)),  
        abort.

*/
no_sum(Hash,Candidates,Sum,N,Alpha,Beta,Result,Alpha1,Beta1):-
        hash_find(Hash,key(Alpha,Beta,N,Sum,Candidates),
                  result(Result,Alpha1,Beta1)),
        !.
no_sum(Hash,Candidates,Sum,N,Alpha,Beta,Result,Alpha1,Beta1):-
        Array =.. [[]|Candidates],
        writeq(no_sum(Candidates,Sum,N,Alpha,Beta)),nl,
        set_c(Array,N,Alpha,0,K1,0,SumC),
        writeln(c(K1,SumC)),
        subscript(Array,[N-K1],SumB),
        lp(0,SumA,SumB,SumC,0,K1,Array,N,Alpha,Beta,Alpha1,Beta1),
        (SumA < Alpha ->
            Result = infeasible
        ;
            Result = feasible
        ),
        hash_add(Hash,key(Alpha,Beta,N,Sum,Candidates),
                  result(Result,Alpha1,Beta1)).


lp(SumA,SumAEnd,SumB,SumC,K,K1,Array,N,Alpha,Beta,Alpha1,Beta1):-
        SumA < Alpha,
        SumB =< Beta,
        !,
        Ka is K+1,
        subscript(Array,[Ka],Size),
        SumA1 is SumA+Size,
        (SumA1 < Alpha ->
            K1a is K1-1,
            SumB1 is SumB+Array[N-K1a],
            SumC1 is SumC-Array[N-K1a],
            lp1(SumA1,SumB1,SumB2,SumC1,SumC2,K1a,K1b,Array,N,Alpha,Ka)

        ;
            SumB2 = SumB,
            SumC2 = SumC,
            K1b = K1
        ),
%        writeln(k(Ka,K1b)),
        lp(SumA1,SumAEnd,SumB2,SumC2,Ka,K1b,Array,N,Alpha,Beta,Alpha1,Beta1).
lp(SumA,SumA,SumB,SumC,_,_,_,_,_,_,Alpha1,SumB):-
        Alpha1 is SumA+SumC.

lp1(SumA,SumB,SumBEnd,SumC,SumCEnd,K1,K1End,Array,N,Alpha,K):-
        SumA+SumC >= Alpha,
        !,
        K1a is K1-1,
        SumC1 is SumC-Array[N-K1a],
        SumB1 is SumB+Array[N-K1a]-Array[N-K1a-K-1],
        lp1(SumA,SumB1,SumBEnd,SumC1,SumCEnd,K1a,K1End,Array,N,Alpha,K).
lp1(_SumA,SumB,SumB,SumC,SumC,K1,K1,_Array,_N,_Alpha,_K).

set_c(Array,N,Alpha,K1,Kend,SumC,SumEnd):-
        I is N-K1,
        (I > 0 ->
            true
        ;
            writeln(i_zero(Array,N,Alpha,K1,SumC)),
            abort
        ),
        arg(I,Array,Size),
        SumC1 is SumC+Size,
        SumC1 < Alpha,
        !,
        K1a is K1+1,
        set_c(Array,N,Alpha,K1a,Kend,SumC1,SumEnd).
set_c(_Array,_N,_Alpha,K1,K1,SumC,SumC).

        

sum_cand([],S,S,N,N).
sum_cand([Size|C1],S,Send,N,Nend):-
        S1 is S+Size,
        N1 is N+1,
        sum_cand(C1,S1,Send,N1,Nend).

candidates([],_,[],[],P,P,F,F).
candidates([item(X,Size)|Items],J,Vars,Cand,P,PEnd,F,FEnd):-
        integer(X),
        !,
        (X == J ->
            F1 is F+Size,
            P1 is P+Size
        ;
            F1 = F,
            P1 = P
        ),
        candidates(Items,J,Vars,Cand,P1,PEnd,F1,FEnd).
candidates([item(X,Size)|Items],J,[X|Vars],[Size|Cand],
           P,PEnd,F,FEnd):-
        is_in_domain(J,X),
        !,
        P1 is P+Size,
        candidates(Items,J,Vars,Cand,P1,PEnd,F,FEnd).
candidates([_|Items],J,Vars,Cand,P,PEnd,F,FEnd):-
        candidates(Items,J,Vars,Cand,P,PEnd,F,FEnd).

l2_limit(Bins,NrBins,Items):-
        max_capacity(Bins,C),
        local(array(fixed_value(NrBins))),        
%        writeln(c(C)),
        (foreach(X,Bins),
         count(J,0,_),
         param(C) do
            get_max(X,Max),
            C1 is C-Max, 
            setval(fixed_value(J),C1)
        ),
        fixed_non_fixed(Items,Fixed,NonFixed),
%        writeln(fixed(Fixed)),
%        writeln(nonfixed(NonFixed)),
        (foreach(item(Fix,Size),Fixed) do
            Fix1 is Fix-1,
            getval(fixed_value(Fix1),Old),
            New is Old+Size,
            setval(fixed_value(Fix1),New)
        ),
        (for(J,0,NrBins-1),
         fromto([],A,A1,FixedItems) do
            getval(fixed_value(J),Size),
            (Size > 0 ->
                A1 = [Size|A]
            ;
                A1 = A
            )
        ),
%        writeln(fixed(FixedItems)),
        sort(0,>=,FixedItems,FixedSorted),
        merge(0,>=,FixedSorted,NonFixed,SortedSizes),
%        writeln(decr(SortedSizes)),
        erase_array(fixed_value/1),
        l2_bound(SortedSizes,C,L2),
        L2 =< NrBins. % may fail

l2_bound(SortedSizes,C,L2):-
        CHalf is C/2,
        CC is integer(floor(CHalf)),
        (for(K,0,CC),
         fromto(0,A,A1,L2),
         param(SortedSizes,C,CHalf) do
            CK is C-K,
%            writeln(step(K,CK,CHalf)),
            get_bigger(SortedSizes,CK,0,N1,0,_,Rest),
            get_bigger(Rest,CHalf,0,N2,0,Size2,Rest2),
            atleast_size_k(Rest2,K,0,Size3),
%            writeln(big(N1,N2,Size2,Size3)),
            Bound is N1+N2+integer(ceiling(max(0,(Size3-(N2*C-Size2))/C))),
%            writeln(bound(Bound)),
            A1 is max(A,Bound)
        ).

get_bigger([V|V1],Limit,N,Nend,Size,SizeEnd,Rest):-
        V > Limit,
        !,
        N1 is N+1,
        Size1 is Size+V,
        get_bigger(V1,Limit,N1,Nend,Size1,SizeEnd,Rest).
get_bigger(Rest,_,N,N,Size,Size,Rest).

atleast_size_k([V|V1],K,Size,SizeEnd):-
        V >= K,
        !,
        Size1 is Size+V,
        atleast_size_k(V1,K,Size1,SizeEnd).
atleast_size_k(_,_,Size,Size).

fixed_non_fixed([],[],[]).
fixed_non_fixed([item(X,S)|R],F,[S|S1]):-
        var(X),
        !,
        fixed_non_fixed(R,F,S1).
fixed_non_fixed([A|A1],[A|F1],S1):-
        fixed_non_fixed(A1,F1,S1).

/*

utility

*/

combine([],[],[]).
combine([B|B1],[S|S1],[item(B,S)|I1]):-
        combine(B1,S1,I1).

max_capacity([B|B1],C):-
        get_max(B,Bmax),
        (foreach(X,B1),
         fromto(Bmax,A,A1,C) do
            get_max(X,Max),
            A1 is max(A,Max)
        ).

       