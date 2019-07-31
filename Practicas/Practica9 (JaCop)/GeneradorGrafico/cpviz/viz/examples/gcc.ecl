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
:-module(gcc).

:-export(top/0).

:-export(gcc/2).
:-export(alldifferent_matrix/1).
:-export(gcc_matrix/3).
:-export(sequence/4).
:-export(sequence/5).
:-export(sequence_total/6).
:-export(sequence_total/7).
:-export(same/2).
:-export(bool_channeling/3).
:-export(inverse/2).

:-lib(ic).
:-lib(ic_global).
:-lib(ic_kernel).
:-lib(graph_algorithms).
%:-use_module(max_flow).
:-use_module(max_flow_old).
:-lib(hash).

:-local variable(cnt).

top:-
        top_total_sequence,
        top_total_sequence1,
/*
        top_gcc_matrix,
        top_fail_same,
        top_inverse,
        top_bool,
        top_alldifferent_matrix,
        top_generic_same,
        top_generic_sequence,
        top_test_sequence,
        top_sequence,
        top_gcc,
        top_same,
*/
        true.

top_total_sequence:-
        setval(cnt,0),
        length(L,10),
        L :: 0..1,
        sequence_total(5,5,0,1,2,L),
        writeln(L),
        labeling(L),
        write(L),
        sequence_total(5,5,0,1,2,L),
        writeln(ok),
        incval(cnt),
        fail.
top_total_sequence:-
        getval(cnt,Cnt),
        writeln(cnt(Cnt)).

top_total_sequence1:-
        setval(cnt,0),
        length(L,5),
        L :: 0..1,
        sequence_total(1,2,1,2,3,L),
        writeln(L),
%        L = [0,0,1,A,_],
%        A = 1,
        labeling(L),
        write(L),
        sequence_total(1,2,1,2,3,L),
        writeln(ok),
        incval(cnt),
        fail.
top_total_sequence1:-
        getval(cnt,Cnt),
        writeln(cnt(Cnt)).

top_fail_same:-
        domains(L1,H1,L2,H2),
        [X,Y,Z] :: L1..H1,
        [A,B,C] :: L2..H2,
        ic:alldifferent([X,Y,Z]),
        ic:alldifferent([A,B,C]),
        (same([A,B,C],[X,Y,Z]) ->
            writeln(ok(L1,H1,L2,H2))
        ;
            writeln(fail(L1,H1,L2,H2)),
            fail
        ),
        labeling([A,B,C,X,Y,Z]),
        writeln([A,B,C,X,Y,Z]),
        fail.
top_fail_same.

domains(1,3,4,6).
domains(1,3,2,4).
domains(1,3,1,3).

top_bool:-
        writeln(test_a),
        X :: 1..10,
        length(B,10),
        B :: 0..1,
        bool_channeling(X,B,1),
        indomain(X),
        write(X-B),
        bool_channeling(X,B,1),
        writeln(ok),
        fail.
top_bool:-
        writeln(test_b),
        X :: 1..10,
        length(B,10),
        B :: 0..1,
        bool_channeling(X,B,1),
        labeling(B),
        write(X-B),
        bool_channeling(X,B,1),
        writeln(ok),
        fail.
top_bool.

top_inverse:-
        top_inverse(5).

top_inverse(N):-
        writeln(test_a),
        length(L,N),
        L :: 1..N,
        length(K,N),
        K :: 1..N,
        inverse(L,K),
        ic:alldifferent(L),
        search(L,0,input_order,indomain,complete,[]),
        write(L-K),
        inverse(L,K),
        writeln(ok),
        fail.
top_inverse(N):-
        writeln(test_b),
        length(L,N),
        L :: 1..N,
        length(K,N),
        K :: 1..N,
        inverse(L,K),
        ic:alldifferent(K),
        search(K,0,input_order,indomain,complete,[]),
        write(L-K),
        inverse(L,K),
        writeln(ok),
        fail.
top_inverse(N):-
        writeln(test_c),
        length(L,N),
        L :: 1..N,
        length(K,N),
        K :: 1..N,
        inverse(L,K),
        ic:alldifferent(K),
        search(L,0,input_order,indomain,complete,[]),
        write(L-K),
        inverse(L,K),
        writeln(ok),
        fail.
top_inverse(_).

top_alldifferent_matrix:-
        top_alldifferent_matrix(3),
        top_alldifferent_matrix(4).

top_alldifferent_matrix(N):-
        dim(Matrix,[N,N]),
        Matrix[1..N,1..N] :: 1..N,
        (for(I,1,N),
         param(Matrix,N) do
            ic:alldifferent(Matrix[I,1..N]),
            ic:alldifferent(Matrix[1..N,I])
        ),
        alldifferent_matrix(Matrix),
        flatten_array(Matrix,List),
        search(List,0,input_order,indomain,complete,[]),
        write(Matrix),
        alldifferent_matrix(Matrix),
        writeln(ok),
        fail.
top_alldifferent_matrix(_).

        
top_gcc_matrix:-
        top_gcc_matrix(2,3).

top_gcc_matrix(N,M):-
        setval(cnt,0),
        dim(Matrix,[N,M]),
        Matrix[1..N,1..M] :: 1..4,
        RowLimit = [gcc(1,2,1),gcc(1,1,2),gcc(0,1,3),gcc(0,1,4)],
        ColLimit = [gcc(0,1,1),gcc(0,1,2),gcc(0,1,3),gcc(0,1,4)],
        (for(I,1,N),
         foreach(RowLimit,RLimits),
         param(Matrix,RowLimit,M) do
            gcc(RowLimit,Matrix[I,1..M])
        ),
        (for(J,1,M),
         foreach(ColLimit,CLimits),
         param(Matrix,ColLimit,N) do
            gcc(ColLimit,Matrix[1..N,J])
        ),
        gcc_matrix(RLimits,CLimits,Matrix),
        flatten_array(Matrix,List),
        writeln(search),
        search(List,0,input_order,indomain,complete,[]),
        write(Matrix),
        gcc_matrix(RLimits,CLimits,Matrix),
        incval(cnt),
        writeln(ok),
        fail.
top_gcc_matrix(_N,_M):-
        getval(cnt,Cnt),
        writeln(Cnt).


top_generic_same:-
        (for(N,2,20) do
            length(L1,N),
            length(L2,N),
            L1 :: 1..N,
            L2 :: 1..N,
            ic:alldifferent(L1),
            same(L1,L2),
            labeling(L1),
            labeling(L2),
            write(L1-L2),
            same(L1,L2),
            writeln(ok)
        ).

top_same:-
        [A,B,C,D,E,F] :: 1..3,
        ic:alldifferent([A,B,C]),
        same([A,B,C],[D,E,F]),
        labeling([A,B,C,D,E,F]),
        sort(0,=<,[A,B,C],L1),
        sort(0,=<,[A,B,C],L2),
        L2 = L1,
        write([A,B,C]-[D,E,F]),
        same([A,B,C],[D,E,F]),
        writeln(ok),
        fail.
top_same.

top_test_sequence:-
        (for(N,5,20) do
            (for(K,2,N),
             param(N) do
                test_sequence(K,N)
            )
        ).

test_sequence(K,N):-
        writeln(N-K),
        length(Vars,N),
        Vars :: 0..1,
        K1 is K-1,
        sequence(1,K1,K,Vars),
        labeling(Vars),
        write(Vars),
        sequence(1,K1,K,Vars),
        writeln(ok),
        !.
test_sequence(K,N):-
        writeln(missed(K,N)),
        abort.

top_sequence:-
        length(L,5),
        L :: 0..1,
        sequence(1,2,3,L),
        writeln(L),
        labeling(L),
        write(L),
        sequence(1,2,3,L),
        writeln(ok),
        fail.
top_sequence.

top_generic_sequence:-
        length(L,6),
        L :: 0..4,
        sequence(1,2,3,L,[2,3,4]),
        sequence(2,2,4,L,[1,2]),
        writeln(L),
        labeling(L),
        write(L),
        sequence(1,2,3,L,[2,3,4]),
        sequence(2,2,4,L,[1,2]),
        writeln(ok),
        fail.
top_generic_sequence.


top_gcc:-
        [X1,X2,X3,X4] :: 1..4,
        gcc([gcc(0,2,1),gcc(1,1,2),gcc(0,1,3)],[X1,X2,X3,X4]),
        writeln([X1,X2,X3,X4]),
        labeling([X1,X2,X3,X4]),
        write([X1,X2,X3,X4]),
        gcc([gcc(0,2,1),gcc(1,1,2),gcc(0,1,3)],[X1,X2,X3,X4]),
        writeln(ok),
        fail.
top_gcc.

/***************************************************************

Structure definitions

***************************************************************/
        
% common structure for all constraints
:-local struct(low_high(low,high,mapping)).

/***************************************************************

GCC with fixed bounds

***************************************************************/
        
gcc(Bounds,Vars):-
        collection_to_list(Vars,Variables),
        length(Variables,N),
        length(Bounds,M),
        check_gcc(Bounds,Variables,N,M),
        (ground(Variables) ->
            true
        ;
            suspend(update_gcc(Bounds,Variables,N,M,Susp),9,
                    [Variables->inst,
                     Variables->ic:min,
                     Variables->ic:max,
                     Variables->ic:hole],Susp)
        ).

:-demon(update_gcc/5).
update_gcc(Bounds,Variables,N,M,Susp):-
        check_gcc(Bounds,Variables,N,M),
        (ground(Variables) ->
            kill_suspension(Susp)
        ;
            true
        ).

check_gcc(Bounds,Variables,N,M):-
        create_gcc_edges(Variables,Bounds,N,M,
                          Mapping,
                          SourceNode,SinkNode,Edges),
%        writeln(Mapping),
        make_graph(SinkNode,Edges,Graph),
        feas_flow_with_lb(Graph, low of low_high, high of low_high,
                          SourceNode, SinkNode, 
                          MaxFlowValue, MaxFlowEdges, _),
%        writeln(N-MaxFlowValue-MaxFlowEdges),
        MaxFlowValue >= N, % may fail
%        is_feasible_flow(MaxFlowEdges,Edges), % testing only, remove later
        residual_graph(MaxFlowEdges,Edges,ResidualEdges,MidEdges,_),
        make_graph(SinkNode,ResidualEdges,ResidualGraph),
        strong_components(ResidualGraph,StrongComponents),
%        writeln(strong(StrongComponents)),
        mark_components(StrongComponents,SinkNode,MidEdges,NotMarked),
%        writeln(unmarked(NotMarked)),
        call_priority(remove_unmarked_edges(NotMarked,Mapping),2),
        wake.

create_gcc_edges(Variables,Bounds,N,M,Mapping,
                  SourceNode,SinkNode,Edges):-
        SourceNode is N+M+1,
        SinkNode is N+M+2,
        dim(Mapping,[SinkNode]),
        arg(SourceNode,Mapping,source),
        arg(SinkNode,Mapping,sink),
        hash_create(Hash),
        hash_add(Hash,node(SourceNode),source),
        hash_add(Hash,node(SinkNode),sink),
        (foreach(gcc(Low,High,Value),Bounds),
         fromto([],E,[e(J,SinkNode,
                        low_high{low:Low,high:High,mapping:0})|E],
                EdgesSink),
         count(J,N+1,_),
         param(Mapping,Hash,SinkNode) do
            (hash_find(Hash,Value,_) ->
                writeln(no_value(Value)),
                abort
            ;
                true
            ),
            hash_add(Hash,Value,J),
            arg(J,Mapping,Value)
        ),
                 
        (foreach(X,Variables),
         count(J,1,_),
         fromto([],E,E1,EdgesMain),
         fromto([],F,[e(SourceNode,J,
                        low_high{low:1,high:1,mapping:0})|F],EdgesSource),
         param(Mapping,Hash,SourceNode) do
            arg(J,Mapping,X),
            get_domain_as_list(X,Domain),
            (foreach(V,Domain),
             fromto(E,Edges,Edges1,E1),
             param(Hash,J,X) do
                (hash_find(Hash,V,Node) ->
                    Edges1 = [e(J,Node,
                                low_high{low:0,high:1,mapping:0})|Edges]
                ;
                    exclude(X,V),
                    Edges1 = Edges
                )
            )
        ),
%        writeln(EdgesSource),
%        writeln(EdgesMain),
%        writeln(EdgesSink),
        append(EdgesSource,EdgesMain,EE),
        append(EdgesSink,EE,Edges).


/*******************************************************************************

alldifferent_matrix

*******************************************************************************/

alldifferent_matrix(Matrix):-
        check_alldifferent_matrix(Matrix),
        flatten_array(Matrix,List),
        (ground(List) ->
            true
        ;
            suspend(update_alldifferent_matrix(List,Matrix,Susp),
                    0,[List->ic:min,
                       List->ic:max,
                       List->ic:hole],Susp)
        ).


:-demon(update_alldifferent_matrix/3).
update_alldifferent_matrix(List,Matrix,Susp):-
        check_alldifferent_matrix(Matrix),
        (ground(List) ->
            kill_suspension(Susp)
        ;
            true
        ).

check_alldifferent_matrix(Matrix):-
        dim(Matrix,[N,N]),
        NrNodes is 2*N,
        (for(I,1,N),
         foreach(I,RowNodes),
         count(J,N+1,_),
         foreach(J,ColNodes) do
            true
        ),
        (multifor([I,J],[1,1],[N,N]),
         fromto([],A,A1,Edges),
         param(N,Matrix) do
            subscript(Matrix,[I,J],X),
            get_domain_as_list(X,Dom),
            J1 is J+N,
            (foreach(V,Dom),
             fromto(A,C,[e(I,J1,V)|C],A1),
             param(I,J1) do
                true
            )
        ),
        sort(3,=<,Edges,Sorted),
        group_by(3,Sorted,Grouped),
        (foreach(_V-EdgeGroup,Grouped),
         param(RowNodes,ColNodes,NrNodes,N,Matrix) do
            make_graph(NrNodes,EdgeGroup,Graph),
            maximum_matching_hopcroft_karp(Graph, RowNodes,ColNodes, MaximalM),
            length(MaximalM,Size),
            Size >= N, % may fail
            invert_edges(EdgeGroup,MaximalM,InvertedEdges),
            append(MaximalM,InvertedEdges,Edges1),
            make_graph(NrNodes,Edges1,Graph1),
            strong_components(Graph1, StrongComponents),
            mark_components(StrongComponents,NrNodes,InvertedEdges,NotMarked),
            % n.b. not marked edges are inverted, i.e. arg 1 is column
            (foreach(e(Col,Row,Value),NotMarked),
             param(N,Matrix) do
                Col1 is Col-N,
                subscript(Matrix,[Row,Col1],X),
                exclude(X,Value)
            )
        ),
        wake.


/*
invert the edges in a list which are not in matching
uses hash table to avoid list lookup
*/
invert_edges(Edges,MaximalM,InvertedEdges):-
        hash_create(Match),
        (foreach(Edge,MaximalM),
         param(Match) do
            hash_add(Match,Edge,1)
        ),
        (foreach(Edge,Edges),
         fromto([],A,A1,InvertedEdges),
         param(Match) do
            direct_edge(Edge,Match,A,A1)
        ).

% ignore edge if it is in the matching
% otherwise invert its direction and put into accumulator
direct_edge(Edge,Match,A,A):-
        hash_find(Match,Edge,_),
        !.
direct_edge(e(From,To,W),_Match,A,[e(To,From,W)|A]).

group_by(Arg,[H|T],S):-
        arg(Arg,H,Key),
        group_by(Arg,T,Key,[H],S).

group_by(_,[],Key,L,[Key-L]).
group_by(Arg,[H|T],Key,L,S):-
        arg(Arg,H,Key),
        !,
        group_by(Arg,T,Key,[H|L],S).
group_by(Arg,[H|T],Key,L,[Key-L|S]):-
        arg(Arg,H,Key1),
        group_by(Arg,T,Key1,[H],S).

/******************************************************************************

GCC Matrix

******************************************************************************/

gcc_matrix(Row,Col,Matrix):-
        hash_create(Limits),
        (foreach(RowLimits,Row),
         count(I,1,_),
         param(Limits) do
            (foreach(gcc(Low,High,Value),RowLimits),
             param(Limits,I) do
                hash_add(Limits,row_limit(I,Value),Low-High)
            )
        ),
        (foreach(ColLimits,Col),
         count(J,1,_),
         param(Limits) do
            (foreach(gcc(Low,High,Value),ColLimits),
             param(Limits,J) do
                hash_add(Limits,col_limit(J,Value),Low-High)
            )
        ),
        check_gcc_matrix(Matrix,Limits),
        flatten_array(Matrix,List),
        (ground(List) ->
            true
        ;
            suspend(update_gcc_matrix(List,Matrix,Limits,Susp),
                    0,[List->ic:min,
                       List->ic:max,
                       List->ic:hole],Susp)
        ).


:-demon(update_gcc_matrix/4).
update_gcc_matrix(List,Matrix,Limits,Susp):-
        check_gcc_matrix(Matrix,Limits),
        (ground(List) ->
            kill_suspension(Susp)
        ;
            true
        ).

check_gcc_matrix(Matrix,Limits):-
        dim(Matrix,[N,M]),
        SourceNode is N+M+1,
        SinkNode is N+M+2,
        NrNodes is N+M+2,
        (multifor([I,J],[1,1],[N,M]),
         fromto([],A,A1,AllEdges),
         param(N,Matrix) do
            subscript(Matrix,[I,J],X),
            get_domain_as_list(X,Dom),
            J1 is J+N,
            (foreach(V,Dom),
             fromto(A,C,[e(I,J1,V)|C],A1),
             param(I,J1) do
                true
            )
        ),
        sort(3,=<,AllEdges,Sorted),
        group_by(3,Sorted,Grouped),
        (foreach(V-EdgeGroup,Grouped),
         param(NrNodes,N,M,Matrix,
               Limits,SourceNode,SinkNode) do
%            writeln(value(V)),
%            writeln(Matrix),
%            writeln(group(EdgeGroup)),
            gcc_matrix_create_edges(EdgeGroup,V,Limits,Matrix,
                                    N,M,SourceNode,SinkNode,Edges,_MidEdges),
            make_graph(NrNodes,Edges,Graph),
%            writeq(V-make_graph(NrNodes,Edges,SourceNode,SinkNode)),nl,
            feas_flow_with_lb(Graph,  low of low_high,high of low_high,
                              SourceNode, SinkNode, 
                              _MaxFlowValue, MaxFlowEdges, _),
%            writeln(N-MaxFlowValue-MaxFlowEdges),
%            is_feasible_flow(MaxFlowEdges,Edges),
%            writeln(feasible),
            residual_graph(MaxFlowEdges,Edges,ResidualEdges,RestEdges,_),
%            writeln(residual(ResidualEdges)),
            make_graph(SinkNode,ResidualEdges,ResidualGraph),
            strong_components(ResidualGraph,StrongComponents),
%            writeln(strong(StrongComponents)),
            mark_components(StrongComponents,SinkNode,RestEdges,NotMarked),
%            writeln(unmarked(NotMarked)),
            (foreach(e(Row,Col,_),NotMarked),
             param(N,M,Matrix,V) do
                Col1 is Col-N,
                ((Row >= 1, Row =< N, Col1 >= 1, Col1 =< M) ->
                    subscript(Matrix,[Row,Col1],X),
%                    writeln(V-X-Row-Col),
%                    writeln(Matrix),
                    exclude(X,V)
                ;
                    true
                )
            )
        ),
%        writeln(wake),
        wake.

gcc_matrix_create_edges(EdgeGroup,V,Limits,Matrix,
                        N,M,SourceNode,SinkNode,Edges,MidEdges):-
        (foreach(e(From,To,_),EdgeGroup),
         foreach(e(From,To,
                   low_high{low:Low,high:1,mapping:0}),MidEdges),
         param(Matrix,N,V) do
            subscript(Matrix,[From,To-N],Entry),
            (Entry == V ->
                Low = 1
            ;
                Low = 0
            )
        ),
        (for(I,1,N),
         foreach(e(SourceNode,I,
                   low_high{low:Low,high:High,mapping:0}),
                 LeftEdges),
         param(Limits,V,SourceNode) do
            hash_find(Limits,row_limit(I,V),Low-High)
        ),
        (for(J,1,M),
         foreach(e(J1,SinkNode,
                   low_high{low:Low,high:High,mapping:0}),
                 RightEdges),
         param(Limits,V,N,SinkNode) do
            J1 is J+N,
            hash_find(Limits,col_limit(J,V),Low-High)
        ),
%        writeln(LeftEdges),
%        writeln(RightEdges),
%        writeln(mid(MidEdges)),
        append(LeftEdges,MidEdges,Edges1),
        append([e(SinkNode,SourceNode,
                  low_high{low:0,high:10000,mapping:0})|RightEdges],
               Edges1,Edges).

/******************************************************************************

SEQUENCE

******************************************************************************/


sequence_total(Min,Max,L,U,K,Vars,Values):-
        index_function(Vars,Values,Index),
        (foreach(X,Vars),
         foreach(Z,ZeroOne),
         param(Index) do
            element(X,Index,Z)
        ),
        sequence_total(Min,Max,L,U,K,ZeroOne).

sequence(L,U,K,Vars,Values):-
        index_function(Vars,Values,Index),
        (foreach(X,Vars),
         foreach(Z,ZeroOne),
         param(Index) do
            element(X,Index,Z)
        ),
        sequence(L,U,K,ZeroOne).

index_function(Vars,Values,Index):-
        upper_bound(Vars,Size),
        dim(Index,[Size]),
        (foreach(V,Values),
         param(Index,Size) do
            (V =< Size ->
                arg(V,Index,1)
            ;
                true
            )
        ),
        (for(J,1,Size),
         param(Index) do
            arg(J,Index,V),
            default(V,0)
        ).

default(V,V):-
        !.
default(_,_).

upper_bound([H|T],Size):-
        get_max(H,Initial),
        (foreach(X,T),
         fromto(Initial,A,A1,Size) do
            get_max(X,XMax),
            A1 is max(A,XMax)
        ).

sequence_total(Min,Max,L,U,K,Vars):-
        total_sequence(Min,Max,L,U,K,Vars),
        sequence(L,U,K,Vars).

total_sequence(Min,Max,L,U,K,Vars):-
        collection_to_list(Vars,Variables),
        length(Variables,N),
%        writeln(check_total_sequence(Min,Max,L,U,K,N,Variables)),
        check_total_sequence(Min,Max,L,U,K,N,Variables),
        (ground(Variables) ->
            true
        ;
            suspend(update_total_sequence(Min,Max,L,U,K,N,Variables,
                                          Susp),
                    9,[Variables->ic:min,
                       Variables->ic:max,
                       Variables->ic:hole],Susp)
        ).

:-demon(update_total_sequence/8).
update_total_sequence(Min,Max,L,U,K,N,Variables,Susp):-
        check_total_sequence(Min,Max,L,U,K,N,Variables),
        (ground(Variables) ->
            kill_suspension(Susp)
        ;
            true
        ).        
        
check_total_sequence(Min,Max,L,U,K,N,Variables):-
        create_total_sequence_edges(Min,Max,L,U,K,N,Variables,
                                    NrNodes,
                                    SourceNode,SinkNode,
                                    Edges,XEdges,
                                    CombinedEdges,
                                    Combination,Mapping),
%        writeln(SourceNode-SinkNode),
%        writeln(Combination),
        make_graph(NrNodes,CombinedEdges,Graph),
%        writeq(feas(Edges)),nl,
%        writeq(combined(CombinedEdges)),nl,
        feas_flow_with_lb(Graph,  
                          low of low_high,high of low_high,
                          SourceNode, SinkNode, 
                          MaxFlowValue, CombinedMaxFlowEdges, _),
%        writeln(Min-Max-MaxFlowValue-CombinedMaxFlowEdges),
%        is_feasible_flow(CombinedMaxFlowEdges,CombinedEdges),
        hash_expand_edges(CombinedMaxFlowEdges,Combination,
                          MaxFlowEdges),
%        writeln(max_flow_edges(MaxFlowEdges)),
%        is_feasible_flow(MaxFlowEdges,Edges),
        MaxFlowValue >= Min,
        MaxFlowValue =< Max,
        residual_graph(MaxFlowEdges,Edges,ResidualEdges,_MidEdges,
                       FlowSolution),
%        writeq(res(ResidualEdges)),nl,
        make_graph(NrNodes,ResidualEdges,ResidualGraph),
        strong_components(ResidualGraph,StrongComponents),
%        writeln(strong(StrongComponents)),
        mark_components(StrongComponents,NrNodes,XEdges,NotMarked),
%        writeln(unmarked(NotMarked,FlowSolution)),
        remove_sequence_inconsistent(NotMarked,Mapping,FlowSolution),
        wake.

hash_expand_edges(CombinedMaxFlowEdges,Combination,MaxFlowEdges):-
        (foreach(Edge,CombinedMaxFlowEdges),
         fromto([],A,A1,MaxFlowEdges),
         param(Combination) do
            (is_multi_edge(Edge,Flow) ->
%                writeln(Edge),
                Edge = _-Edge1,
                hash_find(Combination,Edge1,Multiples),
%                writeln(Multiples),
                distribute_flow(Multiples,Flow,A,A1)
            ;
                A1 = [Edge|A]
            )
        ).

create_total_sequence_edges(Min,Max,L,U,K,N,Variables,
                            NrNodes,
                            SourceNode,SinkNode,
                            Edges,XEdges,
                            CombinedEdges,
                            Combination,Mapping):-
        NrSets is integer(ceiling(N/K)),
        SourceNode is 2*NrSets+1,
        SinkNode is SourceNode+1,
        NrNodes = SinkNode,
%        writeln(nr(NrSets,SourceNode,SinkNode,N)),
        (for(J,1,NrSets),
         foreach(e(SourceNode,J,low_high{low:L1,high:U,mapping:0}),
                 LeftEdges),
         foreach(e(J1,SinkNode,low_high{low:L2,high:U,mapping:0}),
                 RightEdges),
         param(SourceNode,SinkNode,L,U,NrSets,N,K) do
            J1 is J+NrSets,
            ((J =:= NrSets,N mod K =\= 0)  ->
                L1 = 0
            ;
                L1 = L
            ),
            ((J =:= 1,N mod K =\= 0)  ->
                L2 = 0
            ;
                L2 = L
            )
        ),
%        writeln(LeftEdges),
        dim(Mapping,[N]),
        (for(I,1,N),
         foreach(X,Variables),
         fromto([],A,A1,XEdges),
         param(Mapping,N,K,NrSets) do
            arg(I,Mapping,X),
            From is (I+K-1)//K,
            To is 2*NrSets-(N-I)//K,
%            writeln(from(I,From,To)),
            (X == 0 ->
                A1 = A
            ; X ==1 ->
                A1 = [e(From,To,low_high{low:1,high:1,mapping:I})|A]
            ;
                A1 = [e(From,To,low_high{low:0,high:1,mapping:I})|A]
            )
        ),
%        writeln(XEdges),
        hash_create(Combination),
        merge_edges(XEdges,MergedEdges,Combination),
        append([e(SinkNode,SourceNode,
                  low_high{low:Min,high:Max,mapping:0})|LeftEdges],
               RightEdges,
               Edges1),
        append(Edges1,XEdges,Edges),
        append(Edges1,MergedEdges,CombinedEdges).

merge_edges([A|R],S,Combination):-
        merge_edges(R,A,[A],S,Combination).

merge_edges([],M,L,[M],Hash):-
        hash_add(Hash,M,L).
merge_edges([A|R],B,L,S,Hash):-
        merge_two_edges(A,B,M),
        !,
        merge_edges(R,M,[A|L],S,Hash).
merge_edges([A|R],M,L,[M|S],Hash):-
        hash_add(Hash,M,L),
        merge_edges(R,A,[A],S,Hash).

merge_two_edges(e(From,To,low_high{low:L1,high:H1}),
                e(From,To,low_high{low:L2,high:H2}),
                e(From,To,low_high{low:L,high:H,mapping: -1})):-
        L is L1+L2,
        H is H1+H2.

% this is the binary version
sequence(L,U,K,Vars):-
        collection_to_list(Vars,Variables),
        length(Variables,N),
%        writeln(check_sequence(L,U,K,N,Variables)),
        check_sequence(L,U,K,N,Variables),
        (ground(Variables) ->
            true
        ;
            suspend(update_sequence(L,U,K,N,Variables,Susp),
                    9,[Variables->ic:min,
                       Variables->ic:max/*,
                       Variables->ic:hole*/],Susp)
        ).

:-demon(update_sequence/6).
update_sequence(L,U,K,N,Variables,Susp):-
        check_sequence(L,U,K,N,Variables),
        (ground(Variables) ->
            kill_suspension(Susp)
        ;
            true
        ).        
        
check_sequence(L,U,K,N,Variables):-
        create_edges(L,U,K,N,Variables,NrNodes,
                     SourceNode,SinkNode,Edges,XEdges,
                     CombinedEdges,MultiEdges,Mapping),
        make_graph(NrNodes,CombinedEdges,Graph),
        % ??? this needs to handle lower bounds as well
%        writeln(vars(Variables)),
%        writeq(feas(Edges)),nl,
        feas_flow_with_lb(Graph,  low of low_high,high of low_high,
                          SourceNode, SinkNode, 
                          MaxFlowValue, CombinedMaxFlowEdges, _),
        ExpFlow is (N-K)*(U-L)+U,
%        writeln(MaxFlowValue-ExpFlow-MaxFlowEdges),
%        is_feasible_flow(CombinedMaxFlowEdges,CombinedEdges),
        expand_edges(CombinedMaxFlowEdges,MultiEdges,MaxFlowEdges),
%        is_feasible_flow(MaxFlowEdges,Edges),
        MaxFlowValue = ExpFlow,
        residual_graph(MaxFlowEdges,Edges,ResidualEdges,_MidEdges,FlowSolution),
        make_graph(NrNodes,ResidualEdges,ResidualGraph),
        strong_components(ResidualGraph,StrongComponents),
%        writeln(strong(StrongComponents)),
        mark_components(StrongComponents,NrNodes,XEdges,NotMarked),
%        writeln(unmarked(NotMarked)),
        remove_sequence_inconsistent(NotMarked,Mapping,FlowSolution),
        wake.

/*

a lot of extra code is required to work around the restriction of -no
 multiple edges- in the max_flow library: we have to combine edges by
 adding up their capacity and then later on distribute the flow over
 that edge; it also means we can not use from-to as the hash key to
 find the correct variable belonging to an edge, we introdcue the
 mapping field in low_high to store an index number which we then use
 to look up the variable in an array Mapping

*/

% if there is no or only one MultiEdge, then there is nothing to do
expand_edges(MaxFlowEdges,[],MaxFlowEdges):-
        !.
expand_edges(MaxFlowEdges,[_],MaxFlowEdges):-
        !.
expand_edges(CombinedMaxFlowEdges,MultiEdges,MaxFlowEdges):-
        (foreach(Edge,CombinedMaxFlowEdges),
         fromto([],A,A1,MaxFlowEdges),
         param(MultiEdges) do
            expand_edge(Edge,MultiEdges,A,A1)
        ).

expand_edge(Edge,MultiEdges,A,A1):-
        is_multi_edge(Edge,Flow),
        !,
        distribute_flow(MultiEdges,Flow,A,A1).
expand_edge(Edge,_MultiEdges,A,[Edge|A]).

is_multi_edge(Flow-e(_,_,low_high{mapping: -1}),Flow).

% this only works for 0/1 flow limits, needs constraint solver in
% general case
distribute_flow(MultiEdges,Flow,In,Out):-
        (foreach(Edge,MultiEdges),
         fromto(In,A,A1,Forced),
         fromto(Flow,B,B1,Rest),
         fromto([],C,C1,Remaining) do
            (required_flow(Edge) ->
                A1 = [1-Edge|A],
                B1 is B-1,
                C1 = C
            ;
                A1 = A,
                B1 = B,
                C1 = [Edge|C]
            )
        ),
        (foreach(Edge,Remaining),
         fromto(Forced,A,A1,Out),
         fromto(Rest,B,B1,0) do
            (B > 0 ->
                A1 = [1-Edge|A],
                B1 is B-1
            ;
                A1 = A,
                B1 = B
            )
        ).

% the edge has a lower bound
required_flow(e(_,_,low_high{low:Low})):-
        Low > 0.

/*
this is different from the other flow-based constraints
edges which are marked do not indicate a value to remove, but that for
 the variable associated, only the flow solution value is feasible,
 the opposite value can be removed
*/
remove_sequence_inconsistent(NotMarked,Mapping,FlowSolution):-
        (foreach(e(_From,_To,low_high{mapping:M}),NotMarked),
         param(Mapping,FlowSolution) do
            hash_find(FlowSolution,M,ConsistentValue),
            arg(M,Mapping,X),
            ToRemove is 1-ConsistentValue,
            exclude(X,ToRemove)
        ).

/*
In the construction, the only possible parallel edges are between the first and
 last row of the matrix, whether they exist depends on the parameters
*/
create_edges(L,U,K,N,Variables,SinkNode,
             SourceNode,SinkNode,Edges,XEdges,
             CombinedAllEdges,MultiEdges,Mapping):-
        RowNodes is 2*N-2*K+3,
        SourceNode is RowNodes+1,
        SinkNode is RowNodes+2,
        UL is U-L,
%        writeln(problem(L,U,K,N,RowNodes,SourceNode,SinkNode,UL)),
        (UL > 0 ->
            (for(J,2,RowNodes-1,2),
             fromto([],A,[e(SourceNode,J,
                            low_high{low:UL,high:UL,mapping:0})|A],RowEdges1),
             param(SourceNode,UL) do
                true
            ),
            (for(J,3,RowNodes-2,2),
             fromto([],A,
                    [e(J,SinkNode,low_high{low:UL,high:UL,mapping:0})|A],
                    SinkEdges),
             param(SinkNode,UL) do
                true
            )
        ;
            RowEdges1 = [],
            SinkEdges = []
        ),
        (for(J,2,RowNodes-1,2),
         fromto(RowEdges1,
                A,[e(J,J1,low_high{low:0,high:N,mapping:0}),
                   e(J,J2,low_high{low:0,high:N,mapping:0})|A],RowEdges),
         param(N) do
            J1 is J-1,
            J2 is J+1
        ),
%        writeln(RowEdges),
%        writeln(SinkEdges),
        SpecialEdges = [e(SourceNode,1,low_high{low:L,high:L,mapping:0}),
                        e(RowNodes,SinkNode,low_high{low:U,high:U,mapping:0})],
%        writeln(SpecialEdges),
        dim(Mapping,[N]),
        (foreach(X,Variables),
         count(M,1,_),
         fromto([],A,A1,XEdges),
         fromto([],B,B1,SingleEdges),
         fromto([],C,C1,MultiEdges),
         param(RowNodes,K,Mapping) do
            From is max(1,1+2*(M-K)),
            To is min(RowNodes,1+2*M),
            arg(M,Mapping,X),
            (X == 0 ->
                A1 = A,
                B1 = B,
                C1 = C
            ; 
                (X == 1 ->
                    Edge = e(From,To,low_high{low:1,high:1,mapping:M})
                ;
                    Edge = e(From,To,low_high{low:0,high:1,mapping:M})
                ),
                ((From = 1,To = RowNodes) ->
                    A1 = [Edge|A],
                    B1 = B,
                    C1 = [Edge|C]
                ;
                    A1 = [Edge|A],
                    B1 = [Edge|B],
                    C1 = C
                )
            )
        ),
%        writeln(multi(MultiEdges)),
        length(MultiEdges,Multi),
        (Multi > 1 ->
            combine_edges(MultiEdges,CombinedEdges)
        ;
            CombinedEdges = MultiEdges
        ),
%        writeln(CombinedEdges),
        append(CombinedEdges,SingleEdges,CombinedXEdges),
        append(RowEdges,SinkEdges,E1),
        append(SpecialEdges,E1,E2),
        append(CombinedXEdges,E2,CombinedAllEdges),
        append(XEdges,E2,Edges).

% mark combined edge with mapping = -1
combine_edges([e(From,To,low_high{low:Low,high:High})|R],
              [e(From,To,low_high{low:LowEnd,high:HighEnd,mapping: -1})]):-
        (foreach(e(_,_,low_high{low:Low1,high:High1}),R),
         fromto(Low,A,A1,LowEnd),
         fromto(High,B,B1,HighEnd) do
            A1 is A+Low1,
            B1 is B+High1
        ).

% abort if infeasible edge detected
is_feasible_flow(MaxFlowEdges,Edges):-
        hash_create(Hash),
        (foreach(Flow-Edge,MaxFlowEdges),
         fromto(0,A,A1,Err),
         param(Hash) do
            hash_add(Hash,Edge,1),
            Edge = e(_,_,low_high{low:L,high:H}),
            ((Flow >= L,
              Flow =< H) ->
                A1 = A
            ;
                A1 = 1,
                writeln(infeasible(Flow,Edge))
            )
        ),
        (foreach(Edge,Edges),
         fromto(Err,A,A1,Error),
         param(Hash) do
            (hash_find(Hash,Edge,_) ->
                A1 = A
            ;
                Edge = e(_,_,low_high{low:L,high:H}),
                ((0 >= L,0 =< H) ->
                    A1 =A
                ;
                    A1 = 1,
                    writeln(infeasible(0,Edge))
                )
            )
        ),
        (Error = 1 ->
            writeln(problem),
            abort
        ;
            true
        ).

/*********************************************************************

SAME


*********************************************************************/

same(L1,L2):-
        collection_to_list(L1,Vars1),
        collection_to_list(L2,Vars2),
        length(Vars1,N),
        length(Vars2,N),
        append(Vars1,Vars2,Variables),
        check_same(Vars1,Vars2,N),
        (ground(Variables) ->
            true
        ;
            suspend(update_same(Variables,Vars1,Vars2,N,Susp),
                    0,[Variables->ic:min,
                       Variables->ic:max,
                       Variables->ic:hole],Susp)
        ).

:-demon(update_same/5).
update_same(Variables,Vars1,Vars2,N,Susp):-
        check_same(Vars1,Vars2,N),
        (ground(Variables) ->
            kill_suspension(Susp)
        ;
            true
        ).

check_same(Vars1,Vars2,N):-
        same_create_edges(Vars1,Vars2,N,NrNodes,
                          CenterEdges,SourceNode,SinkNode,Edges,
                          Mapping,ValueHash),
        make_graph(NrNodes,Edges,Graph),
        feas_flow_with_lb(Graph, low of low_high,high of low_high, 
                          SourceNode, SinkNode, 
                          MaxFlowValue, MaxFlowEdges, _),
        MaxFlowValue >= N,
        same_residual_graph(MaxFlowEdges,Edges,ResidualEdges),
        make_graph(NrNodes,ResidualEdges,ResidualGraph),
        strong_components(ResidualGraph,StrongComponents),
        mark_components(StrongComponents,NrNodes,CenterEdges,NotMarked),
        remove_same_inconsistent(NotMarked,Mapping,ValueHash),
        wake.

same_create_edges(Vars1,Vars2,N,NrNodes,
                  CenterEdges,
                  SourceNode,SinkNode,Edges,Mapping,Hash):-
        N2 is 2*N,
        SourceNode is N2+1,
        SinkNode is N2+2, 
        FreeNode is SinkNode+1,
        dim(Mapping,[N2]),
        hash_create(Hash),
        (foreach(X,Vars1),
         count(J,1,_),
         fromto([],A,[e(SourceNode,J,low_high{low:1,high:1,mapping:0})|A],LeftEdges),
         fromto([],B,B1,MidLeftEdges),
         fromto(FreeNode,C,C1,LastNode1),
         param(SourceNode,Mapping,Hash) do
            arg(J,Mapping,X),
            get_domain_as_list(X,List),
            (foreach(V,List),
             fromto(B,BB,[e(J,Node,low_high{low:0,high:1,mapping:0})|BB],B1),
             fromto(C,CC,CC1,C1),
             param(J,Hash) do
                new_node(Hash,V,Node,CC,CC1)
            )
        ),
        (foreach(X,Vars2),
         count(J,N+1,_),
         fromto([],A,[e(J,SinkNode,low_high{low:1,high:1,mapping:0})|A],RightEdges),
         fromto([],B,B1,MidRightEdges),
         fromto(LastNode1,C,C1,NrNodes),
         param(SinkNode,Mapping,Hash) do
            arg(J,Mapping,X),
            get_domain_as_list(X,List),
            (foreach(V,List),
             fromto(B,BB,[e(Node,J,low_high{low:0,high:1,mapping:0})|BB],B1),
             fromto(C,CC,CC1,C1),
             param(J,Hash) do
                new_node(Hash,V,Node,CC,CC1)
            )
        ),
        append(MidLeftEdges,MidRightEdges,CenterEdges),
        append(LeftEdges,CenterEdges,Edges1),
        append(RightEdges,Edges1,Edges).

new_node(Hash,V,Node,CC,CC):-
        hash_find(Hash,V,Node),
        !.
new_node(Hash,V,CC,CC,CC1):-
        hash_add(Hash,V,CC),
        hash_add(Hash,node(CC),V),
        CC1 is CC+1.


                
same_residual_graph(MaxFlowEdges,Edges,ResidualEdges):-
        (foreach(_-X,MaxFlowEdges),
         fromto(Edges,A,A1,ResidualEdges) do
            invert_edge(X,A,A1)
        ).

invert_edge(e(From,To,Cap),A,[e(To,From,Cap)|A]).


remove_same_inconsistent(NotMarked,Mapping,Hash):-
        dim(Mapping,[N2]),
        (foreach(e(From,To,_),NotMarked),
         param(Mapping,Hash,N2) do
            ((From =< N2,hash_find(Hash,node(To),Value)) ->
                arg(From,Mapping,X),
                exclude(X,Value)
            ;(To =< N2,hash_find(Hash,node(From),Value)) ->
                arg(To,Mapping,X),
                exclude(X,Value)
            ;
                writeln(inconsistent(From,To)),
                abort
            )
        ).

/*

utility

*/

residual_graph(MaxFlowEdges,Edges,ResidualEdges,Residual2,FlowSolution):-
        hash_create(Hash),
        hash_create(FlowSolution),
        (foreach(Flow-Edge,MaxFlowEdges),
         fromto([],A,A1,Residual1),
         param(Hash,FlowSolution) do
            store_flow(FlowSolution,Edge,Flow),
            hash_add(Hash,Edge,1),
            forward_edge(Flow,Edge,A,AA),
            back_edge(Flow,Edge,AA,A1)
        ),
        (foreach(Edge,Edges),
         fromto([],A,A1,Residual2),
         param(Hash,FlowSolution) do
            (hash_find(Hash,Edge,_) ->
                A1 = A
            ;
                store_flow(FlowSolution,Edge,0),
                forward_edge(0,Edge,A,A1)
            )
        ),
        append(Residual1,Residual2,ResidualEdges).
%        writeln(res1(Residual1)),
%        writeln(res2(Residual2)),
%        writeln(res(ResidualEdges)).

store_flow(Hash,e(_From,_To,low_high{mapping:M}),Flow):-
        hash_add(Hash,M,Flow).

forward_edge(Flow,e(From,To,low_high{high:High}),
             A,[e(From,To,1)|A]):-
        High > Flow,
        !.
forward_edge(_,_,A,A).

back_edge(Flow,e(From,To,low_high{low:Low}),
             A,[e(To,From,1)|A]):-
        Flow > Low,
        !.
back_edge(_,_,A,A).

/*
extract the list of edges whose ends are in different SCC
StrongComponents is a list of list of node indices
*/

mark_components(StrongComponents,NrNodes,MidEdges,NotMarked):-
        dim(Components,[NrNodes]),
        (foreach(Set,StrongComponents),
         count(J,1,_),
         param(Components) do
            (foreach(Sx,Set),
             param(J,Components) do
                subscript(Components,[Sx],J)
            )
        ),
        (foreach(Edge,MidEdges),
         fromto([],A,A1,NotMarked),
         param(Components) do
            marked(Edge,Components,A,A1)
        ).

% if ends are in same SCC, ignore the edge
% otherwise add to accumulator
marked(e(From,To,_W),Components,A,A):-
        subscript(Components,[From],X),
        subscript(Components,[To],X),
        !.
marked(Edge,_Components,A,[Edge|A]).

% remove values from variables corresponding to unmarked edges
remove_unmarked_edges(NotMarked,Mapping):-
        (foreach(e(VarN,ValueN,_),NotMarked),
         param(Mapping) do
            node_map(Mapping,VarN,Var),
            node_map(Mapping,ValueN,Value),
            ((atom(Var);atom(Value)) ->
                true
            ;
                
%            writeln(rem(VarN,Var,ValueN,Value)),
                exclude(Var,Value)
            )
        ).

node_map(Mapping,Node,Value):-
        arg(Node,Mapping,Value).

/************************************************************************

bool_channeling

************************************************************************/

bool_channeling(X,Coll,F):-
        collection_to_list(Coll,B),
        length(B,N),
        X #>= F,
        X #< F+N,
%        sumlist(B,1),
        check_bool_channeling(X,B,F),
        (ground(X) ->
            true
        ;
            suspend(update_bool_channeling(X,B,F,Susp),3,
                    [[X|B]->ic:min,
                     [X|B]->ic:max,
                     X->ic:hole],Susp)
        ).

:-demon(update_bool_channeling/4).
update_bool_channeling(X,B,F,Susp):-
        check_bool_channeling(X,B,F),
        (ground(X) ->
            kill_suspension(Susp)
        ;
            true
        ).

check_bool_channeling(X,B,F):-
        (foreach(V,B),
         count(J,F,_),
         param(X) do
            (V == 0 ->
                exclude(X,J)
            ; V == 1 ->
                X = J
            ; X == J ->
                V = 1
            ; is_in_domain(J,X) ->
                true
            ;
                V = 0
            )
        ),
        (integer(X) ->
            (foreach(V,B),
             count(J,F,_),
             param(X) do
                (X == J ->
                    V = 1
                ;
                    V = 0
                )
            )
        ;
            true
        ),  
        wake.

/*
check_bool_channeling(X,B,F):-
        (foreach(V,B),
         count(J,F,_),
         param(X,F,B) do
            (V == 0 ->
                exclude(X,J)
            ; V == 1 ->
                X = J,
                set_to_zero_up_to_j(B,F,J)
            ; X == J ->
                V = 1
            ; is_in_domain(J,X) ->
                true
            ;
                V = 0
            )
        ),
        wake.
*/

set_to_zero_up_to_j(_,J,J):-
        !.
set_to_zero_up_to_j([0|B],F,J):-
        F1 is F+1,
        set_to_zero_up_to_j(B,F1,J).

/************************************************************************

inverse

************************************************************************/

inverse(XL,YL):-
        collection_to_list(XL,Vars1),
        collection_to_list(YL,Vars2),
        length(Vars1,N),
        length(Vars2,N),
        append(Vars1,Vars2,Variables),
        check_inverse(Vars1,Vars2,N),
        (ground(Variables) ->
            true
        ;
            suspend(update_inverse(Variables,Vars1,Vars2,N,Susp),
                    10,[Variables->ic:min,
                       Variables->ic:max,
                       Variables->ic:hole],Susp)
        ).


:-demon(update_inverse/5).
update_inverse(Variables,Vars1,Vars2,N,Susp):-
        check_inverse(Vars1,Vars2,N),
        (ground(Variables) ->
            kill_suspension(Susp)
        ;
            true
        ).

check_inverse(Vars1,Vars2,N):-
        call_priority(check_inverse_propagate(Vars1,Vars2,N),2),
        wake.

check_inverse_propagate(Vars1,Vars2,N):-        
        dim(Matrix,[N,N]),
        (for(I,1,N),
         foreach(X,Vars1),
         param(Matrix) do
            get_domain_as_list(X,Dom),
            (foreach(V,Dom),
             param(I,Matrix) do
                subscript(Matrix,[I,V],1-_)
            )
        ),
        (for(J,1,N),
         foreach(Y,Vars2),
         param(Matrix) do
            get_domain_as_list(Y,Dom),
            (foreach(V,Dom),
             param(Y,J,Matrix) do
                subscript(Matrix,[V,J],Z-1),
                (var(Z) ->
                    exclude(Y,V)
                ;
                    true
                )
            )
        ),
        (for(I,1,N),
         foreach(X,Vars1),
         param(Matrix) do
            get_domain_as_list(X,Dom),
            (foreach(V,Dom),
             param(X,I,Matrix) do
                subscript(Matrix,[I,V],1-Z),
                (var(Z) ->
                    exclude(X,V)
                ;
                    true
                )
            )
        ).


