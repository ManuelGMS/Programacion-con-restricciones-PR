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
:- module(top).

:-export(top/0).

:-lib(ic).
:-use_module('../reorder').
:-use_module('../visualize_tree').

top:-
        nqueen(credit,32,"CREDIT"),
        true.

    
nqueen(Type,N,Output):-
        length(L,N),
        L :: 1..N,
        alldifferent(L),
        noattack(L),
        create_visualization([output:Output,
                              range_to:3000],Handle),
        add_visualizer(Handle,
                       vector(L),
                       [display:expanded]),
        N2 is N+2,
        add_visualizer(Handle,
                       vector_waterfall(L),
                       [x:N2,
                        display:minimal]),
        add_visualizer(Handle,
                       vector_size(L),
                       [y:N2,
                        display:minimal]),
        number_variables(Handle,L,Pairs),
        root(Handle),
        (Type = credit ->
            NSq is N*N,
            reorder(Pairs,Reordered),
            search(Reordered,1,first_fail,tree_indomain_middle(Handle,_),
                   credit(NSq,10),[])
        ; Type = naive ->
            search(Pairs,1,input_order,tree_indomain(Handle,_),complete,[])
        ; Type = first_fail ->
            search(Pairs,1,first_fail,tree_indomain(Handle,_),complete,[])
        ; Type = middle ->
            reorder(Pairs,Reordered),
            search(Reordered,1,first_fail,tree_indomain_middle(Handle,_),
                   complete,[])
        ;
            writeln(wrong_type(Type)),
            abort
        ),
        solution(Handle),
        close_visualization(Handle),
        true.


noattack([]).
noattack([H|T]):-
        noattack1(H,T,1),
        noattack(T).

noattack1(_,[],_).
noattack1(X,[Y|R],N):-
        X #\= Y+N,
        Y #\= X+N,
        N1 is N+1,
        noattack1(X,R,N1).

    
