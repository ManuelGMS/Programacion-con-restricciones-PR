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
:-module(group).

:-export(group/3).

group(List,Arg,Groups):-
        integer(Arg),
        !,
        sort(Arg,=<,List,[H|T]),
        extract_arg(Arg,H,V),
        lp(T,Arg,[H],V,Groups).
group(List,Arg1+Arg2,Groups):-
        integer(Arg1),
        integer(Arg2),
        !,
        sort(Arg1,=<,List,List1),
        sort(Arg2,=<,List1,[H|T]),
        extract_arg(Arg1+Arg2,H,V),
        lp(T,Arg1+Arg2,[H],V,Groups).

lp([],_,Last,LastV,[LastV-Last]).
lp([H|T],Arg,Old,V,Groups):-
        extract_arg(Arg,H,V),
        !,
        lp(T,Arg,[H|Old],V,Groups).
lp([H|T],Arg,Old,OldV,[OldV-Old|Groups]):-
        extract_arg(Arg,H,V),
        lp(T,Arg,[H],V,Groups).

extract_arg(0,Term,V):-
        !,
        Term = V.
extract_arg(A,Term,V):-
        integer(A),
        !,
        arg(A,Term,V).
extract_arg(A1+A2,Term,V1+V2):-
        arg(A1,Term,V1),
        arg(A2,Term,V2).
