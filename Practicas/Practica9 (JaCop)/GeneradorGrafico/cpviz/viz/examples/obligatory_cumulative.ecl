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
:-module(obligatory_cumulative).

:-export(cumulative/4).

:-lib(ic).
:-lib(ic_kernel).
:-use_module(obligatory_profile).

cumulative(S,D,R,Limit):-
        collection_to_list(S,SL),
        collection_to_list(D,DL),
        collection_to_list(R,RL),
        check_cumulative(SL,DL,RL,Limit),
        (ground(SL) ->
            true
        ;
            suspend(update_cumulative(SL,DL,RL,Limit,Susp),9,
                    [SL->inst,
                     SL->ic:min,
                     SL->ic:max],Susp)
        ).

:-demon(update_cumulative/5).
update_cumulative(SL,DL,RL,Limit,Susp):-
        check_cumulative(SL,DL,RL,Limit),
        (ground(SL) -> 
            kill_suspension(Susp)
        ;
            true
        ).

check_cumulative(SL,DL,RL,Limit):-
        build_obligatory_profile(SL,DL,RL,Profile),
%        writeln(Profile),
%        build_possible_profile(SL,DL,RL,Limit,Possible),
%        writeln(possible(Possible)),
        restrict_tasks(Profile,SL,DL,RL,Limit,Intervals),
        call_priority(restrict_starts(Intervals),2),
        wake.

restrict_tasks(Profile,SL,DL,RL,Limit,Intervals):-
        (foreach(Step,Profile),
         fromto([],A,A1,Intervals),
         param(SL,DL,RL,Limit) do
            (foreach(S,SL),
             foreach(D,DL),
             foreach(R,RL),
             fromto(A,AA,AA1,A1),
             param(Step,Limit) do
                check_task_restriction(Step,S,D,R,Limit,AA,AA1)
            )
        ).

check_task_restriction(profile{from:From,to:To,value:Value},S,D,R,
                       Limit,
                       AA,[remove{var:S,from:X1,to:X2}|AA]):-
        R + Value > Limit,
        get_min(S,SMin),
        SMin < To,
        get_max(S,SMax),
        SMax+D > From,
        not obligatory_part_in_interval(S,D,From,To),
        !,
        X1 is max(SMin,From-D+1),
        X2 is min(SMax,To-1).
check_task_restriction(_Step,_S,_D,_R,_limit,AA,AA).

obligatory_part_in_interval(S,D,From,To):-
        obligatory_part(S,D,T1,T2),
        T1 < To,
        T2 > From.

restrict_starts([]).
restrict_starts([remove{var:Var,from:From,to:To}|R]):-
        exclude_range(Var,From,To),
        restrict_starts(R).
