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
:-module(obligatory_profile).

:-export(build_possible_profile/5).
:-export(build_obligatory_profile/4).
:-export(obligatory_part/4).

:-export struct(ev(time,value)).
:-export struct(profile(from,to,value)).
:-export struct(remove(var,from,to)).

:-lib(ic_kernel).

build_possible_profile(SL,DL,RL,Limit,FlatProfile):-
        (foreach(S,SL),
         foreach(D,DL),
         foreach(R,RL),
         fromto(Events,A1,A,[]) do
            possible_part(S,D,R,A,A1)
        ),
        sort(time of ev,=<,Events,Sorted),
        combine(Sorted,Profile),
        limit_profile(Profile,Limit,LimitedProfile),
        combine_profile(LimitedProfile,FlatProfile).

% the possible part stretches from earliest start to latest end
possible_part(S,D,R,A,[ev(SMin,R),ev(T2,R1)|A]):-
        get_min(S,SMin),
        get_max(S,SMax),
        T2 is SMax+D,
        R1 is -R.

% the possible profile can be atmost as high as Limit
limit_profile(Profile,Limit,LimitedProfile):-
        (foreach(profile{from:T,value:V},Profile),
         foreach(profile{from:T,value:VV},LimitedProfile),
         param(Limit) do
            VV is min(Limit,V)
        ).

build_obligatory_profile(SL,DL,RL,FlatProfile):-
        (foreach(S,SL),
         foreach(D,DL),
         foreach(R,RL),
         fromto(Events,A1,A,[]) do
            obligatory_part(S,D,R,A,A1)
        ),
        sort(time of ev,=<,Events,Sorted),
        combine(Sorted,Profile),
        combine_profile(Profile,FlatProfile).

combine([],[]).
combine([ev(T,V)|Events],Profile):-
        combine_events(Events,T,V,Profile).

combine_events([],T,V,[profile{from:T,value:V}]).
combine_events([ev(T,V1)|R],T,V,Profile):-
        !,
        VV is V+V1,
        combine_events(R,T,VV,Profile).
combine_events([ev(T1,V1)|R],T,V,[profile{from:T,value:V}|Profile]):-
        VV is V+V1,
        combine_events(R,T1,VV,Profile).

combine_profile([],[]).
combine_profile([profile{value:0}],[]):-
        !.
combine_profile([profile{from:T,value:V},profile{value:V}|R],Flat):-
        !,
        combine_profile([profile{from:T,value:V}|R],Flat).
combine_profile([profile{from:T,value:V},profile{from:T1,value:V1}|R],
                [profile{from:T,to:T1,value:V}|Flat]):-
        V1 \= V,
        combine_profile([profile{from:T1,value:V1}|R],Flat).


obligatory_part(S,D,R,A,[ev(T1,R),ev(T2,R1)|A]):-
        obligatory_part(S,D,T1,T2),
        !,
        R1 is -R.
obligatory_part(_S,_D,_R,A,A).

obligatory_part(S,D,SMax,T2):-
        get_min(S,SMin),
        get_max(S,SMax),
        SMin+D > SMax,
        T2 is SMin+D.
