:-module(max_flow_old).
:-comment(summary,"Ford-Fulkerson maximum flow algorithm").
:-comment(author,"CrossCore Optimization Ltd").
:-comment(copyright,"2007, CrossCore Optimization Ltd").
:-comment(status,prototype).
:-comment(date,"2006-2007").

:-lib(graph_algorithms).
:-lib(hash).

:-export(max_flow/5).
:-comment(max_flow/5,
          [
              summary:"Ford-Fulkerson maximum flow algorithm",
              amode:max_flow(+,+,+,+,-),
              args:[
                       "Graph": "a graph structure, no parallel edges,"
                                " e(Src,Dest,EdgeData)", 
                       "CapacityArg": "which argument of EdgeData to use as"
                                      " edge capacity (integer), (0 if"
                                      " EdgeData is a single number and -1"
                                      " if every edge capacity is 1)",
                       "SourceNode": "source node number (integer)",
                       "SinkNode": "sink node number (integer)",
                       "MaxFlowValue": "value of the maximum flow"
                   ],
              desc:html("This predicate provides an implementation of"
                        " the Ford-Fulkerson max-flow algorithm"
                        " between two nodes in a graph. It the returns"
                        " the maximal achievable flow allowed by the"
                        " capacities in the network."),
              see_also:[max_flow:max_flow/5,
                        max_flow:max_flow/7,
                        max_flow_eplex:max_flow_eplex/5,
                        max_flow_eplex:max_flow_eplex_dual/5,
                        max_flow_eplex:max_flow_eplex_dual/7,
                        all_min_cuts:all_min_cuts/8,
                        all_min_cuts:all_min_cuts/9,
                        all_min_cuts:all_min_cuts_list/5,
                        all_min_cuts_eplex:all_min_cuts_eplex/7,
                        all_min_cuts_eplex:all_min_cuts_eplex/8
                       ]
                        
          ]
         ).

:-export(max_flow/7).
:-comment(max_flow/7,
          [
              summary:"Ford-Fulkerson maximum flow algorithm",
              amode:max_flow(+,+,+,+,-,-,-),
              args:[
                       "Graph": "a graph structure, no parallel edges,"
                                " e(Src,Dest,EdgeData)", 
                       "CapacityArg": "which argument of EdgeData to use as"
                                      " edge capacity (integer), (0 if"
                                      " EdgeData is a single number and -1"
                                      " if every edge capacity is 1)",
                       "SourceNode": "source node number (integer)",
                       "SinkNode": "sink node number (integer)",
                       "MaxFlowValue": "value of the maximum flow",
                       "MaxFlowEdges": "list denoting edges with non-zero"
                                       " flow (form: Flow-Edge)",
                       "MaxFlowEdgesGraph": "a graph structure, original"
                                            " nodes (as in Graph) but only"
                                            " the edges that are in max flow"
                   ],
              desc:html("This predicate provides an implementation of"
                        " the Ford-Fulkerson max-flow algorithm"
                        " between two nodes in a graph. It the returns"
                        " the maximal achievable flow allowed by the"
                        " capacities in the network, a list of all"
                        " edges with non-zero flow, and a graph of the"
                        " edges with non-zero flow."),
              see_also:[max_flow:max_flow/5,
                        max_flow:max_flow/7,
                        max_flow_eplex:max_flow_eplex/5,
                        max_flow_eplex:max_flow_eplex_dual/5,
                        max_flow_eplex:max_flow_eplex_dual/7,
                        all_min_cuts:all_min_cuts/8,
                        all_min_cuts:all_min_cuts/9,
                        all_min_cuts:all_min_cuts_list/5,
                        all_min_cuts_eplex:all_min_cuts_eplex/7,
                        all_min_cuts_eplex:all_min_cuts_eplex/8
                       ]
          ]
         ).


max_flow(Graph, CapacityArg, SourceNode, SinkNode, MaxFlowValue):-
		do_max_flow(Graph, CapacityArg, SourceNode, SinkNode, MaxFlowValue, _, _, _). 
        
max_flow(Graph, CapacityArg, SourceNode, SinkNode, MaxFlowValue,
         MaxFlowEdges, MaxFlowEdgesGraph):-
        max_flow(Graph, CapacityArg, SourceNode, SinkNode, MaxFlowValue,
                 MaxFlowEdges, MaxFlowEdgesGraph,_).
        
max_flow(Graph, CapacityArg, SourceNode, SinkNode, MaxFlowValue,
         MaxFlowEdges, MaxFlowEdgesGraph, ResidualCapacities):-
        do_max_flow(Graph, CapacityArg, SourceNode, SinkNode, MaxFlowValue, 
                    N, Edges, ResidualCapacities),
        get_max_flow_edges(N, Edges, CapacityArg,ResidualCapacities,
                           MaxFlowEdges,MaxFlowEdgesGraph). 

do_max_flow(Graph, CapacityArg, SourceNode, SinkNode, MaxFlowValue, N, Edges, ResidualCapacities) :-
        graph_get_maxnode(Graph,N),
        graph_get_all_edges(Graph,Edges),
        initialize_residual_capacities(N,Edges,CapacityArg,ResidualCapacities),
        max_flow_aux(N,ResidualCapacities,SourceNode,SinkNode,0,MaxFlowValue). 

max_flow_aux(N,ResidualCapacities,SourceNode, SinkNode, 
             FlowValue, MaxFlowValue):-
        
        dim(NodeLabels,[N]),
        dim(Predecessors,[N]),
        initialize_node_labels(NodeLabels),
        initialize_predecessor_array(Predecessors),
        
        label(NodeLabels,SourceNode),
        (
            find_path(ResidualCapacities, SinkNode,
                      NodeLabels, Predecessors, [SourceNode])
        ->
            augment(ResidualCapacities,SourceNode, SinkNode,
                    Predecessors,FlowValue,NewFlowValue), 
            max_flow_aux(N,ResidualCapacities, SourceNode,
                         SinkNode, NewFlowValue, MaxFlowValue) 
        ;
            MaxFlowValue = FlowValue
        ).
        
        
find_path(_ResidualCapacities, SinkNode, NodeLabels,
          _Predecessors, 
          _List):-
        labeled(NodeLabels,SinkNode),
        !.

find_path(ResidualCapacities, SinkNode, NodeLabels,
          Predecessors, 
          List):-
        List = [I|Rest],
        get_valid_adjacent_nodes(ResidualCapacities,I,Js),
        (
            foreach(J,Js),
            fromto(Rest,In,Out,NewList),
            param(NodeLabels, Predecessors, I)
        do
            (
                unlabeled(NodeLabels,J)
            ->
                setarg(J,Predecessors,I),
                label(NodeLabels,J),
                Out = [J|In]
            ;
                In = Out
            )
        ),
        
        find_path(ResidualCapacities, SinkNode, NodeLabels,
                  Predecessors, 
                  NewList).
        
            
augment(ResidualCapacities, SourceNode, SinkNode,
        Predecessors, FlowValue, NewFlowValue):-
        arg(SinkNode,Predecessors,Pred),
        
        % residual_capacity of the last edge of the path
        get_edge_residual_capacity(ResidualCapacities,Pred,SinkNode,
                                   ResCapacity),
        
        augment_aux(ResidualCapacities,SourceNode,
                    Predecessors,Pred,ResCapacity,
                    [e(Pred,SinkNode,ResCapacity)],
                    MinResCapacity, Path), 

        update_residual_capacities(ResidualCapacities, Path, MinResCapacity),
        NewFlowValue is FlowValue + MinResCapacity.
        

augment_aux(_ResidualCapacities,SourceNode,_Predecessors,
            SourceNode,MinResCapacity,Path, MinResCapacity, Path):- 
        % source node reached, stop
        !.
augment_aux(ResidualCapacities,SourceNode,Predecessors,CurrentNode,
            CurrentMinResCapacity, PartialPath, MinResCapacity,Path):-  
        arg(CurrentNode,Predecessors,Pred),
        get_edge_residual_capacity(ResidualCapacities,Pred,CurrentNode,
                                   ResCapacity),
        
        (
            ResCapacity < CurrentMinResCapacity
        ->
            NewMinResCapacity = ResCapacity
        ;
            NewMinResCapacity = CurrentMinResCapacity
        ),
        augment_aux(ResidualCapacities,SourceNode,Predecessors,Pred,
                    NewMinResCapacity, [e(Pred,CurrentNode,
                                          ResCapacity)|PartialPath],  
                    MinResCapacity,Path).        

        

%% About the residual graph / residual capacity data structure:

%% ResidualCapacities is an array (arg = source node S) where
%% each item is a hash table (hash key = dest node D of an adjacent edge
%% for S, hash value = residual capacity for edge (S,D)

%% When finding a path, we get the valid adjacent edges from this data, and
%% no additional residual graph representation is needed.


%% Why is it done in this way instead of using residual graph as a graph?
%% Because of all tested residual graph / residual capacity
%% data structures, this appeared to be clearly the fastest representation
%% for this algorithm, where residual capacities of edges are continuously
%% updated (maybe you have a more elegant way to do it in the source of
%% graph_algorithms?) 




initialize_residual_capacities(N,Edges,CapacityArg,
                               ResidualCapacities):- 
        dim(ResidualCapacities,[N]),
        (
            foreacharg(AdjEdges,ResidualCapacities)
        do
            hash_create(AdjEdges)
        ),

        (
            foreach(e(Src,Dst,Info),Edges),
            param(CapacityArg,ResidualCapacities)
        do
            capacity(CapacityArg,Info,Capacity),
            arg(Src,ResidualCapacities,AdjEdges),
            (
		hash_get(AdjEdges,Dst,0)
            ->
                % arc added already as opposite edge
                hash_set(AdjEdges,Dst,Capacity)
            ;
                % arc not added yet
                hash_add(AdjEdges,Dst,Capacity),
                % and the opposite
		arg(Dst,ResidualCapacities,OppAdjEdges),
                hash_add(OppAdjEdges,Src,0)
            )
        ).



capacity(-1,_EdgeInfo,1):-!.
capacity(0,EdgeInfo,EdgeInfo):-!.
capacity(CapacityArg,EdgeInfo,Capacity):-
        CapacityArg > 0,
        !,
        arg(CapacityArg,EdgeInfo,Capacity).
capacity(_,_,_):-!,fail.

        
get_edge_residual_capacity(ResidualCapacities,Src,Dest,ResCapacity):-
	arg(Src,ResidualCapacities,AdjEdges),
	hash_get(AdjEdges,Dest,ResCapacity).

initialize_node_labels(NodeLabels):-
        (
            foreacharg(0,NodeLabels) 
		do
            true
        ).
        

initialize_predecessor_array(Predecessors):-
        (
            foreacharg(0,Predecessors)
        do
            true
        ).
        
        
label(NodeLabels,Node):-
        setarg(Node,NodeLabels,1).

labeled(NodeLabels,Node):-
        arg(Node,NodeLabels,Val),
        Val == 1.
unlabeled(NodeLabels,Node):-
        arg(Node,NodeLabels,Val),
        Val == 0.
        
        
update_residual_capacities(ResidualCapacities, Path, PathSize):-      
        (
            foreach(e(S,D,ResCapacity),Path),
            param(ResidualCapacities,PathSize)
        do
            NewResCapacity is ResCapacity - PathSize,
            arg(S,ResidualCapacities,AdjEdges),
            hash_set(AdjEdges,D,NewResCapacity),

            % and _add_ PathSize to opposite direction
	    arg(D,ResidualCapacities,OppAdjEdges),
	    hash_get(OppAdjEdges,S,OppResCapacity),

            NewOppResCapacity is OppResCapacity + PathSize,
            hash_set(OppAdjEdges,S,NewOppResCapacity)
        ).
  
  
get_valid_adjacent_nodes(ResidualCapacities,Src,Neighbours):-
        arg(Src,ResidualCapacities,AdjEdges),
        hash_list(AdjEdges,Dests,ResCapacities),
        (
            fromto([],In,Out,Neighbours),
            foreach(D,Dests),
            foreach(ResCapacity,ResCapacities)
        do
            (
                ResCapacity > 0
            ->
                Out = [D|In]
            ;
                Out = In
            )
        ).

        
get_max_flow_edges(N, Edges, CapacityArg,ResidualCapacities, MaxFlowEdges, 
                   MaxFlowEdgesGraph):-
        (
            foreach(e(Src,Dst,Info),Edges),
            fromto([],In,Out,MaxFlowEdges),
            fromto([],In1,Out1,MaxFlowJustEdges),
            param(CapacityArg,ResidualCapacities)
        do
            capacity(CapacityArg,Info,Capacity),
            get_edge_residual_capacity(ResidualCapacities,Src,Dst,ResCapacity),
            (
                ResCapacity < Capacity
            ->
                Flow is Capacity - ResCapacity,
                Out = [Flow-e(Src,Dst,Info)|In],
                Out1 = [e(Src,Dst,Info)|In1]
            ;
                Out = In,
                Out1 = In1
            )
        ),
        make_graph(N,MaxFlowJustEdges,MaxFlowEdgesGraph).


transform_graph_for_feasibility_solve(Graph, LwBArg, CapacityArg, Src, Sink, AddedEdge, TSrc, TSink, TGraph) :-
	graph_get_maxnode(Graph, N),
	TSrc is N + 1,
	TSink is N + 2,
        % get incoming edges now, as we will need them (first call computes
        % them. Also, we need the edge data to create new edge data for the
        % new edges in the transformed graph. Use Sink because it should
        % have incoming nodes!
        graph_get_incoming_edges(Graph, Sink, [e(_,_,Data0)|_]), % could fail
        functor(Data0,DataName,DataArity),
        (for(I,1,N), param(Graph,TSrc,TSink,LwBArg,CapacityArg,
                           Src,Sink,DataName,DataArity, AddedEdge),
	 fromto(NewEdges, Es0,Es, []) do
            graph_get_incoming_edges(Graph, I, InEs),	
            (foreach(e(_,_,Data), InEs), 
             param(LwBArg),
             fromto(0, In0,In1, In) do
                In1 is In0 + arg(LwBArg, Data)
            ),
            graph_get_adjacent_edges(Graph, I, OutEs),
            (foreach(e(_,_,Data), OutEs),
             param(LwBArg), 
             fromto(0, Out0,Out1, Out) do
                Out1 is Out0 + arg(LwBArg, Data)
            ),
            (I == Sink -> 
                (graph_get_edge(Graph, Sink, Src, _) ->
                    % already have an edge from Sink to Source
					AddedEdge = 0,
                    Es0 = Es1
                ;
                    functor(Data1, DataName, DataArity),
                    arg(CapacityArg, Data1, 1.0Inf),
                    arg(LwBArg, Data1, 0),
					AddedEdge = 1,
                    Es0 = [e(Sink, Src, Data1)|Es1]
                )
            ;
                Es0 = Es1
            ),
            Bi is In - Out,
            ( Bi > 0 ->
                functor(NData,DataName,DataArity),
                Es1 = [e(TSrc,I,NData)|Es],
                arg(CapacityArg, NData, Bi),
                arg(LwBArg, NData, 0)
            ; Bi < 0 ->
                functor(NData,DataName, DataArity),
                Bi1 is -Bi,
                Es1 = [e(I,TSink, NData)|Es],
                arg(CapacityArg, NData,  Bi1),
                arg(LwBArg, NData, 0)
            ; 
                Es1 = Es
            )
	),
	graph_get_all_edges(Graph, Edges0),
	(foreach(E0, Edges0),
	 param(CapacityArg,LwBArg,DataName,DataArity),
	 fromto(TEdges, TE0,TE1, NewEdges) do
		E0 = e(S,D,Data),
		arg(LwBArg, Data, Low),
		(Low > 0 ->
			TCap is arg(CapacityArg, Data) - Low,
			functor(TData, DataName, DataArity),
			arg(LwBArg, TData, Low),
			arg(CapacityArg, TData, TCap),
			TE0  = [e(S,D,TData)|TE1]
		;
			TE0 = [E0|TE1]
		)
	),
			
	make_graph(TSink, TEdges, TGraph).

:- export feas_flow_with_lb/8.

feas_flow_with_lb(Graph, LwBArg, CapacityArg, Src, Sink, FeasFlowVal, FlowEdges, FlowGraph) :-
	transform_graph_for_feasibility_solve(Graph, LwBArg, CapacityArg, 
                                              Src, Sink, AddedEdge, TSrc, TSink, TGraph),
	do_max_flow(TGraph, CapacityArg, TSrc, TSink, _, N, Edges, ResCaps),
	check_if_feas_and_extract_orig(Graph, N, Edges, ResCaps, TSrc, TSink, Src,
                                       Sink, LwBArg, CapacityArg, AddedEdge,
                                       FeasFlowVal, FlowEdges, FlowGraph).  % fail if infeasible
	
check_if_feas_and_extract_orig(Graph,TN, TEdges, ResCaps, TSource, TSink, Source,
                               Sink, LwBArg, CapacityArg, AddedEdge, FeasFlowVal,
                               FlowEdges, FlowGraph) :-	
	(foreach(Edge, TEdges),
         param(Graph,TSource,TSink,Source,Sink,CapacityArg,LwBArg,ResCaps,AddedEdge),
         fromto(FlowEdges, FE0,FE1, []),
         fromto(Edges, E0,E1, []), 
		 fromto(0, FV0,FV1, FeasFlowVal) do
            Edge = e(S,D,TData),
            get_edge_residual_capacity(ResCaps,S,D,ResCapacity),
            arg(CapacityArg, TData, Capacity),
            ( ResCapacity < Capacity -> TFlow is Capacity - ResCapacity ; TFlow = 0 ),
            ( S == TSource ->
            /* added transformed edge, check feasibility */
                Capacity =:= TFlow, 
                FE0 = FE1,
                E0 = E1,
				FV1 = FV0
            ; D == TSink ->
                Capacity =:= TFlow,
                FE0 = FE1,
                E0 = E1,
				FV1 = FV0
            ; S == Sink, D == Source ->
	    /* edge from Sink to Source needs special treatment */
                ( AddedEdge == 1 ->	
                    % edge added during transformation, has no lower bound
                    FE0 = FE1,			
                    E0 = E1,
					FV1 = FV0
                ;
                    % edge is original, get actual flow on it
					arg(LwBArg, TData, Low),
					Flow is TFlow + Low,
					(Flow > 0 ->
						(Low > 0 ->
							% need to get original edge
							graph_get_edge(Graph, S, D, OrigEdge)
						;
							% assume reuse of original edge if Low == 0 in transformation
							OrigEdge = Edge
						),
						FE0 = [Flow-OrigEdge|FE1],
						E0 = [OrigEdge|E1],					
						FV1 = FV0
                    ;
                        FE0 = FE1,
                        E0 = E1, 
						FV1 = FV0
                    )
                )
            ;
            /* flow in original problem, convert it to actual flow */
				arg(LwBArg, TData, Low),
                Flow is TFlow + Low,
                (Flow > 0 ->
					(Low > 0 ->
						% need to get original edge
						graph_get_edge(Graph, S, D, OrigEdge)
					;
						% assume reuse of original edge if Low == 0 in transformation
						OrigEdge = Edge
					),
                    FE0 = [Flow-OrigEdge|FE1],
                    E0 = [OrigEdge|E1],
					(D == Sink -> FV1 is FV0 + Flow ; FV1 = FV0)
                ;
                    FE0 = FE1,
                    E0 = E1,
					FV1 = FV0
                )
            )
        ),
	N is TN - 2, % original without TSrc and TSink
        make_graph(N, Edges, FlowGraph).




%% the following are exported for module all_min_cuts


:-comment(initialize_residual_capacities/4,hidden).
:-export(initialize_residual_capacities/4).

:-comment(max_flow_aux/6,hidden).
:-export(max_flow_aux/6).

:-comment(get_max_flow_edges/6,hidden).
:-export(get_max_flow_edges/6).

:-comment(max_flow/8,hidden).
:-export(max_flow/8).



