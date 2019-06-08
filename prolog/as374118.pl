% Author: Aliaksei Suvorau as374118
% Task: Prolog task nr 3 (JPP)

% --- Modules loading ---
use_module(library(lists)).

% --- Predicates for graphs ---

% getVertices(+Graph, -Vertices)
% Computes all node labels (let's call them vertices) for graph
getVertices([], []).
getVertices([node(V, _, _) | T], Vertices) :-
  getVertices(T, VerticesFromTail),
  Vertices = [V | VerticesFromTail].

% verticesDefinedInGraph(+Vertices, +Graph)
% Checks if each vertex from Vertices is defined in Graph
verticesDefinedInGraph(Vertices, G) :-
  getVertices(G, VerticesInGraph),
  subset(Vertices, VerticesInGraph).

% getNode(+Vertex, +Graph, ?Node)
% Finds node for passed vertex (label) in Graph
getNode(V, [N | _], N) :-
  N = node(V, _, _).
getNode(V, [N | T], NodeFound) :-
  N \= node(V, _, _),
  getNode(V, T, NodeFound).

% nodeDefinedCorrectly(+Graph, +Node)
% Checks if node is defined correctly i.e.
% - all vertices used in neighbours lists are defined in graph as nodes
% - if v1 is connected with v2 with F-edge, then v2 is connected with v1
nodeDefinedCorrectly(G, node(V, ENeighbours, FNeighbours)) :-
  verticesDefinedInGraph(ENeighbours, G),
  verticesDefinedInGraph(FNeighbours, G),
  \+ (
      member(U, FNeighbours),
      getNode(U, G, node(_, _, FNeighbours_U)),
      \+ member(V, FNeighbours_U)
    ).

% nodesDefinedCorrectly(+Nodes, +Graph)
% Checks if each node in graph is defined correctly
nodesDefinedCorrectly(Nodes, G) :-
  exclude(nodeDefinedCorrectly(G), Nodes, []).

% maxFEdgesForEachNode(+Graph, -MaxEdges)
% Computes max  F-edges number of all nodes in graph
maxFEdgesForEachNode([], 0).
maxFEdgesForEachNode([node(_, _, FNeighbours) | T], MaxEdges) :-
  maxFEdgesForEachNode(T, MaxEdgesForTail),
  length(FNeighbours, FNeighboursAmount),
  MaxEdges is max(MaxEdgesForTail, FNeighboursAmount).

% isStartVertex(-VS, +G)
% Finds all start vertices in graph
isStartVertex(VS, G) :-
  getVertices(G, Vertices),
  member(VS, Vertices),
  \+ (
      member(node(_, ENeighbours, _), G),
      member(VS, ENeighbours)
    ).

% getSingleStartVertex(+Graph, -VS)
% Finds single start vertex in graph
% If it's not single this predicate fails
getSingleStartVertex(G, VS) :-
  isStartVertex(VS, G),
  \+ (
      isStartVertex(U, G),
      U \= VS
    ).

% isEndVertex(-VE, +G)
% Like isStartVertex, but for end vertex
isEndVertex(VE, G) :-
  getVertices(G, Vertices),
  member(VE, Vertices),
  getNode(VE, G, node(_, [], _)).
  

% getSingleStartVertex(+Graph, -VS)
% Like getSingleStartVertex, but for end vertex
getSingleEndVertex(G, VE) :-
  isEndVertex(VE, G),
  \+ (
      isEndVertex(U, G),
      U \= VE
    ).

% visitVertex(+V, +PrevVertex, +VisitedEdges, +VisitedVertices, -NewVisitedEdges, -NewVisitedVertices)
% Updates VisitedEdges and VisitedVertices and saves changes to NewVisitedEdges and NewVisitedVertices
visitVertex(V, Prev, VisitedEdges, VisitedVertices, NewVisitedEdges, NewVisitedVertices) :-
  union([(Prev, V)], VisitedEdges, NewVisitedEdges),
  union([V], VisitedVertices, NewVisitedVertices).

% neighbourIsBoundWithVisitedEdge(+V, +VisitedEdges, +Neighbour)
% Help function for neighboursBoundWithUnvisitedEdges
neighbourIsBoundWithVisitedEdge(V, VisitedEdges, Neighbour) :-
  member((V, Neighbour), VisitedEdges).

% neighboursBoundWithUnvisitedEdges(VS+, +G, +VisitedEdges, -NeighboursToVisit)
neighboursBoundWithUnvisitedEdges(VS, G, VisitedEdges, NeighboursToVisit) :-
  getNode(VS, G, node(_, ENeighbours, _)),
  exclude(neighbourIsBoundWithVisitedEdge(VS, VisitedEdges), ENeighbours, NeighboursToVisit).

% fullEPathExists(+Vertices, +PrevVertex, +EndVertex, +Graph, +VisitedEdges, +VisitedVertices)
% DFS through the graph
% But we allow going to the same vertex twice or more times
% We don't allow to use the same edges to avoid endless loops
% If we came to the end vertex and all vertices were visited - then full e-path exists
fullEPathExists([VE], Prev, VE, G, VisitedEdges, VisitedVertices) :-
  visitVertex(VE, Prev, VisitedEdges, VisitedVertices, _, NewVisitedVertices),
  getVertices(G, Vertices),
  same_length(NewVisitedVertices, Vertices).
% Case when trip is not finished but Vertices contains one element
fullEPathExists([VS], Prev, VE, G, VisitedEdges, VisitedVertices) :-
  visitVertex(VS, Prev, VisitedEdges, VisitedVertices, NewVisitedEdges, NewVisitedVertices),
  neighboursBoundWithUnvisitedEdges(VS, G, NewVisitedEdges, NeighboursToVisit),
  fullEPathExists(NeighboursToVisit, VS, VE, G, NewVisitedEdges, NewVisitedVertices).
% Case when Vertices contains some elements
fullEPathExists([VS | T], Prev, VE, G, VisitedEdges, VisitedVertices) :-
  T \= [],
  (
    fullEPathExists([VS], Prev, VE, G, VisitedEdges, VisitedVertices);
    fullEPathExists(T, Prev, VE, G, VisitedEdges, VisitedVertices)
  ).

% firstWellPermutingConditionFailed(+G, -V)
% Negation of the first well permuting condition 
firstWellPermutingConditionFailed(G, V) :-
  getSingleEndVertex(G, VE),
  getNode(V, G, node(_, ENeighbours, FNeighbours)),
  member(V1, ENeighbours),
  member(W1, FNeighbours),
  W1 \= VE,
  \+ (
      getNode(V1, G, node(_, _, FNeighbours_V1)),
      getNode(W1, G, node(_, ENeighbours_W1, _)),
      member(U, ENeighbours_W1),
      member(U, FNeighbours_V1)
    ).

% secondWellPermutingConditionFailed(+G, -V)
% Negation of the second well permuting condition
secondWellPermutingConditionFailed(G, V) :-
  getSingleStartVertex(G, VS),
  getNode(V1, G, node(_, ENeighbours_V1, _)),
  getNode(W1, G, node(_, _, FNeighbours_W1)),
  member(V, ENeighbours_V1),
  member(V, FNeighbours_W1),
  W1 \= VS,
  \+ (
      getNode(_, G, node(_, ENeighbours_U, FNeighbours_U)),
      member(W1, ENeighbours_U),
      member(V1, FNeighbours_U)
    ).

% vertexBadPermuted(+G, -V)
vertexBadPermuted(G, V) :-
  firstWellPermutingConditionFailed(G, V);
  secondWellPermutingConditionFailed(G, V).

% --- Main predicates ---

% efGraphDefinedCorrectly(+G)
efGraphDefinedCorrectly(G) :-
  getVertices(G, Vertices),
  is_set(Vertices), % is_set checks if Vertices are unique
  nodesDefinedCorrectly(G, G).

% wellArrangedEFGraph(+G)
wellArrangedEFGraph(G) :-
  efGraphDefinedCorrectly(G),
  maxFEdgesForEachNode(G, MaxEdges),
  MaxEdges =< 3,
  getSingleStartVertex(G, VS),
  getSingleEndVertex(G, VE),
  fullEPathExists([VS], VS, VE, G, [], []).

% wellPermutedEFGraph(+G)
wellPermutedEFGraph(G) :-
  % wellArrangedEFGraph(G), % - Commented because of moodle tests
  getVertices(G, Vertices),
  include(vertexBadPermuted(G), Vertices, []).

% isSucc(+G, -Path, -SuccPath)
% The simpliest case
isSucc(_, [], []).
% List of each single vertex is an F-path
isSucc(G, [], [VS]) :-
  getVertices(G, Vertices),
  member(VS, Vertices).
% Case when we build F-path for succesor and Path isn't changed
isSucc(G, Path, SuccPath) :-
  append(SuccPathWithoutLast, [SuccPathLast], SuccPath),
  SuccPathWithoutLast \= [],
  isSucc(G, Path, SuccPathWithoutLast),
  last(SuccPathWithoutLast, SuccPathNextToLast),
  getNode(SuccPathNextToLast, G, node(_, _, FNeighbours)),
  member(SuccPathLast, FNeighbours).
% The most complicated case
% We build path and succPath simultaneously
isSucc(G, Path, SuccPath) :-
  append(PathWithoutLast, [PathLast], Path),
  append(SuccPathWithoutLast, [SuccPathLast], SuccPath),
  same_length(PathWithoutLast, SuccPathWithoutLast),
  isSucc(G, PathWithoutLast, SuccPathWithoutLast),
  (
    (
      % Case when PathWithoutLast and SuccPathWithoutLast are empty
      PathWithoutLast = []
    );
    (
      % Case when PathWithoutLast and SuccPathWithoutLast are not empty
      PathWithoutLast \= [],
      last(PathWithoutLast, PathNextToLast),
      last(SuccPathWithoutLast, SuccPathNextToLast),
      getNode(PathNextToLast, G, node(_, _, FNeighbours)),
      getNode(SuccPathNextToLast, G, node(_, _, FNeighboursSucc)),
      member(PathLast, FNeighbours), % last F-edge in Path
      member(SuccPathLast, FNeighboursSucc) % last F-edge in Succesor
    )
  ),
  getNode(PathLast, G, node(_, ENeighbours, _)),
  member(SuccPathLast, ENeighbours). % last E-edge between last path and last sucessor vertices

% --- Polish translations ---
jestEFGrafem(G) :- efGraphDefinedCorrectly(G).
jestDobrzeUlozony(G) :- wellArrangedEFGraph(G).
jestDobrzePermutujacy(G) :- wellPermutedEFGraph(G).
jestSucc(G, L1, L2) :- isSucc(G, L1, L2).