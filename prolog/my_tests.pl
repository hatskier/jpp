[as374118].

% -------------------- jestEFGrafem tests --------------------
writeln('-------------------- jestEFGrafem tests --------------------').

% Tests for vertices from labels
jestEFGrafem([
    node(v1, [], []),
    node(v2, [], []),
    node(v3, [], [])
  ]).
  
  not(jestEFGrafem([
      node(v1, [], []),
      node(v2, [], []),
      node(v1, [], [])
    ])).
  
  % Check if E neighbours lists are correct
  jestEFGrafem([
      node(v4, [v5], []),
      node(v5, [], []),
      node(v6, [], [])
    ]).
  
  not(jestEFGrafem([
      node(v4, [v5], []),
      node(v7, [], []),
      node(v6, [], [])
    ])).
  
  % Check if F neighbours lists are correct
  jestEFGrafem([
      node(a1, [a2], [a2]),
      node(a2, [], [a1]),
      node(a3, [], [])
    ]).
  
  
  not(jestEFGrafem([
      node(a1, [a2], [a3]),
      node(a2, [], []),
      node(a3, [], [])
    ])).
  

% -------------------- jestDobrzeUlozony tests --------------------

writeln('-------------------- jestDobrzeUlozony tests --------------------').


writeln('-- Tests for 3 edges -- ').
% tests for 3 edges

% getAllEndVertices([q1, q2, q3, q4], [
%         node(q1, [q2], [q2, q3, q4]),
%         node(q2, [q3], [q1]),
%         node(q3, [q4], [q1]),
%         node(q4, [], [q1])
%       ], X).

% getOutboundNeighbours(q4,[
%     node(q1, [q2], [q2, q3, q4]),
%     node(q2, [q3], [q1]),
%     node(q3, [q4], [q1]),
%     node(q4, [], [q1])
%   ], X).

format("Test1 ~n"), assertion(wellLayoutedEFGraph([
    node(q1, [q2], [q2, q3, q4]),
    node(q2, [q3], [q1]),
    node(q3, [q4], [q1]),
    node(q4, [], [q1])
  ])).

format("Test2 ~n"), assertion(\+ wellLayoutedEFGraph([
    node(q11, [q22], [q22, q33, q44, q55]),
    node(q22, [q33], [q11]),
    node(q33, [q44], [q11]),
    node(q44, [q55], [q11]),
    node(q55, [], [q11])
  ])).

format("Test3 ~n"), assertion(\+ wellLayoutedEFGraph([
    node(q111, [q222], [q222, q333, q444]),
    node(q222, [q333], [q111, q333, q444, q555]),
    node(q333, [q444], [q111, q222]),
    node(q444, [q555], [q111, q222]),
    node(q555, [], [q222])
  ])).

format("Test4 ~n"), assertion(wellLayoutedEFGraph([
    node(q1, [q2], [q2, q3, q4]),
    node(q2, [q3], [q1, q4, q5]),
    node(q3, [q4], [q1]),
    node(q4, [q5], [q1, q2]),
    node(q5, [], [q2])
  ])).

writeln('-- Tests for end vertex -- ').

format("Test5 ~n"), assertion(\+ wellLayoutedEFGraph([
    node(q1, [q2], [q2, q3, q4]),
    node(q2, [q3], [q1, q4, q5]),
    node(q3, [], [q1]),
    node(q4, [q5], [q1, q2]),
    node(q5, [], [q2])
  ])).

writeln('-- Tests for start vertex -- ').
format("Test6 ~n"), assertion(\+ wellLayoutedEFGraph([
    node(q1, [q2], [q2, q3, q4]),
    node(q2, [q3], [q1, q4, q5]),
    node(q3, [q4], [q1]),
    node(q4, [q5, q1], [q1, q2]),
    node(q5, [], [q2])
  ])).

writeln('-- Tests for path existance -- ').
format("Test7 ~n"), assertion(\+ fullEPathExists_(q1, q5, [
    node(q1, [q2], [q2, q3, q4]),
    node(q2, [q3], [q1, q4, q5]),
    node(q3, [q4], [q1]),
    node(q4, [q1, q2, q4], [q1, q2]),
    node(q5, [], [q2])
  ])).

format("Test8 ~n"), assertion(\+ fullEPathExists_(q1, q5, [
    node(q1, [q2, q1, q1], [q2, q3, q4]),
    node(q2, [q3, q1], [q1, q4, q5]),
    node(q3, [q4, q4, q3], [q1]),
    node(q4, [q1, q2, q4], [q1, q2]),
    node(q5, [], [q2])
  ])).

format("Test9 ~n"), assertion(fullEPathExists_(q1, q5, [
    node(q1, [q2, q1, q1], [q2, q3, q4]),
    node(q2, [q3, q1], [q1, q4, q5]),
    node(q3, [q4, q4, q3], [q1]),
    node(q4, [q1, q2, q4, q5], [q1, q2]),
    node(q5, [], [q2])
  ])).

format("Test10 ~n"), assertion(fullEPathExists_(q1, q5, [
    node(q1, [q2, q1, q1], [q2, q3, q4]),
    node(q2, [q3, q1, q5], [q1, q4, q5]),
    node(q3, [q4, q4, q3], [q1]),
    node(q4, [q1, q2, q4], [q1, q2]),
    node(q5, [], [q2])
  ])).


format("Test11 ~n"), assertion(\+ wellLayoutedEFGraph([
    node(q1, [q2], [q2, q3, q4]),
    node(q2, [q3], [q1, q4, q5]),
    node(q3, [q2], [q1]),
    node(q4, [q5], [q1, q2]),
    node(q5, [], [q2])
  ])).