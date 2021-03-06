:- ensure_loaded(p6_graphs).

% 7.01 (**) Eight queens problem.
% This is a classical problem in computer science. The objective is to place
% eight queens on a chessboard so that no two queens are attacking each other;
% i.e., no two queens are in the same row, the same column, or on the same
% diagonal.

% Hint: Represent the positions of the queens as a list of numbers 1..N.
% Example: [4, 2, 7, 3, 6, 8, 5, 1] means that the queen in the first column is
% in row 4, the queen in the second column is in row 2, etc. Use the
% generate-and-test paradigm.

eight_queens(Ys) :- n_queens(8, Ys).

n_queens(N, Ys) :- n_queens(N, 1, Ys).

n_queens(N, N, [Y]) :- between(1, N, Y).
n_queens(N, X, [Y|Ys]) :-
  X < N, X1 is X+1,
  between(1, N, Y),
  numlist(X1, N, Xs),
  n_queens(N, X1, Ys),
  maplist({X,Y}/[X2,Y2]>>(\+ attacking(X/Y, X2/Y2)), Xs, Ys).

attacking(_/Y, _/Y) :- !.                              % Same row
attacking(X/_, X/_) :- !.                              % Same column
attacking(X1/Y1, X2/Y2) :- abs(X1-X2) =:= abs(Y1-Y2).  % Same diagonal


% 7.02 (**) Knight's tour.
% Another famous problem is this one: How can a knight jump on an NxN chessboard
% in such a way that it visits every square exactly once?

% Hints: Represent the squares by pairs of their coordinates of the form X/Y,
% where both X and Y are integers between 1 and N. (Note that '/' is just a 
% convenient functor, not division!) Define the relation jump(N, X/Y, U/V) to
% express the fact that a knight can jump from X/Y to U/V on a NxN chessboard.
% And finally, represent the solution of our problem as a list of N*N knight
% positions (the knight's tour).

% This implementation is mysteriously faster than the equivalent using assocs
% for checking visitedness. Maybe they just have a very high constant overhead?

knights_tour(N, Start, Tour) :- knights_tour(N, Start, Tour, Tour, _, 1).

knights_tour(N, Current, _, [Current], _, Step) :- Step =:= N*N.
knights_tour(N, Current, Tour, [Current|Tour1], Tour2, Step) :-
  jump(N, Current, Next),
  \+ memberchk_dl(Next, Tour),
  Step1 is Step + 1,
  knights_tour(N, Next, Tour, Tour1, Tour2, Step1).

jump(N, X/Y, U/V) :-
  jump_delta(DX/DY),
  U is X+DX, V is Y+DY,
  0 < U, U =< N,
  0 < V, V =< N.

jump_delta(X/Y) :- member(X, [-2, 2]), member(Y, [-1, 1]).
jump_delta(X/Y) :- member(X, [-1, 1]), member(Y, [-2, 2]).


% 7.03 (***) Von Koch's conjecture.
% Several years ago I met a mathematician who was intrigued by a problem for
% which he didn't know a solution. His name was Von Koch, and I don't know
% whether the problem has been solved since. The problem goes like this:
% given a tree with N nodes (and hence N-1 edges), find a way to enumerate the
% nodes from 1 to N and, accordingly the edges from 1 to N-1, in such a way that
% for each edge k the difference of its node numbers equals to K. The conjecture
% is that this is always possible.

% For small trees the problem is easy to solve by hand. However, for larger
% trees, and 14 is already very large, it is extremely difficult to find a
% solution. And remember, we don't know for sure whether there is always a
% solution!

% Write a predicate that calculates a numbering scheme for a given tree. What is
% the solution for the larger tree pictured at:
% https://sites.google.com/site/prologsite/prolog-problems/7/p92b.gif

% One solution (turns out there are many):
% [a=1, b=2, c=12, d=3, e=4, f=5, g=11, h=13, i=14, k=8, m=6, n=7, p=9, q=10]

van_koch(G, Enum) :-
  graph_term(G, Ns, Es, _),
  length(Ns, N),
  N1 is N-1,
  numlist(1, N, NodeValues),
  numlist(1, N1, EdgeValues),
  van_koch(Ns, Es, Enum, NodeValues, EdgeValues).

van_koch([], [], Enum, [], []) :- close_dl(Enum).
van_koch([N|Ns], Es, Enum, AvailableNodes, AvailableEdges) :-
  select(N1, AvailableNodes, AvailableNodes1),
  map_and_check_edges(N, N1, Es, Es1, AvailableEdges, AvailableEdges1),
  memberchk(N=N1, Enum),
  van_koch(Ns, Es1, Enum, AvailableNodes1, AvailableEdges1).

map_and_check_edges(N, N1, Es, Unmapped, AvailableEs, AvailableEs1) :-
  maplist({N,N1}/[E,E1]>>map_edge(N, N1, E, E1), Es, Es1),
  partition(fully_mapped, Es1, Mapped, Unmapped),
  foldl([m(A, B, _), Es, Es1]>>(
      Diff is abs(A-B),
      memberchk(Diff, Es),
      ord_subtract(Es, [Diff], Es1)),
    Mapped, AvailableEs, AvailableEs1).

% m terms represent partially or fully mapped edges. They have one of two
% structures:
%   m(A, nil, B) -> Partially mapped: A is mapped; B is unmapped
%   m(A, B, nil) -> Fully mapped
map_edge(N, N1, E, m(N1, nil, B)) :- edge_terms(E, _, _, [N, B]), !.
map_edge(N, N1, E, m(N1, nil, A)) :- edge_terms(E, _, _, [A, N]), !.
map_edge(N, N1, m(A, nil, N), m(A, N1, nil)) :- !.
map_edge(_, _, E, E).

fully_mapped(m(_, _, nil)).


% 7.04 (***) An arithmetic puzzle.
% Given a list of integer numbers, find a correct way of inserting arithmetic
% signs (operators) such that the result is a correct equation.

% Example: Given the list of numbers [2, 3, 5, 7, 11], we can form the equations
% 2 - 3 + 5 + 7 = 11, 2 = (3 * 5 + 7) / 11, and ten others!

atoms_equation(Ns, LExpr = RExpr) :-
  non_empty_append(LHS, RHS, Ns),
  atoms_expr_tree(LHS, LExpr),
  atoms_expr_tree(RHS, RExpr),
  LExpr =:= RExpr.

atoms_expr_tree([N], N).
atoms_expr_tree(Ns, Expr) :-
  non_empty_append(LNs, RNs, Ns),
  atoms_expr_tree(LNs, LExpr),
  atoms_expr_tree(RNs, RExpr),
  member(Op, [+, -, *, /]),
  Expr =.. [Op, LExpr, RExpr].

non_empty_append(A, B, C) :- append(A, B, C), A = [_|_], B = [_|_].


% 7.05 (**) English number words.
% On financial documents, like cheques, numbers must sometimes be written in
% full words. Example: 175 must be written as one-seven-five. Write a predicate
% full_words/1 to print (non-negative) integer numbers in full words.

full_words(N) :-
  atom_chars(N, Cs),
  maplist([C, W]>>(digit_word(C, W)), Cs, Ws),
  atomic_list_concat(Ws, '-', Output),
  writeln(Output).

digit_word('0', zero).
digit_word('1', one).
digit_word('2', two).
digit_word('3', three).
digit_word('4', four).
digit_word('5', five).
digit_word('6', six).
digit_word('7', seven).
digit_word('8', eight).
digit_word('9', nine).


% 7.06 (**) Syntax checker.
% In a certain programming language (Ada) identifiers are defined by the syntax
% diagram (railroad chart) at:
% https://sites.google.com/site/prologsite/prolog-problems/7/p96.gif

% Transform the syntax diagram into a system of syntax diagrams which do not
% contain loops; i.e. which are purely recursive. Using these modified diagrams,
% write a predicate identifier/1 that can check whether or not a given string is
% a legal identifier.

identifier(Str) :- atom_chars(Str, Cs), identifier_start(Cs).

identifier_start([C|Cs]) :- char_type(C, alpha), identifier_cont(Cs).

identifier_cont([]).
identifier_cont(['_',C|Cs]) :- !, char_type(C, alnum), identifier_cont(Cs).
identifier_cont([C|Cs]) :- char_type(C, alnum), identifier_cont(Cs).


% 7.07 (**) Sudoku.
% Sudoku puzzles go like this:
%   Problem statement                Solution
%   .  .  4 | 8  .  . | .  1  7      9  3  4 | 8  2  5 | 6  1  7
%           |         |                      |         |
%   6  7  . | 9  .  . | .  .  .      6  7  2 | 9  1  4 | 8  5  3
%           |         |                      |         |
%   5  .  8 | .  3  . | .  .  4      5  1  8 | 6  3  7 | 9  2  4
%   --------+---------+--------      --------+---------+--------
%   3  .  . | 7  4  . | 1  .  .      3  2  5 | 7  4  8 | 1  6  9
%           |         |                      |         |
%   .  6  9 | .  .  . | 7  8  .      4  6  9 | 1  5  3 | 7  8  2
%           |         |                      |         |
%   .  .  1 | .  6  9 | .  .  5      7  8  1 | 2  6  9 | 4  3  5
%   --------+---------+--------      --------+---------+--------
%   1  .  . | .  8  . | 3  .  6      1  9  7 | 5  8  2 | 3  4  6
%           |         |                      |         |
%   .  .  . | .  .  6 | .  9  1      8  5  3 | 4  7  6 | 2  9  1
%           |         |                      |         |
%   2  4  . | .  .  1 | 5  .  .      2  4  6 | 3  9  1 | 5  7  8

% Every spot in the puzzle belongs to a (horizontal) row and a (vertical)
% column, as well as to one single 3x3 square (which we call "square" for
% short). At the beginning, some of the spots carry a single-digit number
% between 1 and 9. The problem is to fill the missing spots with digits in such
% a way that every number between 1 and 9 appears exactly once in each row, in
% each column, and in each square.

sudoku_solve(Puzzle, Solved) :-
  sudoku_list_to_cells(Puzzle, Fixed, Empties),
  sudoku_solve(Empties, Fixed, Solved).

sudoku_solve([], Fixed, Fixed).
sudoku_solve([E|Es], Fixed, S) :-
  between(1, 9, V), valid_cell(V-E, Fixed), sudoku_solve(Es, [V-E|Fixed], S).

valid_cell(V-P, Cells) :- \+ (member(V-P1, Cells), same_neighbourhood(P, P1)).

same_neighbourhood(C1, C2) :-
  same_column(C1, C2); same_row(C1, C2); same_square(C1, C2).

same_column(X/_, X/_).
same_row(_/Y, _/Y).
same_square(C1, C2) :- square(C1, Square), square(C2, Square).

square(X/Y, XS/YS) :- XS is X // 3, YS is Y // 3.

sudoku_list_to_cells(Vs, F, E) :- sudoku_list_to_cells(Vs, F, E, 0, 0).

sudoku_list_to_cells([], [], [], _, _).
sudoku_list_to_cells([V|Vs], Fixed, [X/Y|Empties], X, Y) :-
  var(V), !,
  increment(X, Y, X1, Y1),
  sudoku_list_to_cells(Vs, Fixed, Empties, X1, Y1).
sudoku_list_to_cells([V|Vs], [V-X/Y|Fixed], Empties, X, Y) :-
  increment(X, Y, X1, Y1),
  sudoku_list_to_cells(Vs, Fixed, Empties, X1, Y1).

increment(X, Y, X1, Y1) :- R is Y*9+X+1, X1 is R mod 9, Y1 is R // 9.

write_sudoku(Cells) :-
  predsort([O, _-X1/Y1, _-X2/Y2]>>compare(O, Y1/X1, Y2/X2), Cells, CellsSorted),
  write_sudoku(CellsSorted, 0), nl.

write_sudoku([], _).
write_sudoku(Cells, 9) :- nl, write_sudoku(Cells, 0), !.
write_sudoku([C-_|Cs], X) :- write(C), tab(1), X1 is X+1, write_sudoku(Cs, X1).

% Sample puzzle
puzzle([
  _,_,4, 8,_,_, _,1,7,
  6,7,_, 9,_,_, _,_,_,
  5,_,8, _,3,_, _,_,4,

  3,_,_, 7,4,_, 1,_,_,
  _,6,9, _,_,_, 7,8,_,
  _,_,1, _,6,9, _,_,5,

  1,_,_, _,8,_, 3,_,6,
  _,_,_, _,_,6, _,9,1,
  2,4,_, _,_,1, 5,_,_]).
