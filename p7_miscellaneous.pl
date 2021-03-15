:- use_module(library(readutil)).
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
sudoku_puzzle([
  _,_,4, 8,_,_, _,1,7,
  6,7,_, 9,_,_, _,_,_,
  5,_,8, _,3,_, _,_,4,

  3,_,_, 7,4,_, 1,_,_,
  _,6,9, _,_,_, 7,8,_,
  _,_,1, _,6,9, _,_,5,

  1,_,_, _,8,_, 3,_,6,
  _,_,_, _,_,6, _,9,1,
  2,4,_, _,_,1, 5,_,_]).


% 7.08 (***) Nonograms.
% Around 1994, a certain kind of puzzles was very popular in England. The
% "Sunday Telegraph" newspaper wrote: "Nonograms are puzzles from Japan and are
% currently published each week only in The Sunday Telegraph. Simply use your
% logic and skill to complete the grid and reveal a picture or diagram." As a
% Prolog programmer, you are in a better situation: you can have your computer
% do the work!

% The puzzle goes like this: Essentially, each row and column of a rectangular
% bitmap is annotated with the respective lengths of its distinct strings of
% occupied cells. The person who solves the puzzle must complete the bitmap
% given only these lengths.

%   Problem statement:          Solution:
%
%   |_|_|_|_|_|_|_|_| 3         |_|X|X|X|_|_|_|_| 3
%   |_|_|_|_|_|_|_|_| 2 1       |X|X|_|X|_|_|_|_| 2 1
%   |_|_|_|_|_|_|_|_| 3 2       |_|X|X|X|_|_|X|X| 3 2
%   |_|_|_|_|_|_|_|_| 2 2       |_|_|X|X|_|_|X|X| 2 2
%   |_|_|_|_|_|_|_|_| 6         |_|_|X|X|X|X|X|X| 6
%   |_|_|_|_|_|_|_|_| 1 5       |X|_|X|X|X|X|X|_| 1 5
%   |_|_|_|_|_|_|_|_| 6         |X|X|X|X|X|X|_|_| 6
%   |_|_|_|_|_|_|_|_| 1         |_|_|_|_|X|_|_|_| 1
%   |_|_|_|_|_|_|_|_| 2         |_|_|_|X|X|_|_|_| 2
%    1 3 1 7 5 3 4 3             1 3 1 7 5 3 4 3
%    2 1 5 1                     2 1 5 1

% For the example above, the problem can be stated as the two lists
% [[3], [2, 1], [3, 2], [2, 2], [6], [1, 5], [6], [1], [2]] and
% [[1, 2], [3, 1], [1, 5], [7, 1], [5], [3], [4], [3]] which give the "solid"
% lengths of the rows and columns, top-to-bottom and left-to-right,
% respectively. Published puzzles are larger than this example, e.g. 25 x 20,
% and apparently always have unique solutions.

nonogram(Rows, Cols, Solution) :-
  annotate(Rows, r, RowsAnnotated, ColLength),
  annotate(Cols, c, ColsAnnotated, RowLength),
  sort_by_num_options(RowsAnnotated, RowLength, RowsSorted),
  sort_by_num_options(ColsAnnotated, ColLength, ColsSorted),
  interleave(RowsSorted, ColsSorted, Constraints),
  nonogram_solve(Constraints, RowLength, ColLength, Solution).

annotate(Cs, D, CsAnnotated, L) :-
  length(Cs, L),
  numlist(1, L, Indices),
  maplist({D}/[I, C, A]>>(A = D-I-C), Indices, Cs, CsAnnotated).

nonogram_solve([], _, _, Solution) :- close_dl(Solution).
nonogram_solve([D-Fixed-C|Cs], RowLength, ColLength, Solution) :-
  indexed_options(D, Fixed, C, RowLength, ColLength, IndexedRow),
  maplist({Solution}/[Y/X-V]>>(memberchk(Y/X-V1, Solution), V=V1), IndexedRow),
  nonogram_solve(Cs, RowLength, ColLength, Solution).

% Indexed is a line satisfying Constraint, with each cell represented in the
% format Y/X-Value. Generates all options on backtracking.
indexed_options(r, Fixed, Constraint, Length, _, Indexed) :-
  line_options(Constraint, Length, Line),
  numlist(1, Length, Indices),
  maplist({Fixed}/[V, I, C]>>(C = Fixed/I-V), Line, Indices, Indexed).
indexed_options(c, Fixed, Constraint, _, Length, Indexed) :-
  line_options(Constraint, Length, Line),
  numlist(1, Length, Indices),
  maplist({Fixed}/[V, I, C]>>(C = I/Fixed-V), Line, Indices, Indexed).

% CsSorted is the list of constraints, Cs, sorted according to how many options
% they generate when applied to a line of length L.
sort_by_num_options(Cs, L, CsSorted) :-
  predsort(
    {L}/[Ord, _-N1-C1, _-N2-C2]>>(
      num_line_options(C1, L, O1),
      num_line_options(C2, L, O2),
      compare(Ord, O1-N1, O2-N2)),
    Cs, CsSorted).

:- table factorial/2.
:- arithmetic_function(factorial/1).
factorial(0, 1).
factorial(N, R) :- N > 0, N1 is N-1, factorial(N1, R1), R is N*R1.

:- arithmetic_function(binomial/2).
binomial(N, K, R) :- R is factorial(N) / (factorial(K) * factorial(N-K)).

% R is the number of ways of filling a line of LineLength according to the
% nonogram constraints in Ns (a list of numbers specifying the block lengths).
% This is based on the identical objects into distinct bins problem, where the
% objects are the leftover cells (those neither in a block nor the required
% single-cell separator between blocks) and the bins are the gaps around blocks.
num_line_options(Ns, LineLength, R) :-
  sum_list(Ns, S), length(Ns, L), R is binomial(LineLength - S + 1, L).

% R is a way of filling a line of LineLength according to the nonogram
% constraints in Ns (a list of numbers specifying the block lengths). Generates
% all options on backtracking. Flow order: (+, +, ?).
line_options(Ns, LineLength, R) :- line_options(Ns, LineLength, R, []).

line_options([N|[]], Length, R, RHole) :-
  constraint_options(N, Length, Length1, R, R1),
  replicate(' ', Length1, R1, RHole).
line_options([N|Ns], Length, R, RHole) :-
  min_width(Ns, EndSpace),
  OptionSpace is Length - EndSpace,
  constraint_options(N, OptionSpace, Length1, R, [' '|R1]),
  Length2 is Length1 + EndSpace - 1,
  line_options(Ns, Length2, R1, RHole).

constraint_options(N, Length, Remaining, R, RHole) :-
  RightmostStart is Length - N,
  between(0, RightmostStart, Start),
  replicate(' ', Start, R, R1),
  replicate('x', N, R1, RHole),
  Remaining is Length - (Start + N).

% Appends N copies of X to the difference list R, with new hole R1.
replicate(_, 0, R, R).
replicate(X, N, R, R1) :-
  N > 0, R = [X|RH], N1 is N-1, replicate(X, N1, RH, R1).

% R is the minimum number of cells needed to accommodate the constraints in Ns.
min_width(Ns, R) :- sum_list(Ns, S), length(Ns, L), R is S+L.

% Zs is the interleaving of Xs and Ys. Flow order: (Xs, Ys, Zs).
% If one list is shorter, the remaining elements of the other are all placed at
% the end.
interleave([], Ys, Ys) :- !.
interleave(Xs, [], Xs).
interleave([X|Xs], [Y|Ys], [X,Y|T]) :- interleave(Xs, Ys, T).

% Outputs the completed nonogram solution in a grid.
write_nonogram(N) :-
  sort(N, NSorted),
  NSorted = [Y0/_-_|_],
  foldl(
    [Y/_-V, LastY, Y]>>(
      (Y \= LastY -> nl; true),
      write(V),
      tab(1)),
    NSorted, Y0, _),
  nl.

% Sample puzzle
nonogram_puzzle(
  'Hen',
  [[3], [2,1], [3,2], [2,2], [6], [1,5], [6], [1], [2]],
  [[1,2], [3,1], [1,5], [7,1], [5], [3], [4], [3]]).


% 7.09 (***) Crossword puzzle.
% Given an empty (or almost empty) framework of a crossword puzzle and a set of
% words. The problem is to place the words into the framework.

% The crossword puzzle depicted at:
% https://sites.google.com/site/prologsite/prolog-problems/7/p99.gif
% is specified in a text file which first lists the words (one word per line) in
% an arbitrary order. Then, after an empty line, the crossword framework is
% defined. In this framework specification, an empty character location is
% represented by a dot (.). In order to make the solution easier, character
% locations can also contain predefined character values. The image puzzle is
% defined in the file p7_09a.dat, other examples are p7_09b.dat and p7_09d.dat.
% There is also an example of a puzzle (p7_09c.dat) which does not have a
% solution.

% Words are strings (character lists) of at least two characters. A horizontal
% or vertical sequence of character places in the crossword puzzle framework is
% called a site. Our problem is to find a compatible way of placing words onto
% sites.

% Hints:
% 1) The problem is not easy. You will need some time to thoroughly understand
%    it. So, don't give up too early! And remember that the objective is a clean
%    solution, not just a quick-and-dirty hack!
% 2) Reading the data file is a tricky problem for which a solution is provided
%    in the file p7_09-readfile.pl. Use the predicate read_lines/2.
% 3) For efficiency reasons it is important, at least for larger puzzles, to
%    sort the words and the sites in a particular order. For this part of the
%    problem, the solution of 1.28 may be very helpful.

solve_crossword(File, Framework) :-
  read_lines(File, Lines),
  words_framework(Lines, Words, FrameworkRaw),
  prepare_framework(FrameworkRaw, Framework),

  % Group words by length. Descending order ensures that the longest words
  % (i.e. those most likely to cause conflicts) are handled first.
  group_by_length(Words, WordsReverse),
  reverse(WordsReverse, WordsByLength),

  % Extract all slots and group by their lengths.
  across_slots(Framework, Across),
  down_slots(Framework, Down),
  append(Across, Down, Slots),
  group_by_length(Slots, SlotsByLength),

  % Try all permutations of words into suitably-sized slots.
  words_slots_allocation(WordsByLength, SlotsByLength).

% Simplified version of provided file-reading predicate using built-ins.
read_lines(File, Lines) :-
  read_file_to_string(File, CharList, []),
  split_string(CharList, "\n", "", StringLines),
  maplist(string_chars, StringLines, Lines).

words_framework(Lines, Words, Framework) :-
  append(Words, [[]|Framework], Lines).

%% prepare_framework(+Lines, -Prepared) is det
%  Expands a crossword framework so that all lines are the same length and
%  blanks are replaced by unbound variables.
prepare_framework(Lines, Prepared) :-
  maplist(deblank_line, Lines, Deblanked),
  maplist(length, Deblanked, Lengths),
  max_list(Lengths, Max),
  maplist({Max}/[L, L1]>>pad_right(Max, ' ', L, L1), Deblanked, Prepared).

deblank_line(Line, Deblanked) :-
  maplist([L, D]>>(L = '.' -> D = _; D = L), Line, Deblanked).

%% pad_right(+Length, +Pad, +List, -Padded) is det
%  Pads a list to the specified length using the given pad element. If the input
%  list is longer than the specified length, it is returned unchanged.
pad_right(0, _, L, L) :- !.
pad_right(N, Pad, [], [Pad|Padded]) :-
  N > 0, !, N1 is N-1, pad_right(N1, Pad, [], Padded).
pad_right(N, Pad, [X|Xs], [X|Padded]) :-
  N > 0, N1 is N-1, pad_right(N1, Pad, Xs, Padded).

%% across_slots(+Framework, -Slots) is det
%  Extracts the horizontal slots from the crossword framework.
across_slots(Framework, Slots) :-
  foldl(
    [Line, S, S1]>>(extract_slots(Line, LineSlots), append(S, LineSlots, S1)),
    Framework, [], AllSlots),
    exclude([S]>>length(S, 1), AllSlots, Slots).

%% down_slots(+Framework, -Slots) is det
%  Extracts the vertical slots from the crossword framework.
down_slots(Framework, Slots) :-
  transpose(Framework, Cols), across_slots(Cols, Slots).

%% extract_slots(+List, -Slots) is det
%  Extract crossword slots from the given list. Slots are made up of v(X) terms
%  and non-space chars, and separated by any number of spaces.
extract_slots([], []).
extract_slots([Current], [S]) :-
  nonvar(Current), Current = s(R), !,
  reverse(R, S).
extract_slots([Current, Space|Xs], [S|Ss]) :-
  nonvar(Current), Current = s(R),
  nonvar(Space), Space = ' ', !,
  reverse(R, S), extract_slots(Xs, Ss).
extract_slots([Current, X|Xs], Ss) :-
  nonvar(Current), Current = s(R),
  (var(X); X \= ' '), !,
  extract_slots([s([X|R])|Xs], Ss).
extract_slots([Space|Xs], Ss) :-
  nonvar(Space), Space = ' ', !,
  extract_slots(Xs, Ss).
extract_slots([X|Xs], Ss) :-
  (var(X); X \= ' ', X \= s(_)), !,
  extract_slots([s([X])|Xs], Ss).

%% transpose(?List, ?Transposed) is det
%  Transpose a list of lists of the same length.
transpose(Rows, []) :- maplist(=([]), Rows), !.
transpose(Rows, [Col|Cols]) :-
  maplist([[X|Xs], X, Xs]>>true, Rows, Col, Rows1), transpose(Rows1, Cols).

%% group_by_length(+Lists, -Groups) is det
%  Groups a list of lists by their lengths into length-group pairs.

%  ?- group_by_length([[a], [a, b, c], [d, e], [f]], G).
%  G = [1-[[a], [f]], 2-[[d, e]], 3-[[a, b, c]]].
group_by_length(Lists, Groups) :-
  map_list_to_pairs(length, Lists, KeyedByLength),
  keysort(KeyedByLength, SortedByLength),
  key_pack(SortedByLength, Groups), !.

key_pack([], []).
key_pack([K-V], [K-[V]]).
key_pack([K-V|Ps], [K-[V|Vs]|T]) :- key_pack(Ps, [K-Vs|T]).
key_pack([K1-V|Ps], [K1-[V],K2-Vs|T]) :- key_pack(Ps, [K2-Vs|T]), K1 \= K2.

%% words_slots_allocation(+Words, !Slots) is multi
%  Words is a list of words grouped by their length, and Slots is a list of
%  unbound or partially-bound slots with some shared variables between them. On
%  success, all variables in Slots will be given a valid binding from Words.
words_slots_allocation([], []).
words_slots_allocation([_-[]|Ws], Ss) :- !, words_slots_allocation(Ws, Ss).
words_slots_allocation(Ws, [_-[]|Ss]) :- !, words_slots_allocation(Ws, Ss).
words_slots_allocation([L-[Word|Words]|WordGroups], SlotGroups) :-
  ord_selectchk(L-Slots, SlotGroups, SlotGroups1),
  select(Word, Slots, Slots1),
  ord_union([L-Slots1], SlotGroups1, SlotGroups2),
  words_slots_allocation([L-Words|WordGroups], SlotGroups2).
