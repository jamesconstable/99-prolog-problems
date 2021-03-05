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

knights_tour(N, Start, Tour) :-
  list_to_assoc([Start-_], Visited),
  knights_tour(N, Start, Tour, [], Visited, 1).

knights_tour(N, Current, [Current|Tour], Tour, _, Step) :- Step =:= N*N.
knights_tour(N, Current, [Current|Tour], TourH, Visited, Step) :-
  jump(N, Current, Next),
  \+ get_assoc(Next, Visited, _),
  put_assoc(Next, Visited, _, Visited1),
  Step1 is Step + 1,
  knights_tour(N, Next, Tour, TourH, Visited1, Step1).

jump(N, X/Y, U/V) :-
  jump_delta(DX/DY),
  U is X+DX, V is Y+DY,
  0 < U, U =< N,
  0 < V, V =< N.

jump_delta(X/Y) :- member(X, [-2, 2]), member(Y, [-1, 1]).
jump_delta(X/Y) :- member(X, [-1, 1]), member(Y, [-2, 2]).
