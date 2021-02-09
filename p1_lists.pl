% 1.01 (*) Find the last element of a list.

% Example:
% ?- my_last(X, [a, b, c, d]).
% X = d

my_last(X, [X]).
my_last(X, [_|T]) :- my_last(X, T).


% 1.02 (*) Find the last but one element of a list.

% Example:
% ?- last_but_one(X, [a, b, c, d]).
% X = c

last_but_one(X, [X,_]).
last_but_one(X, [_|T]) :- last_but_one(X, T).


% 1.03 (*) Find the Kth element of a list.
% The first element in the list is number 1.

% Example:
% ?- element_at(X, [a, b, c, d], 3).
% X = c

element_at(X, [X|_], 1).
element_at(X, [_|T], K) :- K > 1, K_ is K-1, element_at(X, T, K_).


% 1.04 (*) Find the number of elements of a list.

% Example:
% ?- my_length(X, [a, b, c]).
% X = 3

my_length([], 0).
my_length([_|T], L) :- my_length(T, L_), L is L_ + 1.


% 1.05 (*) Reverse a list.

% Example:
% ?- reverse([a, b, c, d], X).
% X = [d, c, b, a]

my_reverse(X, R) :- my_reverse_(X, R, []).

my_reverse_([], R, R).
my_reverse_([X|Xs], R, Acc) :- my_reverse_(Xs, R, [X|Acc]).

