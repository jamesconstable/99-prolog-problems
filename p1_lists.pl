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


% 1.06 (*) Find out whether a list is a palindrome.

% Example:
% ?- is_palindrome([x, a, m, a, x]).
% true.

is_palindrome(X) :- my_reverse(X, X).


% 1.07 (**) Flatten a nested list structure.

% Example:
% ?- my_flatten([a, [b, [c, d], e]], X).
% X = [a, b, c, d, e]

my_flatten(X, [X]) :- \+ is_list(X).
my_flatten([], []).
my_flatten([X|Xs], Y) :-
  my_flatten(X, X_), my_flatten(Xs, Xs_), append(X_, Xs_, Y).


% 1.08 (**) Eliminate consecutive duplicates of list elements.

% Example:
% ?- compress([a, a, a, a, b, c, c, a, a, d, e, e, e, e], X).
% X = [a, b, c, d, e]

compress([], []).
compress([X], [X]).
compress([X,X|Xs], Y) :- compress([X|Xs], Y).
compress([X,Z|Xs], [X|Y]) :- X \= Z, compress([Z|Xs], Y).


% 1.09 (**) Pack consecutive duplicates of list elements into sublists.

% Example:
% ?- pack([a, a, a, a, b, c, c, a, a, d, e, e, e, e], X).
% X = [[a, a, a, a], [b], [c, c], [a, a], [d], [e, e, e, e]]

pack([], []).
pack([X], [[X]]).
pack([X|Xs], [[X,X|P]|T]) :- pack(Xs, [[X|P]|T]).
pack([X|Xs], [[X],[Y|P]|T]) :- pack(Xs, [[Y|P]|T]), X \= Y.


% 1.10 (*) Run-length encoding of a list.

% Example:
% ?- encode([a, a, a, a, b, c, c, a, a, d, e, e, e, e], X).
% X = [[4, a], [1, b], [2, c], [2, a], [1, d], [4, e]]

encode([], []).
encode([X], [[1, X]]).
encode([X|Xs], [[N_, X]|E]) :- encode(Xs, [[N, X]|E]), N_ is N + 1.
encode([X|Xs], [[1, X], [N, Y]|E]) :- encode(Xs, [[N, Y]|E]), X \= Y.
