% 4.01 (*) Check whether a given term represents a binary tree.
% Write a predicate istree/1 which succeeds if and only if its argument is a
% Prolog term representing a binary tree.

% Example:
% ?- istree(t(a,t(b,nil,nil),nil)).
% Yes
% ?- istree(t(a,t(b,nil,nil))).
% No

istree(nil).
istree(t(_, T1, T2)) :- istree(T1), istree(T2).


% 4.02 (**) Construct completely balanced binary trees.
% In a completely balanced binary tree, the following property holds for every
% node: the number of nodes in its left subtree and the number of nodes in its
% right subtree are almost equal, which means their difference is not greater
% than one.

% Write a predicate cbal_tree/2 to construct completely balanced binary trees
% for a given number of nodes. The predicate should generate all solutions vi
% backtracking. Put the letter 'x' as information into all nodes of the tree.

% Example:
% ?- cbal_tree(4,T).
% T = t(x, t(x, nil, nil), t(x, nil, t(x, nil, nil))) ;
% T = t(x, t(x, nil, nil), t(x, t(x, nil, nil), nil)) ;
% ...
% No

cbal_tree(0, nil) :- !.
cbal_tree(N, t(x, T1, T2)) :-
  N mod 2 =:= 1, !,
  N_ is N // 2, cbal_tree(N_, T1), cbal_tree(N_, T2).
cbal_tree(N, t(x, T1, T2)) :-
  N1 is N // 2, N2 is N1 - 1, cbal_tree(N1, T1), cbal_tree(N2, T2).
cbal_tree(N, t(x, T1, T2)) :-
  N2 is N // 2, N1 is N2 - 1, cbal_tree(N1, T1), cbal_tree(N2, T2).


% 4.03 (**) Symmetric binary trees.
% Let us call a binary tree symmetric if you can draw a vertical line through
% the root node and then the right subtree is the mirror image of the left
% subtree. Write a predicate symmetric/1 to check whether a given binary tree is
% symmetric. Hint: Write a predicate mirror/2 first to check whether one tree is
% the mirror image of another. We are only interested in the structure, not in
% the contents of the nodes.

symmetric(nil).
symmetric(t(_, L, R)) :- mirror(L, R).

mirror(nil, nil).
mirror(t(_, A, B), t(_, B_, A_)) :- mirror(A, A_), mirror(B, B_).


% 4.04 (**) Binary search trees (dictionaries).
% Use the predicate add/3, developed in chapter 4 of the course, to write a
% predicate to construct a binary search tree from a list of integer numbers.

% Example:
% ?- construct([3,2,5,7,1],T).
% T = t(3, t(2, t(1, nil, nil), nil), t(5, nil, t(7, nil, nil)))

% Then use this predicate to test the solution of the problem P56.
% Example:
% ?- test_symmetric([5,3,18,1,4,12,21]).
% Yes
% ?- test_symmetric([3,2,5,7,4]).
% No

construct(X, T) :- construct_(X, T, nil).

construct_([], T, T).
construct_([X|Xs], R, Acc) :- add(X, Acc, Acc2), construct_(Xs, R, Acc2).

add(X, nil, t(X, nil, nil)).
add(X, t(Y, L, R), t(Y, L_, R)) :- X @=< Y, add(X, L, L_).
add(X, t(Y, L, R), t(Y, L, R_)) :- X @> Y, add(X, R, R_).

test_symmetric(L) :- construct(L, T), symmetric(T).


% 4.05 (**) Generate-and-test paradigm.
% Apply the generate-and-test paradigm to construct all symmetric, completely
% balanced binary trees with a given number of nodes.

% Example:
% ?- sym_cbal_trees(5,Ts).
% Ts = [t(x, t(x, nil, t(x, nil, nil)), t(x, t(x, nil, nil), nil)), t(x, t(x,
%   t(x, nil, nil), nil), t(x, nil, t(x, nil, nil)))]

% How many such trees are there with 57 nodes? Investigate about how many
% solutions there are for a given number of nodes? What if the number is even?
% Write an appropriate predicate.

sym_cbal_trees(N, Ts) :- setof(T, sym_cbal_tree(N, T), Ts), !.
sym_cbal_trees(_, []).

sym_cbal_tree(N, T) :- cbal_tree(N, T), symmetric(T).

sym_cbal_trees_count(Nodes, N) :- sym_cbal_trees(Nodes, Ts), length(Ts, N).


% 4.06 (**) Construct height-balanced binary trees.
% In a height-balanced binary tree, the following property holds for every node:
% The height of its left subtree and the height of its right subtree are almost
% equal, which means their difference is not greater than one.

% Write a predicate hbal_tree/2 to construct height-balanced binary trees for a
% given height. The predicate should generate all solutions via backtracking.
% Put the letter 'x' as information into all nodes of the tree.

% Example:
% ?- hbal_tree(3,T).
% T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil),
%   t(x, nil, nil))) ;
% T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), nil)) ;
% ...
% No

hbal_tree(0, nil).
hbal_tree(N, t(x, L, R)) :-
  N > 0, N1 is N-1, hbal_tree(N1, L), hbal_tree(N1, R).
hbal_tree(N, t(x, L, R)) :-
  N > 1, N1 is N-1, N2 is N-2, hbal_tree(N1, L), hbal_tree(N2, R).
hbal_tree(N, t(x, L, R)) :-
  N > 1, N1 is N-1, N2 is N-2, hbal_tree(N2, L), hbal_tree(N1, R).
