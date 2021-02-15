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
hbal_tree(H, t(x, L, R)) :-
  H > 0, H1 is H-1, hbal_tree(H1, L), hbal_tree(H1, R).
hbal_tree(H, t(x, L, R)) :-
  H > 1, H1 is H-1, H2 is H-2, hbal_tree(H1, L), hbal_tree(H2, R).
hbal_tree(H, t(x, L, R)) :-
  H > 1, H1 is H-1, H2 is H-2, hbal_tree(H2, L), hbal_tree(H1, R).


% 4.07 (**) Construct height-balanced binary trees with a given number of nodes.
% a) Consider a height-balanced binary tree of height H. What is the maximum
%    number of nodes it can contain? Clearly, MaxN = 2**H - 1. However, what is
%    the minimum number MinN? This question is more difficult. Try to find a
%    recursive statement and turn it into a predicate minNodes/2 defined as
%    follows:

%    minNodes(H, N) :- N is the minimum number of nodes in a height-balanced
%    binary tree of height H. (integer, integer), (+, ?)

:- table(minNodes/2).
minNodes(0, 0).
minNodes(1, 1).
minNodes(H, N) :-
  H > 1, H1 is H-1, H2 is H-2, minNodes(H1, N1), minNodes(H2, N2), N is N1+N2+1.

% b) On the other hand, we might ask: what is the maximum height H a
%    height-balanced binary tree with N nodes can have?

%    maxHeight(N, H) :- H is the maximum height of a height-balanced binary tree
%    with N nodes. (integer, integer), (+, ?)

maxHeight(N, H) :- maxHeight(N, H, 0).

maxHeight(N, H, HCurr) :-
  minNodes(HCurr, M), M =< N, HCurr_ is HCurr+1, maxHeight(N, H, HCurr_).
maxHeight(N, H, HCurr) :-
  minNodes(HCurr, M), M > N, H is HCurr-1.

% c) Now, we can attack the main problem: construct all the height-balanced
%    binary trees with a given nuber of nodes. Find out how many height-balanced
%    trees exist for N = 15.

%    hbal_tree_nodes(N, T) :- T is a height-balanced binary tree with N nodes.

hbal_tree_nodes(N, T) :-
  minHeight(N, HMin),
  maxHeight(N, HMax),
  between(HMin, HMax, H),
  hbal_tree(H, T),
  count_nodes(T, N).

count_nodes(nil, 0).
count_nodes(t(_, L, R), N) :-
  count_nodes(L, Nl), count_nodes(R, Nr), N is 1+Nl+Nr.

log2(X, R) :- R is log(X) / log(2).
:- arithmetic_function(log2/1).

minHeight(N, H) :- H is 1 + floor(log2(N)).

count_hbal_trees(N, R) :- setof(T, hbal_tree_nodes(N, T), Ts), length(Ts, R).


% 4.08 (*) Count the leaves of a binary tree.
% A leaf is a node with no successors. Write a predicate count_leaves/2 to count
% them.

% count_leaves(T, N) :- the binary tree T has N leaves

count_leaves(nil, 0).
count_leaves(t(_, nil, nil), 1) :- !.
count_leaves(t(_, L, R), N) :-
  count_leaves(L, Nl), count_leaves(R, Nr), N is Nl+Nr.


% 4.09 (*) Collect the leaves of a binary tree in a list.
% A leaf is a node with no successors. Write a predicate leaves/2 to collect
% them in a list.

% leaves(T, S) :- S is the list of all leaves of the binary tree T

leaves(nil, []).
leaves(t(X, nil, nil), [X]) :- !.
leaves(t(_, L, R), Ls) :- leaves(L, Ls1), leaves(R, Ls2), append(Ls1, Ls2, Ls).


% 4.10 (*) Collect the internal nodes of a binary tree in a list.
% An internal node of a binary tree has either one or two non-empty successors.
% Write a predicate internals/2 to collect them in a list.

% internals(T,S) :- S is the list of internal nodes of the binary tree T.

internals(nil, []).
internals(t(_, nil, nil), []) :- !.
internals(t(X, L, R), Is) :-
  internals(L, Is1), internals(R, Is2), append(Is1, [X|Is2], Is).


% 4.11 (*) Collect the nodes at a given level in a list.
% A node of a binary tree is at level N if the path from the root to the node
% has length N-1. The root node is at level 1. Write a predicate atlevel/3 to
% collect all nodes at a given level in a list.

% atlevel(T,L,S) :- S is the list of nodes of the binary tree T at level L

atlevel(_, 0, []).
atlevel(nil, _, []).
atlevel(t(X, _, _), 1, [X]) :- !.
atlevel(t(_, L, R), N, S) :-
  N_ is N-1, atlevel(L, N_, Sl), atlevel(R, N_, Sr), append(Sl, Sr, S).
