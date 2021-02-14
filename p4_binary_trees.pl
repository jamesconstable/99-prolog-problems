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
