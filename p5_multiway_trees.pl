% 5.01 (*) Check whether a given term represents a multiway tree.
% Write a predicate istree/1 which succeeds if and only if its argument is a
% Prolog term representing a multiway tree.

% Example:
% ?- istree(t(a,[t(f,[t(g,[])]),t(c,[]),t(b,[t(d,[]),t(e,[])])])).
% Yes

istree(t(_, C)) :- maplist(istree, C).


% 5.02 (*) Count the nodes of a multiway tree.
% Write a predicate nnodes/1 which counts the nodes of a given multiway tree.

% Example:
% ?- nnodes(t(a,[t(f,[])]),N).
% N = 2

% Write another version of the predicate that allows for a flow pattern (o,i).

nnodes(t(_, C), N) :- maplist(nnodes, C, Ns), foldl(plus, Ns, 1, N).


% 5.03 (**) Tree construction from a node string.
% We suppose that the nodes of a multiway tree contain single characters. In the
% depth-first order sequence of its nodes, a special character ^ has been
% inserted whenever, during the tree traversal, the move is a backtrack to the
% previous level.

% By this rule, the tree in the figure at:
% https://sites.google.com/site/prologsite/prolog-problems/5/p70.gif
% is represented as: afg^^c^bd^e^^^

% Define the syntax of the string and write a predicate tree(String,Tree) to
% construct the Tree when the String is given. Work with atoms (instead of
% strings). Make your predicate work in both directions.

tree(S, T) :- nonvar(S), atom_chars(S, As), tree(As, [], T).
tree(S, T) :- nonvar(T), tree(As, [], T), atom_chars(S, As).

tree([X|S], S_, t(X, C)) :- char_type(X, alpha), trees(S, ['^'|S_], C).

trees(['^'|S], ['^'|S], []).
trees(S, S2, [T|Ts]) :- tree(S, S1, T), trees(S1, S2, Ts).
