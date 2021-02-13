% 3.01 (**) Truth tables for logical expressions.
% Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2 (for
% logical equivalence) which succeed or fail according to the result of their
% respective operations; e.g. and(A, B) will succeed, if and only if both A and
% B succeed. Note that A and B can be Prolog goals (not only the constants true
% and fail).

% A logical expression in two variables can then be written in prefix notation,
% as in the following example: and(or(A, B), nand(A, B)).

% Now, write a predicate table/3 which prints the truth table of a given logical 
% expression in two variables.

% Example:
% ?- table(A, B, and(A, or(A, B))).
% true  true  true
% true  false true
% false true  false
% false false false

and(A, B) :- A, B.

or(A, _) :- A.
or(_, B) :- B.

nand(A, B) :- not(and(A, B)).

nor(A, B) :- not(or(A, B)).

xor(A, B) :- and(or(A, B), not(and(A, B))) .

impl(A, B) :- or(not(A), B).

equ(A, B) :- not(xor(A, B)).

table(A, B, Expr) :- bind(A), bind(B), write_row(A, B, Expr), fail.
table(_, _, _).

bind(true).
bind(false).

write_row(A, B, E) :- E, !, writef("%t\t%t\t%t", [A, B, true]), nl.
write_row(A, B, _) :- writef("%t\t%t\t%t", [A, B, false]), nl.


% 3.02 (*) Truth tables for logical expressions (2).
% Continue problem 3.01 by defining and/2, or/2, etc as being operators. This
% allows to write the logical expression in the more natural way, as in the
% example: A and (A or not B). Define operator precedence as usual.

% Example:
% ?- table(A, B, A and (A or not B)).
% true  true  true
% true  false true
% false true  false
% false false false

:- op(900,  fy, not).
:- op(920, yfx, and).
:- op(940, yfx, or).
:- op(920, yfx, nand).
:- op(940, yfx, nor).
:- op(960, yfx, xor).
:- op(960, yfx, impl).
:- op(960, yfx, equ).


% 3.03 (**) Truth tables for logical expressions (3).
% Generalize problem 3.02 in such a way that the logical expression may contain
% any number of logical variables. Define table/2 in a way that
% table(List, Expr) prints the truth table for the expression Expr, which
% contains the logical variables enumerated in List.

% Example:
% ?- table2([A,B,C], A and (B or C) equ A and B or A and C).
% true  true  true  true
% true  true  false true
% true  false true  true
% true  false false true
% false true  true  true
% false true  false true
% false false true  true
% false false false true

table2(Vs, Expr) :- bind_vars(Vs), write_row2(Vs, Expr), fail.
table2(_, _).

bind_vars([]).
bind_vars([V|Vs]) :- bind(V), bind_vars(Vs).

write_row2([], E) :- E, !, write(' true'), nl.
write_row2([], _) :- write(' false'), nl.
write_row2([V|Vs], E) :- write(V), write('\t'), write_row2(Vs, E).


% 3.04 (**) Gray code.
% An n-bit Gray code is a sequence of n-bit strings constructed according to
% certain rules. For example,
% n = 1: C(1) = ['0','1'].
% n = 2: C(2) = ['00','01','11','10'].
% n = 3: C(3) = ['000','001','011','010','110','111','101','100'].

% Find out the construction rules and write a predicate with the following
% specification:
% gray(N,C) :- C is the N-bit Gray code

% Can you apply the method of "result caching" in order to make the predicate
% more efficient, when it is to be used repeatedly?

:- table gray/2.

gray(1, ['0', '1']).
gray(N, C) :-
  N > 1,
  N_ is N - 1,
  gray(N_, C_),
  reverse(C_, Cr),
  prepend_all('0', C_, P1),
  prepend_all('1', Cr, P2),
  append(P1, P2, C).

prepend_all(_, [], []).
prepend_all(S, [X|Xs], [Y|Ys]) :- atom_concat(S, X, Y), prepend_all(S, Xs, Ys).


% 3.05 (***) Huffman code.
% First of all, study a good book on discrete mathematics or algorithms for a
% detailed description of Huffman codes, or consult Wikipedia.

% We suppose a set of symbols with their frequencies, given as a list of fr(S,F)
% terms. Example: [fr(a, 45), fr(b, 13), fr(c, 12), fr(d, 16), fr(e, 9),
% fr(f, 5)]. Our objective is to construct a list hc(S, C) terms, where C is the
% Huffman code word for the symbol S. In our example, the result could be
% Hs = [hc(a, '0'), hc(b, '101'), hc(c, '100'), hc(d, '111'), hc(e, '1101'),
% hc(f, '1100')]. The task shall be performed by the predicate huffman/2 defined
% as follows:

% huffman(Fs, Hs) :- Hs is the Huffman code table for the frequency table Fs

huffman([], []).
huffman([fr(H, _)], H) :- is_list(H), !.
huffman([fr(S, _)], [hc(S, '0')]).
huffman([fr(S, _)], [hc(S, '1')]).
huffman(Fs, Hs) :-
  select_min(Fs, A, Fs1),
  select_min(Fs1, B, Fs2),
  combine(A, B, C),
  huffman([C|Fs2], Hs).

min_weight([fr(_, F)], F).
min_weight([fr(_, F)|SFs], F) :- min_weight(SFs, M), F < M, !.
min_weight([_|SFs], M) :- min_weight(SFs, M).

select_by_weight(W, [fr(S, W)|R], fr(S, W), R).
select_by_weight(W, [fr(S, X)|SFs], I, [fr(S, X)|R]) :-
  select_by_weight(W, SFs, I, R).

select_min(L, M, R) :- min_weight(L, W), select_by_weight(W, L, M, R).

combine(A, B, C) :- combine_(A, B, C).
combine(A, B, C) :- combine_(B, A, C).

combine_(fr(Sa, Fa), fr(Sb, Fb), fr(HCs, Fc)) :-
  prepend_all_codes('0', Sa, HCs1),
  prepend_all_codes('1', Sb, HCs2),
  append(HCs1, HCs2, HCs),
  Fc is Fa + Fb.

prepend_all_codes(P, S, [hc(S, P)]) :- \+ is_list(S).
prepend_all_codes(_, [], []).
prepend_all_codes(P, [hc(S, C)|Xs], [hc(S, C_)|Ys]) :-
  atom_concat(P, C, C_), prepend_all_codes(P, Xs, Ys).
