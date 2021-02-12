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

table(A, B, Pred) :- bind(A), bind(B), write_row(A, B, Pred), fail.
table(_, _, _).

bind(true).
bind(false).

write_row(A, B, P) :- P, !, writef("%t\t%t\t%t", [A, B, true]), nl.
write_row(A, B, P) :- \+ P, writef("%t\t%t\t%t", [A, B, false]), nl.


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
