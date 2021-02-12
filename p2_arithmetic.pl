% 2.01 (**) Determine whether a given number is prime.

% Example:
% ?- is_prime(7).
% true

is_prime(N) :- integer(N), N > 1, \+ has_factor(N, 2).

has_factor(N, K) :- K * K =< N, N mod K =:= 0.
has_factor(N, K) :- K * K =< N, K_ is K + 1, has_factor(N, K_).


% 2.02 (**) Determine the prime factors of a given positive integer.
% Construct a flat list containing the prime factors in ascending order.

% Example:
% ?- prime_factors(315, L).
% L = [3, 3, 5, 7]

prime_factors(N, Fs) :-
  N > 1, range(2, N, R), prime_sieve(R, Ps), prime_factors(N, Ps, Fs).

prime_factors(_, [], []).
prime_factors(1, _, []) :- !.
prime_factors(N, [P|Ps], Fs) :- N mod P =\= 0, !, prime_factors(N, Ps, Fs).
prime_factors(N, [P|Ps], [P|Fs]) :- N2 is N / P, prime_factors(N2, [P|Ps], Fs).

prime_sieve([], []).
prime_sieve([N|Ns], [N|Ps]) :- filter_multiples(N, Ns, R), prime_sieve(R, Ps).

filter_multiples(_, [], []).
filter_multiples(N, [X|Xs], T) :- X mod N =:= 0, !, filter_multiples(N, Xs, T).
filter_multiples(N, [X|Xs], [X|T]) :- filter_multiples(N, Xs, T).

range(N, N, [N]).
range(A, B, [A|R]) :- A \= B, A_ is A + sign(B-A), range(A_, B, R).


% 2.03 (**) Determine the prime factors of a given positive integer (2).
% Construct a list containing the prime factors and their multiplicity.

% Example:
% ?- prime_factors_mult(315, L).
% L = [[3, 2], [5, 1], [7, 1]]

prime_factors_mult(N, FMs) :- prime_factors(N, Fs), group_count(Fs, FMs).

group_count([], []).
group_count([X], [[X, 1]]).
group_count([X|Xs], [[X, N_]|E]) :- group_count(Xs, [[X, N]|E]), N_ is N + 1.
group_count([X|Xs], [[X, 1], [Y, N]|E]) :- group_count(Xs, [[Y, N]|E]), X \= Y.


% 2.04 (*) A list of prime numbers.
% Given a range of integer by its lower and upper limit, construct a list of all
% prime numbers in that range.

primes_in_range(L, U, RPs) :- range(2, U, R), prime_sieve(R, Ps),
  drop_while_less_than(L, Ps, RPs).

drop_while_less_than(N, [X|Xs], T) :- X < N, !, drop_while_less_than(N, Xs, T).
drop_while_less_than(_, Xs, Xs).


% 2.05 (**) Goldbach's conjecture.
% Goldbach's conjecture says that every positive even number greater than 2 is
% the sum of two prime numbers. It is one of the most famous facts in number
% theory that has not been proven to be correct in the general case, but has
% been numerically confirmed up to very large numbers (much larger than is
% possible in Prolog). Write a predicate to find the two prime numbers that sum
% to a given even number.

% Example:
% ?- goldbach(28, L).
% L = [5, 23]

goldbach(N, [A, B]) :-
  integer(N), N > 2, N mod 2 =:= 0,
  HalfN is N / 2,
  primes_in_range(2, HalfN, Ps),
  member(A, Ps),
  B is N - A,
  is_prime(B).

member(X, [X|_]).
member(X, [_|Xs]) :- member(X, Xs).


% 2.06 (**) A list of Goldbach compositions.
% a) Given a range of integers by its lower and upper limit, print a list of all
%    even numbers and their Goldbach composition.
%
%    Example:
%    ?- goldbach_list(9,20).
%    10 = 3 + 7
%    12 = 5 + 7
%    14 = 3 + 11
%    16 = 3 + 13
%    18 = 5 + 13
%    20 = 3 + 17

goldbach_list(L, U) :- goldbach_list(L, U, 0).


% b) In most cases, if an even number is written as the sum of two prime
%    numbers, one of them is very small. Very rarely, the primes are both
%    bigger than say 50. Try to find out how many such cases there are in the
%    range 2..3000.
%
%    Example (for a print limit of 50):
%    ?- goldbach_list(1,2000,50).
%    992 = 73 + 919
%    1382 = 61 + 1321
%    1856 = 67 + 1789
%    1928 = 61 + 1867

goldbach_list(L, U, PL) :- L =< 2, goldbach_list(4, U, PL), !.
goldbach_list(L, U, _) :- L > U, !.
goldbach_list(L, U, PL) :-
  L1 is (L + 1) // 2 * 2,    % Round up to the next even number
  goldbach(L1, [A, B]),
  goldbach_write(L1, A, B, PL),
  L2 is L1 + 2,
  goldbach_list(L2, U, PL).

goldbach_write(X, P1, P2, L) :- P1 > L, writef('%t = %t + %t', [X, P1, P2]), nl.
goldbach_write(_, _, _, _).


% 2.07 (**) Determine the greatest common divisor of two positive integer
% numbers. Use Euclid's algorithm.

% Example:
% ?- gcd(36, 63, G).
% G = 9

% Define gcd as an arithmetic function, so you can use it like this:
% ?- G is gcd(36, 63).
% G = 9

gcd(N1, 0, N1).
gcd(N1, N2, D) :- N2 > 0, R is N1 mod N2, gcd(N2, R, D).

:- arithmetic_function(gcd/2).


% 2.08 (*) Determine whether two positive integer numbers are coprime.
% Two numbers are coprime if their greatest common divisor equals 1.

% Example:
% ?- coprime(35, 64).
% true

coprime(A, B) :- gcd(A, B, 1).
