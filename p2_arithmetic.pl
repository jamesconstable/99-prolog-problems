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
