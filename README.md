# 99 Prolog Problems
A set of solutions to the
[P-99: Ninety-Nine Prolog Problems](https://sites.google.com/site/prologsite/prolog-problems)
collection. I'm using these problems as a way to learn the basics and mindset of
general-purpose programming in Prolog.

Problems are grouped by topic and use the relation names given in the examples,
where applicable. To play with a solution set, open the SWI Prolog interpreter
and enter the name of the the source file minus the extension between square
brackets, followed by a period. For example, to try out the solution to the
first problem:

```
$ swipl
Welcome to SWI-Prolog ...
...

?- [p1_lists].
true.

?- my_last(X, [1, 2, 3]).
X = 3 .
```
