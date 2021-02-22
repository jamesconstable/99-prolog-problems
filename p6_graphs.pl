:- use_module(library(ordsets)).

% 6.01 (***) Conversions

% Write predicates to convert between the different graph representations. With
% these predicates, all representations are equivalent; i.e. for the following
% problems you can always freely pick the most convenient form. The reason this
% problem is rated (***) is not because it's particularly difficult, but because
% it's a lot of work to deal with all the special cases.

% Here is a brief summary of the abbreviations I'm using for this task, and
% three examples of each representation (first is undirected, second is
% directed, third is labelled).

% Figure for undirected graph examples:
% https://sites.google.com/site/prologsite/_/rsrc/1264948248705/prolog-problems/6/graph1.gif

% Figure for directed graph examples:
% https://sites.google.com/site/prologsite/_/rsrc/1264948667063/prolog-problems/6/graph2.gif

% Figure for labelled graph examples:
% https://sites.google.com/site/prologsite/prolog-problems/6/graph3.gif

% ec: edge-clause form, e.g.
%     [edge(h, g), edge(k, f), edge(f, b), edge(b, c), edge(c, f)]
%     [arc(s, u), arc(u, r), arc(s, r), arc(u, s), arc(v, u)]
%     [arc(m, q, 7), arc(p, q, 9), arc(p, m, 5)]

% gt: graph-term form, e.g.
%     graph([b, c, d, f, g, h, k],
%           [e(b, c), e(b, f), e(c, f), e(f, k), e(g, h)])
%     digraph([r, s, t, u, v], [a(s, r), a(s, u), a(u, r), a(u, s), a(v, u)])
%     digraph([k, m, p, q], [a(m, q, 7), a(p, m, 5), a(p, q, 9)])

% al: adjacency-list form, e.g.
%     [n(b, [c, f]), n(c, [b, f]), n(d, []), n(f, [b, c, k]), n(g, [h]),
%      n(h, [g]), n(k, [f])]
%     [n(r, []), n(s, [r, u]), n(t, []), n(u, [r]), n(v, [u])]
%     [n(k, []), n(m, [q/7]), n(p, [m/5, q/9]), n(q, [])]

% hf: human-friendly form, e.g.
%     [b-c, f-c, g-h, d, f-b, k-f, h-g]
%     [s > r, t, u > r, s > u, u > s, v > u]
%     [p>q/9, m>q/7, k, p>m/5]

% Convert between edge-clause and graph-term forms in either flow order, for
% both directed and undirected, labelled and unlabelled.
ec_gt(ECs, G) :- nonvar(ECs), ECs = [E|_], is_arc(E), !, ec_to_gt_d(ECs, G).
ec_gt(ECs, G) :- nonvar(ECs), !, ec_to_gt_u(ECs, G).
ec_gt(ECs, G) :- G = digraph(_, _), !, gt_to_ec_d(G, ECs).
ec_gt(ECs, G) :- gt_to_ec_u(G, ECs).

is_arc(arc(_, _)).
is_arc(arc(_, _, _)).

% Convert undirected edge-clause to graph-term.
ec_to_gt_u([], graph([], [])).
ec_to_gt_u([Edge|Edges], graph(N2, E2)) :-
  ec_to_gt_u(Edges, graph(N1, E1)),
  e_edge_nodes(E, Edge, Ns),
  ord_union(N1, Ns, N2),
  ord_union(E1, [E], E2).

% Convert undirected graph-term to edge-clause.
gt_to_ec_u(graph(_, Es), Edges) :- maplist(e_edge_nodes, Es, Edges, _).

% Convert directed edge-clause to graph-term.
ec_to_gt_d([], digraph([], [])).
ec_to_gt_d([Arc|Arcs], digraph(N2, A2)) :-
  ec_to_gt_d(Arcs, digraph(N1, A1)),
  a_arc_nodes(A, Arc, Ns),
  ord_union(N1, Ns, N2),
  ord_union(A1, [A], A2).

% Convert directed graph-term to edge-clause.
gt_to_ec_d(digraph(_, As), Arcs) :- maplist(a_arc_nodes, As, Arcs, _).

% Convert between e terms, edge terms and sorted node pair lists.
e_edge_nodes(e(A, B), edge(A_, B_), [A, B]) :- msort([A_, B_], [A, B]).
e_edge_nodes(e(A, B, C), edge(A_, B_, C), [A, B]) :- msort([A_, B_], [A, B]).

% Convert between a terms, arc terms and sorted node pair lists.
a_arc_nodes(a(A_, B_), arc(A_, B_), [A, B]) :- msort([A_, B_], [A, B]).
a_arc_nodes(a(A_, B_, C), arc(A_, B_, C), [A, B]) :- msort([A_, B_], [A, B]).


