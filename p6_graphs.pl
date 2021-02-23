:- use_module(library(apply)).
:- use_module(library(apply_macros)).
:- use_module(library(assoc)).
:- use_module(library(ordsets)).
:- use_module(library(yall)).

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
ec_gt(ECs, G) :-
  nonvar(ECs), ECs = [E|_], functor(E, arc, _), !, ec_to_gt(ECs, G, d).
ec_gt(ECs, G) :- nonvar(ECs), !, ec_to_gt(ECs, G, u).
ec_gt(ECs, G) :- G = digraph(_, _), !, gt_to_ec(G, ECs, d).
ec_gt(ECs, G) :- gt_to_ec(G, ECs, u).

% Convert edge-clause to graph-term.
ec_to_gt([], G, D) :- graph_term(G, [], [], D).
ec_to_gt([Edge|Edges], G, D) :-
  ec_to_gt(Edges, G1, D),
  graph_term(G1, N1, E1, _),
  edge_terms(E, Edge, _, UNs),
  msort(UNs, Ns),
  ord_union(N1, Ns, N2),
  ord_union(E1, [E], E2),
  graph_term(G, N2, E2, D).

% Convert graph-term to edge-clause.
gt_to_ec(G, Edges, _) :-
  graph_term(G, _, Es, _), maplist(edge_terms, Es, Edges, _, _).

% Convert between (di)graphs and their component terms. The final u/d term flags
% directedness (necessary when using in the construction flow order).
graph_term(graph(Ns, Es),   Ns, Es, u).
graph_term(digraph(Ns, As), Ns, As, d).

% Convert between e/a terms, edge/arc terms, "human-friendly" terms and node
% lists.
edge_terms(e(A, B),    edge(A, B),    A-B,   [A, B]).
edge_terms(e(A, B, C), edge(A, B, C), A-B/C, [A, B]).
edge_terms(a(A, B),    arc(A, B),     A>B,   [A, B]).
edge_terms(a(A, B, C), arc(A, B, C),  A>B/C, [A, B]).

% Convert between graph-term and adjacency-list forms in either flow order, for
% both directed and undirected, labelled and unlabelled. The adjacency-list form
% doesn't distinguish between graphs and digraphs, the (?, +) flow order always
% returns a digraph.
gt_al(G, AL) :- nonvar(G), !, gt_to_al(G, AL).
gt_al(G, AL) :- al_to_gt(AL, G).

% Convert graph-term to adjacency-list.
gt_to_al(G, AL) :-
  graph_term(G, Ns, Es, _),
  maplist([X, Y]>>(Y = X-[]), Ns, Empties),
  list_to_assoc(Empties, Assoc),
  expand_edges(Es, As),
  add_arcs_to_assoc(As, Assoc, Assoc1),
  assoc_to_list(Assoc1, Pairs),
  maplist([P, N]>>(P = K-V, msort(V, V1), N = n(K, V1)), Pairs, AL).

% Convert e/a terms to human-friendly arc terms, with edges expanded to two
% arc terms rather than the corresponding -/2 functor.
expand_edges([], []).
expand_edges([E|Es], A) :- edge_arcs(E, A, A1), expand_edges(Es, A1).

% Convert an e/a term to a difference list of human-friendly arc terms, with
% edges being expanded to two arc terms.
edge_arcs(e(A, B),    [A>B, B>A|X],     X).
edge_arcs(e(A, B, C), [A>B/C, B>A/C|X], X).
edge_arcs(a(A, B),    [A>B|X],          X).
edge_arcs(a(A, B, C), [A>B/C|X],        X).

add_arcs_to_assoc([], A, A).
add_arcs_to_assoc([X|Xs], A, A_) :-
  arc_split(X, P, Q),
  get_assoc(P, A, V),
  put_assoc(P, A, [Q|V], A1),
  add_arcs_to_assoc(Xs, A1, A_).

% Relates a human-friendly arc term to its start and end/label (as used in the
% adjacency list representation).
arc_split(A>B, A, B).
arc_split(A>B/C, A, B/C).

% Convert adjacency-list to graph-term. The adjacency-list form doesn't
% distinguish between graphs and digraphs, so we always return a digraph.
al_to_gt([], digraph([], [])).
al_to_gt([n(N, Neighbours)|Ns], digraph(GNs_, GEs_)) :-
  al_to_gt(Ns, digraph(GNs, GEs)),
  ord_union(GNs, [N], GNs_),
  maplist(
    {N}/[Neighbour, A]>>(arc_split(HF, N, Neighbour), edge_terms(A, _, HF, _)),
    Neighbours, As),
  ord_union(GEs, As, GEs_).
