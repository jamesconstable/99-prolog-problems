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

% Convert between edge-clause and graph-term forms in either flow order.
% Edge-clause form cannot encode isolated nodes, so these are lost in the (?, +)
% flow order.
ec_gt(ECs, G) :- nonvar(ECs), !, ec_to_gt(ECs, G, _).
ec_gt(ECs, G) :- graph_term(G, _, Es, _), maplist(edge_terms, Es, ECs, _, _).

% Convert edge-clause to graph-term.
ec_to_gt([], G, D) :- (var(D) -> D=u; true), graph_term(G, [], [], D).
ec_to_gt([Edge|Edges], G, D) :-
  (functor(Edge, arc, _) -> D=d; D=u),
  ec_to_gt(Edges, G1, D),
  graph_term(G1, N1, E1, _),
  edge_terms(E, Edge, _, Ns),
  ord_add_all(N1, Ns, N2),
  ord_union(E1, [E], E2),
  graph_term(G, N2, E2, D).

ord_add_all(S1, Es, S2) :- sort(Es, SEs), ord_union(S1, SEs, S2).

% Convert between (di)graphs and their component terms. The final u/d term flags
% directedness (necessary when using in the construction flow order).
graph_term(graph(Ns, Es),   Ns, Es, u).
graph_term(digraph(Ns, As), Ns, As, d).

% Convert between e/a terms, edge/arc terms, "human-friendly" terms and node
% lists.
edge_terms(e(A, B, C), edge(A, B, C), A-B/C, [A, B]).
edge_terms(e(A, B),    edge(A, B),    A-B,   [A, B]).
edge_terms(a(A, B, C), arc(A, B, C),  A>B/C, [A, B]).
edge_terms(a(A, B),    arc(A, B),     A>B,   [A, B]).

% Convert between graph-term and adjacency-list forms in either flow order.
% Note that adjacency-list form doesn't distinguish between graphs and digraphs,
% so the (?, +) flow order always returns a digraph.
gt_al(G, AL) :- nonvar(G), !, gt_to_al(G, AL).
gt_al(G, AL) :- al_to_gt(AL, G).

% Convert graph-term to adjacency-list.
gt_to_al(G, AL) :-
  graph_term(G, Ns, Es, _),
  maplist([X, Y]>>(Y = X-[]), Ns, Empties),
  list_to_assoc(Empties, Assoc),
  expand_edges(Es, As),
  foldl([P>Q, A, A1]>>(get_assoc(P, A, V), put_assoc(P, A, [Q|V], A1)),
    As, Assoc, Assoc1),
  assoc_to_list(Assoc1, Pairs),
  maplist([K-V, N]>>(msort(V, V1), N = n(K, V1)), Pairs, AL).

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

% Convert adjacency-list to graph-term. The adjacency-list form doesn't
% distinguish between graphs and digraphs, so we always return a digraph.
al_to_gt([], digraph([], [])).
al_to_gt([n(N, Neighbours)|Ns], digraph(GNs_, GEs_)) :-
  al_to_gt(Ns, digraph(GNs, GEs)),
  ord_union(GNs, [N], GNs_),
  maplist({N}/[Neighbour, A]>>edge_terms(A, _, N>Neighbour, _), Neighbours, As),
  ord_union(GEs, As, GEs_).

% Convert between edge-clause and adjacency-list forms in either flow order.
% Note that isolated nodes are lost in the (?, +) flow order as edge-clause form
% has no way to represent them, and arcs are always used, as adjacency-list form
% doesn't distinguish between graphs and digraphs.
ec_al(EC, AL) :- nonvar(EC), !, ec_gt(EC, GT), gt_al(GT, AL).
ec_al(EC, AL) :- gt_al(GT, AL), ec_gt(EC, GT).

% Convert between graph-term and human-friendly forms in either flow order.
% Fails if arc and edges are intermixed in the human-friendly form.
gt_hf(GT, HF) :- nonvar(GT), !, gt_to_hf(GT, HF).
gt_hf(GT, HF) :- hf_to_gt(HF, GT, _).

% Convert graph-term to human-friendly.
gt_to_hf(GT, HF) :- graph_term(GT, Ns, Es, _), gt_to_hf(Ns, Es, HF).

gt_to_hf(Ns, [], Ns).
gt_to_hf(Ns, [E|Es], [HF|HFs]) :-
  edge_terms(E, _, HF, ENs), subtract(Ns, ENs, Ns1), gt_to_hf(Ns1, Es, HFs).

% Convert human-friendly to graph-term.
hf_to_gt([], G, D) :- (var(D) -> D=u; true), graph_term(G, [], [], D).
hf_to_gt([E|HFs], G_, D) :-
  edge_terms(A, _, E, S), !,    % Doubles as non-isolated node check
  (E = (_>_) -> D=d; D=u),
  hf_to_gt(HFs, G, D),
  graph_term(G, Ns, Es, _),
  ord_add_all(Ns, S, Ns_),
  graph_term(G_, Ns_, [A|Es], D).
hf_to_gt([N|HFs], G_, D) :-
  hf_to_gt(HFs, G, D),
  graph_term(G, Ns, Es, _),
  ord_union(Ns, [N], Ns_),
  graph_term(G_, Ns_, Es, D).

% Convert between edge-clause and human-friendly forms in either flow order.
% Note that isolated nodes are lost in the (?, +) flow order as edge-clause form
% has no way to represent them.
ec_hf(EC, HF) :- nonvar(EC), !, ec_gt(EC, GT), gt_hf(GT, HF).
ec_hf(EC, HF) :- gt_hf(GT, HF), ec_gt(EC, GT).

% Convert between adjacency-list and human-friendly forms in either flow order.
% Note that adjacency-list form doesn't distinguish between graphs and digraphs,
% so the human-friendly form in (+, ?) flow order always uses arcs.
al_hf(AL, HF) :- nonvar(AL), !, gt_al(GT, AL), gt_hf(GT, HF).
al_hf(AL, HF) :- gt_hf(GT, HF), gt_al(GT, AL).


% 6.02 (**) Path from one node to another one.
% Write a predicate path(G, A, B, P) to find an acyclic path P from node A to
% node B in the graph G. The predicate should return all paths via backtracking.

% Flow pattern: (+, +, +, ?). G is in adjacency-list form.

path(G, A, B, P) :- empty_assoc(S), path(G, A, B, P, S).

path(_, A, A, [A], _) :- !.
path(G, A, B, [A|P], Seen) :-
  \+ get_assoc(A, Seen, _), !,
  put_assoc(A, Seen, true, Seen_),
  neighbour_al(A, N, G),
  path(G, N, B, P, Seen_).

neighbour_al(A, B, [n(A, N)|_]) :- maplist(strip_label, N, N1), member(B, N1).
neighbour_al(A, B, [_|AL]) :- neighbour_al(A, B, AL).

strip_label(X/_, X) :- !.
strip_label(X, X).


% 6.03 (*) Cycle from a given node.
% Write a predicate cycle(G, A, P) to find a closed path (cycle) P starting at a
% given node A in the graph G. The predicate should return all cycles via
% backtracking.

% Note that only cycles passing through each intermediate node once are
% returned to avoid infinite sequences of internally-cycling cycles.
% Flow pattern: (+, +, ?). G is in adjacency-list form.

cycle(G, A, [A|P]) :- neighbour_al(A, N, G), path(G, N, A, P).


% 6.04 (**) Construct all spanning trees
% Write a predicate s_tree(Graph,Tree) to construct (by backtracking) all
% spanning trees of a given graph. When you have a correct solution for the
% s_tree/2 predicate, use it to define two other useful predicates:
% is_tree(Graph) and is_connected(Graph). Both are five-minutes tasks!

s_tree(Graph, Tree) :-
  graph_term(Graph, Ns, Es, D),
  Ns = [N|_],
  s_tree([N], Es, Ns, Es1),    % Ns fails to unify if G is disconnected
  graph_term(Tree, Ns, Es1, D).

s_tree(Seen, Es, Seen2, Es3) :-
  select(E, Es, Es1),
  valid_addition(E, Seen),
  edge_terms(E, _, _, Ns),
  ord_add_all(Seen, Ns, Seen1),
  s_tree(Seen1, Es1, Seen2, Es2),
  ord_union(Es2, [E], Es3).
s_tree(Seen, Es, Seen, []) :-
  maplist({Seen}/[E]>>(\+ valid_addition(E, Seen)), Es).

valid_addition(A, Seen) :-
  functor(A, a, _), !,
  edge_terms(A, _, _, [P, Q]),
  ord_memberchk(P, Seen), \+ ord_memberchk(Q, Seen).
valid_addition(E, Seen) :-
  edge_terms(E, _, _, [P, Q]),
  ( ord_memberchk(P, Seen), \+ ord_memberchk(Q, Seen)
  ; ord_memberchk(Q, Seen), \+ ord_memberchk(P, Seen)).

is_tree(Graph) :- s_tree(Graph, Graph).

is_connected(Graph) :- s_tree(Graph, _).


% 6.05 (**) Construct the minimal spanning tree.
% Write a predicate ms_tree(Graph, Tree, Sum) to construct the minimal spanning
% tree of a given labelled graph. Hint: Use the algorithm of Prim. A small
% modification of the solution of 6.04 does the trick.

% On backtracking, this solution produces all other spanning trees in
% decreasingly minimal order. Flow order: (+, ?, ?).

ms_tree(Graph, Tree, Sum) :-
  graph_term(Graph, Ns, Es, D),
  Ns = [N|_],
  predsort(label_compare, Es, SEs),
  s_tree([N], SEs, Ns, Es1),
  graph_term(Tree, Ns, Es1, D),
  foldl([N, Acc, R]>>(label(N, L), R is Acc + L), Es1, 0, Sum).

label(e(_, _, L), L).
label(a(_, _, L), L).

label_compare(O, A, B) :- label(A, L1), label(B, L2), compare(O, L1-A, L2-B).


% 6.06 (**) Graph isomorphism.
% Two graphs G1(N1, E1) and G2(N2, E2) are isomorphic if there is a bijection
% f: N1 -> N2 such that for any nodes X, Y of N1, X and Y are adjacent if and
% only if f(X) and f(Y) are adjacent.

% Write a predicate that determines whether two graphs are isomorphic. Hint: Use
% an open-ended list to represent the function f.

isomorphic(A, B) :- isomorphic(A, B, _).

isomorphic(A, B, []) :- graph_term(A, [], [], D), graph_term(B, [], [], D).
isomorphic(A, B, [AN=BN|I]) :-
  graph_term(A, [AN|ANs], [], D), graph_term(B, BNs, [], D),
  select(BN, BNs, BNs1),
  graph_term(A1, ANs, [], D), graph_term(B1, BNs1, [], D),
  isomorphic(A1, B1, I).
isomorphic(A, B, I1) :-
  graph_term(A, ANs, [AE|AEs], D), graph_term(B, BNs, BEs, D),
  select(BE, BEs, BEs1),
  edge_terms(AE, _, _, AENs),
  ord_del_all(ANs, AENs, ANs1),
  edge_terms(BE, _, _, BENs),
  ord_del_all(BNs, BENs, BNs1),
  graph_term(A1, ANs1, AEs, D), graph_term(B1, BNs1, BEs1, D),
  isomorphic(A1, B1, I),
  equate(AE, BE, I, I1).

ord_del_all(S1, Es, S2) :-
  foldl([E, S1, S2]>>ord_del_element(S1, E, S2), Es, S1, S2).

equate(A, B, I, I) :-
  functor(A, a, _),
  edge_terms(A, _, _, [A1,A2]), edge_terms(B, _, _, [B1,B2]),
  bound(A1=B1, I), bound(A2=B2, I), !.
equate(A, B, I, [A2=B2|I]) :-
  functor(A, a, _),
  edge_terms(A, _, _, [A1,A2]), edge_terms(B, _, _, [B1,B2]),
  bound(A1=B1, I), free(A2=B2, I), !.
equate(A, B, I, [A1=B1|I]) :-
  functor(A, a, _),
  edge_terms(A, _, _, [A1,A2]), edge_terms(B, _, _, [B1,B2]),
  free(A1=B1, I), bound(A2=B2, I), !.
equate(A, B, I, [A1=B1, A2=B2|I]) :-
  functor(A, a, _),
  edge_terms(A, _, _, [A1,A2]), edge_terms(B, _, _, [B1,B2]),
  free(A1=B1, I), free(A2=B2, I).
equate(A, B, I, I1) :-
  functor(A, e, _),
  edge_terms(A, _, _, [A1,A2]), edge_terms(B, _, _, [B1,B2]),
  equate(a(A1, A2), a(B1, B2), I, I1).
equate(A, B, I, I1) :-
  functor(A, e, _),
  edge_terms(A, _, _, [A1,A2]), edge_terms(B, _, _, [B1,B2]),
  equate(a(A1, A2), a(B2, B1), I, I1).

bound(A=B, I) :- memberchk(A=B, I).
free(A=B, I) :- \+ bound(A=_, I), \+ bound(_=B, I).
