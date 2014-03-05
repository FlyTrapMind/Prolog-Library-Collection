:- module(
  rdf_dataset,
  [
    rdf_dataset/3, % ?DefaultGraph:atom
                   % ?NamedGraphs:list(iri)
                   % ?Dataset:compound
    rdf_default_graph/2, % ?Dataset:compound
                         % ?DefaultGraph:atom
    rdf_graph/2, % ?Dataset:compound
                 % ?Graph:atom
    rdf_named_graph/2 % ?Dataset:compound
                      % ?NamedGraph:iri
  ]
).

/** <module> RDF Dataset

# RDF 1.1

An *|RDF dataset|* is a collection of RDF graphs.
All but one of these graphs have an associated IRI or blank node.
They are called *|named graphs|*,
 and the IRI or blank node is called the *|graph name|*.
The remaining graph does not have an associated IRI,
 and is called the *|default graph|* of the RDF dataset.

# SPARQL 1.1

The RDF data model expresses information as graphs
 consisting of triples with subject, predicate and object.
Many RDF data stores hold multiple RDF graphs
 and record information about each graph,
 allowing an application to make queries that involve
 information from more than one graph.

An RDF Dataset may contain zero named graphs;
 an RDF Dataset always contains one default graph.

The definition of RDF Dataset does not restrict
 the relationships of named and default graphs.
Information can be repeated in different graphs;
 relationships between graphs can be exposed.

RDF data can be combined by the RDF merge [RDF-MT] of graphs.

\cite{sparql11}

The fundamental concept of VoID is the dataset.
A dataset is a set of RDF triples that are published,
 maintained or aggregated by a single provider.
Unlike RDF graphs, which are purely mathematical constructs,
 the term dataset has a social dimension:
 we think of a dataset as a meaningful collection of triples,
 that deal with a certain topic,
 originate from a certain source or process,
 are hosted on a certain server,
 or are aggregated by a certain custodian.
Also, typically a dataset is accessible on the Web,
 for example through resolvable HTTP URIs or through a SPARQL endpoint,
 and it contains sufficiently many triples that there is benefit
 in providing a concise summary.

Since most datasets describe a well-defined set of entities,
 datasets can also be seen as a set of descriptions of certain entities,
 which often share a common URI prefix
 (such as http://dbpedia.org/resource/).

In VoID, a dataset is modelled as an instance of the `void:Dataset` class.
Such a `void:Dataset` instance is
 a single RDF resource that represents the entire dataset,
 and thus allows us to easily make statements about the entire dataset
 and all its triples.

The relationship between a `void:Dataset` instance
 and the concrete triples contained in the dataset
 is established through access information,
 such as the address of a SPARQL endpoint where the triples can be accessed.

\cite{void2011}

# Definition

## RDF Dataset

An RDF dataset is a set:
\[
  \bigunion_{1 \leq i \leq n} \set{\pair{u_i}{G_i}} \union $\set{G}
\]
where $G$ and each $G_i$ are graphs, and each $u_i$ is an IRI.
Each $u_i$ is distinct.

\cite{sparql11}

## Default Graph & Named Graph

$G$ is called the default graph. $\pair{u_i}{G_i}$ are called named graphs.

\cite{sparql11}

## Active Graph

The active graph is the graph from the dataset used for
 basic graph pattern matching.

\cite{sparql11}

## RDF Dataset Merge

Let $DS_1 = \bigunion_{1 \leq i \leq n} \set{\pair{u_{1,i}}{G_{1,i}}} \union \set{G_1}$
and $DS_2 = \bigunion_{1 \leq i \leq m} \set{\pair{u_{2,i}}{G_{2,i}}} \union \set{G_2}$.

We define the RDF Dataset Merge of $DS_1$ and $DS_2$ to be
 $\bigunion_{1 \leq i \leq k} \set{\pair{u_i}{G_i}} \union \set{G}$,
 where:

Write $N_1$ for $\bigunion_{1 \leq j \leq n} \set{u_{1,j}}$,
  and $N_2$ for $\bigunion_{1 \leq j \leq m} \set{u_{2,j}}$.

\begin{itemize}
  \item $G$ is the merge of $G_1$ and $G_2$.
  \item $\pair{u_i}{G_i}$ where $u_i$ is in $N_1$ but not in $N_2$.
  \item $\pair{u_i}{G_i}$ where $u_i$ is in $N_2$ but not in $N_1$.
  \item $\pair{u_i}{G_i}$ where $u_i$ is equal to $u_j \in N_1$
        and equal to $u_k \in N_2$
        and $G_i$ is the merge of $G_{1,j}$ and $G_{2,k}$.
\end{itemize}

\cite{sparql11}

# Pragmatics

Two useful arrangements are:
  - to have information in the default graph that includes
    provenance information about the named graphs
  - to include the information in the named graphs
    in the default graph as well.
  - to have the default graph be the RDF merge of some or all
    of the information in the named graphs.

\cite{sparql11}

# Querying: SPARQL

A SPARQL query is executed against an RDF Dataset
 which represents a collection of graphs.
An RDF Dataset comprises one graph, the default graph,
 which does not have a name, and zero or more named graphs,
 where each named graph is identified by an IRI.
A SPARQL query can match different parts of the query pattern
 against different graphs.

The graph that is used for matching a basic graph pattern
 is the active graph.
The `GRAPH` keyword is used to make the active graph
 one of all of the named graphs in the dataset for part of the query.

A query does not need to involve matching the default graph;
 the query can just involve matching named graphs.

\cite{sparql11}

--

@author Wouter Beek
@tbd rdf_dataset_merge(+Dataset1:compound, +Dataset2:compound, -MergedDataset:compound) is det.
@version 2013/09-2013/10, 2014/03
*/

:- use_module(library(lists)).



%! rdf_create_dataset(
%!   +DefaultGraph:atom,
%!   +NamedGraphs:list(pair(iri,atom)),
%!   +Dataset:compound
%! ) is semidet.
%! rdf_create_dataset(
%!   +DefaultGraph:atom,
%!   +NamedGraphs:list(pair(iri,atom)),
%!   -Dataset:compound
%! ) is det.
%! rdf_create_dataset(
%!   -DefaultGraph:atom,
%!   -NamedGraphs:list(pair(iri,atom)),
%!   +Dataset:compound
%! ) is det.

rdf_dataset(DefaultGraph, NamedGraphs, rdf_dataset(DefaultGraph,NamedGraphs)).


%! rdf_default_graph(+Dataset:compound, +DefaultGraph:atom) is semidet.
%! rdf_default_graph(+Dataset:compound, -DefaultGraph:atom) is det.

rdf_default_graph(rdf_dataset(DefaultGraph,_), DefaultGraph).


%! rdf_graph(+Dataset:compound, +Graph:atom) is semidet.
%! rdf_graph(+Dataset:compound, -Graph:atom) is nondet.

rdf_graph(Dataset, DefaultGraph):-
  rdf_default_graph(Dataset, DefaultGraph).
rdf_graph(Dataset, NamedGraph):-
  rdf_named_graph(Dataset, NamedGraph).


%! rdf_named_graph(+Dataset:compound, +NamedGraph:atom) is semidet.
%! rdf_named_graph(+Dataset:compound, -NamedGraph:atom) is nondet.

rdf_named_graph(rdf_dataset(_,NamedGraphs), NamedGraph):-
  member(NamedGraph, NamedGraphs).
