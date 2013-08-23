# Prolog Generics Collection (PGC)

This is a collection of predicates that I use across projects.
This includes extensions to SWI-Prolog,
or to one of the standard Prolog libraries;
support for modeling languages and visualization techniques.

## Contents

This repository contains the following subcollections of Prolog modules:
  * Datasets
  * DCG
  * Generics
  * Graph theory
    * DGRAPH
    * RDF Graph
    * UGRAPH
  * Inductive Logic Programming (ILP)
  * Logic
    * RDF Model Theory
  * Mathematics
  * Operating System interaction
  * Server
  * Standards
    * DateTime
    * Geography
    * GraphViz
    * HTML
    * HTTP
    * Language
    * OWL
    * RDF
    * RDFS
    * SPARQL
    * SVG
    * Tests
    * URI
    * XML
      * XML Schema (XSD)
  * Truth-Maintenance System (TMS)
  * Vocabularies
    * SKOS

## Reuse

The following Git commands can be used to include this repository in
other projects:

~~~{.bash}
git remote add generics https://github.com/wouterbeek/PrologGenerics.git
git fetch generics
git merge generics/master
~~~

@author Wouter Beek
@version alpha1
