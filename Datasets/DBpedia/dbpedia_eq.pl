:- module(
  dbpedia_eq,
  [
    dbpedia_eq/3 % +Resource:iri
                 % -EntityConfidence:between(0.0,1.0)
                 % -QuantityConfidence:between(0.0,1.0)
  ]
).

/** <module> DBpedia Entity/Quantity

In Qualitative Reasoning it is important to distinguish between
 things whose fundamental property changes over time, called *quantities*,
 and things whose fundamental properties do not change over time,
 called *entities*.
It would therefore be neat to have a predicate that tells us
 whether something is an entity or a quantity (assuming it is either).

@author Wouter Beek
@version 2014/02
*/


dbpedia_eq(Resource, 
  


