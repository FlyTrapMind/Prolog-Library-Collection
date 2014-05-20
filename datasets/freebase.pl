:- module(
  freebase,
  [
    freebase_download_latest/1 % ?File:atom
  ]
).

/** <module> Freebase

Support for the Freebase dataset.

@author Wouter Beek
@version 2014/04
*/

:- use_module(library(sgml)).
:- use_module(library(uri)).
:- use_module(library(xpath)).

:- use_module(generics(uri_ext)).
:- use_module(http(http_download)).
:- use_module(xml(xml_dom)).
:- use_module(xsd(xsd)).



freebase_download_latest(LatestFile):-
  freebase_latest_url(LatestUrl),
  download_to_file(LatestUrl, LatestFile).


freebase_latest_url(LatestUrl):-
  freebase_all_url(AllUrl),
  xml_url_to_dom(AllUrl, Dom),
  findall(
    LastModified-Url,
    freebase_entry(AllUrl, Dom, Url, LastModified),
    Pairs1
  ),
  keysort(Pairs1, Pairs2),
  last(Pairs2, _-LatestUrl).


%! freebase_entry(
%!   +AllUrl:atom,
%!   +Dom:list,
%!   -Url:atom,
%!   -LastModified:compound
%! ) is nondet.

freebase_entry(AllUrl, Dom, Url, LastModified):-
  xpath(Dom, //'Contents', Entry),
  xpath_chk(Entry, //'LastModified'(text), LexicalForm),
  once(xsd_lexical_map(xsd:dateTime, LexicalForm, LastModified)),
  xpath_chk(Entry, //'Key'(text), Key),
  relative_url_path(Url, AllUrl, Key).


%! freebase_all_url(-AllUrl:atom) is det.

freebase_all_url(Url):-
  uri_components(
    Url,
    uri_components(
      http,
      'commondatastorage.googleapis.com',
      '/freebase-public/',
      _,
      _
    )
  ).

