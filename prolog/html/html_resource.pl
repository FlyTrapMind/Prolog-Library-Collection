:- module(html_resource, []).
:- reexport(library(http/html_head)).

/** <module> HTML resource

Initialize locations for serving HTML resources.

@author Wouter Beek
@version 2015/08, 2016/02, 2016/04
*/

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_server_files)).

:- dynamic
    http:location/3,
    user:file_search_path/2,
    user:prolog_file_type/2.

:- multifile
    http:location/3,
    user:file_search_path/2,
    user:prolog_file_type/2.

http:location(css,   root(css),   []).
http:location(fonts, root(fonts), []).
http:location(icon,  root(icon),  []).
http:location(img,   root(img),   []).
http:location(js,    root(js),    []).
http:location(mp3,   root(mp3),   []).
http:location(pdf,   root(pdf),   []).

user:prolog_file_type(htm,  html).
user:prolog_file_type(html, html).

user:file_search_path(css,   library(resource/css)).
user:file_search_path(fonts, library(resource/fonts)).
user:file_search_path(icon,  library(resource/icon)).
user:file_search_path(img,   library(resource/img)).
user:file_search_path(js,    library(resource/js)).
user:file_search_path(mp3,   library(resource/mp3)).
user:file_search_path(pdf,   library(resource/pdf)).

:- http_handler(css(.),   serve_files_in_directory(css),   [prefix]).
:- http_handler(fonts(.), serve_files_in_directory(fonts), [prefix]).
:- http_handler(icon(.),  serve_files_in_directory(icon),  [prefix]).
:- http_handler(img(.),   serve_files_in_directory(img),   [prefix]).
:- http_handler(js(.),    serve_files_in_directory(js),    [prefix]).
:- http_handler(mp3(.),   serve_files_in_directory(mp3),   [prefix]).
:- http_handler(pdf(.),   serve_files_in_directory(pdf),   [prefix]).
