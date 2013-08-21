load_rdfs_schema:-
  absolute_file_name(rdfs(rdfs), File, [access(read), file_type(rdf)]),
  rdf_load(File, [graph(rdfs_schema)]).

