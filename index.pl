% Index for the Prolog Library Collection repository.

index(ParentDir):-
  directory_file_path(ParentDir, plc, PlcDir),
  assert(user:file_search_path(plc, PlcDir)),
    assert(user:file_search_path(ap,              plc(ap))),
    assert(user:file_search_path(datasets,        plc(datasets))),
      assert(user:file_search_path(dbpedia,         datasets(dbpedia))),
    assert(user:file_search_path(dcg,             plc(dcg))),
      assert(user:file_search_path(flp,             dcg(flp))),
      assert(user:file_search_path(nlp,             dcg(nlp))),
    assert(user:file_search_path(generics,        plc(generics))),
    assert(user:file_search_path(graph_theory,    plc(graph_theory))),
      assert(user:file_search_path(dgraph,          graph_theory(dgraph))),
      assert(user:file_search_path(rdf_graph,       graph_theory(rdf_graph))),
      assert(user:file_search_path(ugraph,          graph_theory(ugraph))),
    assert(user:file_search_path(ilp,             plc(ilp))),
    assert(user:file_search_path(lod,           plc(lod))),
      assert(user:file_search_path(owl,             lod(owl ))),
      assert(user:file_search_path(rdf,             lod(rdf))),
        assert(user:file_search_path(rdf_conv,        rdf(conversion))),
        assert(user:file_search_path(rdf_file,        rdf(file))),
        assert(user:file_search_path(rdf_man,         rdf(management))),
        assert(user:file_search_path(rdf_mt,          rdf(rdf_mt))),
        assert(user:file_search_path(rdf_reasoning,   rdf(reasoning))),
        assert(user:file_search_path(rdf_term,        rdf(term))),
        assert(user:file_search_path(rdf_web,         rdf(web))),
      assert(user:file_search_path(rdfs,            lod(rdfs))),
      assert(user:file_search_path(skos,            lod(skos))),
      assert(user:file_search_path(sparql,        lod(sparql))),
      assert(user:file_search_path(void,            lod(void))),
    assert(user:file_search_path(logic,           plc(logic))),
    assert(user:file_search_path(math,            plc(math))),
    assert(user:file_search_path(os,              plc(os))),
    assert(user:file_search_path(programming,     plc(programming))),
      assert(user:file_search_path(pl,              programming(prolog))),
        assert(user:file_search_path(pl_web,        pl(web))),
    assert(user:file_search_path(ps,              plc(pl))),
      assert(user:file_search_path(tms,             ps(tms))),
        assert(user:file_search_path(atms,            tms(atms))),
        assert(user:file_search_path(doyle,           tms(doyle))),
    assert(user:file_search_path(server,          plc(server))),
    assert(user:file_search_path(standards,       plc(standards))),
      assert(user:file_search_path(datetime,        standards(date_time))),
      assert(user:file_search_path(geo,             standards(geography))),
      assert(user:file_search_path(gv,              standards(graphviz))),
      assert(user:file_search_path(html,            standards(html))),
      assert(user:file_search_path(http,            standards(http))),
        assert(user:file_search_path(http_headers,    http(headers))),
        assert(user:file_search_path(http_parameters, http(parameters))),
      assert(user:file_search_path(lang,            standards(language))),
      assert(user:file_search_path(latex,           standards(latex))),
      assert(user:file_search_path(svg,             standards(svg))),
      assert(user:file_search_path(uri,             standards(uri))),
      assert(user:file_search_path(xml,             standards(xml))),
        assert(user:file_search_path(xsd,             xml(xsd))),
    assert(user:file_search_path(web,             plc(web))),
      assert(user:file_search_path(crawler,         web(crawler))).

