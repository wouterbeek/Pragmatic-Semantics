package org.data2semantics.RDFmodel;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintStream;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import javax.json.Json;
import javax.json.JsonObjectBuilder;

public class RDFbits extends RDFhelper {
	
	public static enum Heuristic { 
		NODETYPE ("nodetype"),
		TBOX     ("tbox"),
		URIS     ("uris"),
		TYPESET  ("typeset"),
		PREDSET  ("predset"),
		TEST     ("test");
		
		private String _name;
		private Heuristic(String name) { _name = name; }
		public String get_name() { return _name; }
	}
	
	public static Heuristic _heuristic;

	public static void main(String[] args) {
		
		boolean preprocess = args.length==3 && args[0].equals("preprocess");
		boolean compress   = args.length==3 && args[0].equals("compress");
				
		if (!(preprocess||compress)) {
			System.err.print("usage: java RDFbits preprocess source_dir target_dir\n" +
		                     "                    compress   heuristic source_dir\n" +
					         "            (heuristic is one of {nodetype, tbox, uris, typeset, predset, test})\n");
			System.exit(1);
		}
		
		if (preprocess) {
			Preprocess.load(args[1], args[2]);
		} else {
			_heuristic = null;
			for (Heuristic h : Heuristic.values()) {
				if (args[1].equalsIgnoreCase(h.get_name())) _heuristic = h;
			}
			if (_heuristic==null) throw new RuntimeException("Unrecognised heuristic '"+args[2]+"'");
			collect_statistics(args[2]);
		}
	}

	public static void collect_statistics(String dir) {
		RDFGraph G = new RDFGraph(new TripleFile(dir + "/triples.dat"));
		System.out.println("Memory used: " + (mem_used() / 1024) + "K");
		
		JsonObjectBuilder jb = Json.createBuilderFactory(null).createObjectBuilder();
		G.printSomeStats(jb);
		
		int type_pred_id = RDFhelper.get_uri_ids(dir, "http://www.w3.org/1999/02/22-rdf-syntax-ns#type")[0];
		/* Let's skip the type targets business for now.
		
			Set<Integer> type_targets = G.get_targets(type_pred_id);
			System.out.println("Separating "+type_targets.size()+" type targets");
			jb.add("type_targets", type_targets.size());
		*/
		

		// Find the partition based on the specified heuristic
		Partition partition;
		switch (_heuristic) {
		case NODETYPE: partition = Partition.by_node_type(G); break;
		case URIS:
			List<String> uris = null;
			try {
				uris = load_strings(dir + "/uris.txt");
			} catch (IOException e) {
				System.err.println("Can't load uri's: "+e);
				System.exit(1);
			}
			partition = Boundary.find_best(G, uris).getRight(); 
			break;
		case TBOX: partition    = Partition.greedy_search(G, null); break;
		case TYPESET: partition = Partition.by_type_set(G, type_pred_id); break;
		case PREDSET: partition = Partition.by_pred_set(G); break;
		case TEST: partition = Partition.by_predset_heuristic(G); break;
		default : throw new RuntimeException("Unimplemented heuristic");
		}
		
		jb.add("partition", partition.asJsonArray());

		CLAccountant res = encode("test", G, partition).getResults();
		CLAccountant.report(Arrays.asList(res));		
		
		jb.add("codelength", res.L());		
		
		try {
			report_terms(jb, new PrintStream(dir + "/terms.csv"), G, partition, type_pred_id);
			FileWriter fw = new FileWriter(dir+"/stats.json");
			fw.write(jb.build().toString());
			fw.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		try {
			FileWriter fw = new FileWriter(dir+"/partition.json");
			fw.write(partition.get_json().toString());
			fw.close();
		} catch (IOException e) {
			e.printStackTrace();
			System.exit(1);
		}
		

	}
	
	static void report_terms(JsonObjectBuilder jb, PrintStream out, RDFGraph G, Partition partition, int type_pred_id) {
		out.println("# type,ix,property_set_id,type_set_id,fingerprint_id,partition_cell");
		IndexMap<Set<Integer>> propset_map = new IndexMap<Set<Integer>>();
		IndexMap<Set<Integer>> typeset_map = new IndexMap<Set<Integer>>();
		IndexMap<Fingerprint> fingerprint_map = new IndexMap<Fingerprint>();
		int type[] = { TermType.NAMED, TermType.BNODE };
		int num[] = { G._nnamed, G._nbnodes };
		String typename[] = { "URI", "BNODE" };
		for (int type_ix=0; type_ix<2; type_ix++) {
			for (int ix=0; ix<num[type_ix]; ix++) {
				int id = TermType.ix2id(type[type_ix],  ix);
				Term t = G.get_term(id);
				Set<Integer> prop_set = t.get_property_set();
				Set<Integer> type_set = t.get_obj_set(type_pred_id);
				Fingerprint f         = t.get_fingerprint(partition);
				int cell_id = partition.look_up(id).get_class();
				out.printf("%s,%d,%d,%d,%d,%d\n", typename[type_ix], ix, 
						propset_map.map(prop_set), typeset_map.map(type_set), fingerprint_map.map(f),
						cell_id); 
			}
		}
		jb.add("typesets", typeset_map.size());
		jb.add("propsets", propset_map.size());
		jb.add("fingerprints", fingerprint_map.size());
	}
	

	/*
	 * returns the number of rdf graphs that can be specified using ntriples
	 * triples, using a domain with nres resources of which npred are predicates,
	 * and nlit literals.
	 */
	public static double log_ngraphs(long nuri, long nbnodes, long npred,
	    long nlit, long ntriples) {
		long nres = nuri + nbnodes;
		long n_res2res = ntriples - nlit; // number of res to res triples
		long n_res2lit = nlit; // number of res to lit triples

		double lgsum = Double.MIN_VALUE;
		for (long n_r2r_preds = 0; n_r2r_preds <= npred; n_r2r_preds++) {
			// assume n_r2r_preds predicates run from resource to resource\
			long n_r2l_preds = npred - n_r2r_preds;
			lgsum = Codes.lgsum(lgsum,
			    Codes.lgbinomial(nres * nres * n_r2r_preds, n_res2res));
			lgsum = Codes.lgsum(lgsum,
			    Codes.lgbinomial(nres * n_r2l_preds, n_res2lit));
		}
		return lgsum - Codes.lgfac(nbnodes);
	}

	public static long mem_used() {
		System.gc(); // this hack is the only easy way to get memory consumption
		Runtime rt = Runtime.getRuntime();
		return rt.totalMemory() - rt.freeMemory();
	}


}
