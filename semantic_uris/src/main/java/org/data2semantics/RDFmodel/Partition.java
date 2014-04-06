package org.data2semantics.RDFmodel;

import java.util.ArrayList;
import java.util.BitSet;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;
import javax.json.JsonBuilderFactory;

public class Partition {

	private Map<Integer, ObjInfo> _id2info = new HashMap<Integer, ObjInfo>();
	private List<Integer> _class_sizes = new ArrayList<Integer>();
	private BitSet _binary_representation = null;
	
	public Partition() {}
	
	public boolean equivalent(Partition p) {
		if (num_classes()!=p.num_classes()) return false;
		int [] class_map = new int[num_classes()];
		for (int i=0; i<num_classes(); i++) class_map[i]=-1;
		for (Entry<Integer,ObjInfo> entry : p._id2info.entrySet()) {
			ObjInfo mystuff = look_up(entry.getKey());
			assert mystuff != null : "Different domains in partition?!";
			int c1 = mystuff.get_class(), c2 = entry.getValue().get_class();
			if (class_map[c1]==-1) {
				class_map[c1] = c2;
			} else {
				if (class_map[c1] != c2) return false;
			}
		}
		return true;
	}
	
	public Partition(BitSet binary_representation) { _binary_representation = binary_representation; }	
	public BitSet get_binary_representation() { return _binary_representation; }
	
	public ObjInfo look_up(int obj_id) {
		return _id2info.get(obj_id);
	}

	public int class_size(int obj_class) {
		return _class_sizes.get(obj_class);
	}

	public int num_classes() {
		return _class_sizes.size();
	}

	public int new_class() {
		int n = _class_sizes.size();
		_class_sizes.add(0);
		return n;
	}
	
	@Override public String toString() { 
		StringBuilder sb = new StringBuilder("( "+num_classes()+") <");
		boolean first = true;
		for (int size : _class_sizes) {
			if (first) first=false; else sb.append("|"); 
			sb.append(size);
		}
		sb.append(">");
		return sb.toString();
	}

	public JsonArray asJsonArray() {
		JsonArrayBuilder jab = Json.createBuilderFactory(null).createArrayBuilder();
		for (int size : _class_sizes) jab.add(size);
		return jab.build();
	}
	
	public void add(int obj_class, int obj_id) {
		int nclasses = _class_sizes.size();
		assert obj_class < nclasses : "Bad object class";
		assert !_id2info.containsKey(obj_id) : "Only distinct objects may be added to ObjectClass";
		Integer ix_in_class = _class_sizes.get(obj_class);
		_class_sizes.set(obj_class, ix_in_class + 1);
		ObjInfo obj_info = new ObjInfo(obj_class, ix_in_class);
		_id2info.put(obj_id, obj_info);
	}

	public static class ObjInfo {
		private int _obj_class, _ix_in_class;

		public ObjInfo(int obj_class, int ix_in_class) {
			_obj_class = obj_class;
			_ix_in_class = ix_in_class;
		}

		public int get_class() { return _obj_class;   }
		public int get_ix()    { return _ix_in_class; }
	}

	// Used by greedy_search. It has nothing to do with real tboxes.
	private static Partition tbox2partition(RDFGraph G, Set<Integer> tbox, Set<Integer> fixed_class_set) {
		Partition partition = new Partition();

		final int[] type_val = { TermType.NAMED, TermType.BNODE, TermType.LITERAL };
		final int[] type_class = { partition.new_class(), partition.new_class(),
		    partition.new_class() };
		final int[] num = { G._nnamed, G._nbnodes, G._nlits };
		int fixed_class = -1; 
		if (fixed_class_set!=null) fixed_class = partition.new_class();

		for (int type = 0; type < 3; type++) {
			for (int ix = 0; ix < num[type]; ix++) {
				int id = TermType.ix2id(type_val[type], ix);
				int cls;
				if (fixed_class_set!=null && fixed_class_set.contains(id)) {
					cls = fixed_class;
				} else {
					cls = tbox.contains(id) ? partition.new_class() : type_class[type];
				}
				partition.add(cls, id);
			}
		}
		return partition;
	}

	
	public static Partition greedy_search(RDFGraph G, Set<Integer> fixed_class) {
		System.out.println("Looking for best partition...");
		List<Entry<Integer, Integer>> items = new ArrayList<Entry<Integer, Integer>>(
		    G.num_incoming().entrySet());
		Collections.sort(items, new ByValue<Integer>());
		int last_included = -1;
		Set<Integer> tbox_best = new HashSet<Integer>();
		Partition partition_best = tbox2partition(G, tbox_best, fixed_class);
		double cl = RDFhelper.encode("test", G, partition_best).getResults().L();
		for (int ix = 0; ix < items.size() && ix-last_included < Math.max(20,  0.1*last_included); ix++) {
			Entry<Integer, Integer> entry = items.get(items.size() - ix - 1);
			int obj_id = entry.getKey();
			if (fixed_class!=null && fixed_class.contains(obj_id)) continue;
			Set<Integer> tbox_test = new HashSet<Integer>(tbox_best);
			tbox_test.add(entry.getKey());
			Partition partition_test = tbox2partition(G, tbox_test, fixed_class);
			double cl_new = RDFhelper.encode("test", G, partition_test).getResults().L();
			if (cl_new < cl) {
				// add this resource to the tbox
				cl = cl_new;
				tbox_best = tbox_test;
				partition_best = partition_test;
				last_included = ix;
			}
		}
		System.out.println("Done, partition is "+partition_best);
		return partition_best;
	}
	
	public static Partition single(RDFGraph G) {
		Partition partition = new Partition();
		int cls = partition.new_class();
		for (int id : G) partition.add(cls, id);
		return partition;
	}
	
	public static Partition by_node_type(RDFGraph G) {
		Partition partition = new Partition();
		int named_cls = -1, bnode_cls = -1, lit_cls = -1;
		for (int id : G) {
			switch (TermType.id2type(id)) {
			case TermType.NAMED:
				if (named_cls==-1) named_cls = partition.new_class();
				partition.add(named_cls, id);
				break;
			case TermType.BNODE:
				if (bnode_cls==-1) bnode_cls = partition.new_class();
				partition.add(bnode_cls, id);
				break;
			case TermType.LITERAL:
				if (lit_cls==-1) lit_cls = partition.new_class();
				partition.add(lit_cls, id);
				break;
			default: throw new RuntimeException("This can't happen");
			}
		}
		return partition;
	}
	
	
	public static Partition by_type_set(RDFGraph G, int type_pred_id) {
		Partition partition = new Partition();
		Map<Set<Integer>, Integer> ts_map = new HashMap<Set<Integer>, Integer>();
		int lit_cls = -1;
		for (int id : G) {
			if (TermType.is_literal(id)) {
				if (lit_cls==-1) lit_cls = partition.new_class();
				partition.add(lit_cls, id);
			} else {
				Term t = G.get_term(id);
				Set<Integer> type_set = t.get_obj_set(type_pred_id);
				Integer cls = ts_map.get(type_set);
				if (cls==null) {
					cls = partition.new_class();
					ts_map.put(type_set, cls);
				}
				partition.add(cls, id);
			}
		}
		return partition;
	}
	
	public static Partition by_pred_set(RDFGraph G) {
		Partition partition = new Partition();
		Map<Set<Integer>, Integer> ts_map = new HashMap<Set<Integer>, Integer>();
		int lit_cls = -1;
		for (int id : G) {
			if (TermType.is_literal(id)) {
				if (lit_cls==-1) lit_cls = partition.new_class();
				partition.add(lit_cls, id);
			} else {
				Term t = G.get_term(id);
				Set<Integer> pred_set = t.get_property_set();
				Integer cls = ts_map.get(pred_set);
				if (cls==null) {
					cls = partition.new_class();
					ts_map.put(pred_set, cls);
				}
				partition.add(cls, id);
			}
		}
		return partition;
	}
	
	public static Partition from_fingerprint(RDFGraph G, Partition old_partition) {
		Partition partition = new Partition();
		Map<Fingerprint,Integer> fp2class = new HashMap<Fingerprint,Integer>();
		int lit_cls = -1;
		for (int id : G) {
			Integer cls;
			if (TermType.is_literal(id)) {
				if (lit_cls == -1) lit_cls = partition.new_class();
				cls = lit_cls;
			} else {
				Term t = G.get_term(id);
				if (t==null) t = new Term();
				Fingerprint fp = new Fingerprint(old_partition, t);
				cls = fp2class.get(fp);
				if (cls==null) {
					cls = partition.new_class();
					fp2class.put(fp,  cls);
				}
			}
			partition.add(cls, id);
		}
		return partition;
	}
	
	
	
	public static Partition by_predset_heuristic(RDFGraph G) {
		PredsetHeuristic ph = new PredsetHeuristic(G);
		return PredsetHeuristic.search(ph);
	}
	
	public static CoderFactory<Partition> get_coder_factory(int nnamed, int nbnodes, int nlits) {
		return new PartitionCoderFactory(nnamed, nbnodes, nlits);
	}
	
	private static class PartitionCoderFactory implements CoderFactory<Partition> {
		private int _nnamed, _nbnodes, _nlits;
		public PartitionCoderFactory(int nnamed, int nbnodes, int nlits) { 
			_nnamed = nnamed; _nbnodes = nbnodes; _nlits = nlits; 
		}
		@Override public Coder<Partition> build() {
			return new PartitionCoder(_nnamed, _nbnodes, _nlits);
		}
	}
	
	private static class PartitionCoder implements Coder<Partition> {
		
		private int _nnamed, _nbnodes, _nlits;
		public PartitionCoder(int nnamed, int nbnodes, int nlits) { 
			_nnamed = nnamed; _nbnodes = nbnodes; _nlits = nlits; 
		}
		
		@Override public void encode(CoderContext C, Partition p) {
			BitSet bs = p.get_binary_representation();
			if (bs != null) {
				encode_partition_using_uris(C, (BitSet) bs.clone());
			} else {
				encode_partition_normally(C, p);
			}
		}
		
		void encode_partition_using_uris(CoderContext C, BitSet bs) {
			for (int i=0; !bs.isEmpty(); i++) { 
				boolean bit = bs.get(i);
				C._c_boundary.encode(C, bit ? 1 : 0);
				if (bit) bs.clear(i);
			}	
		}
		
		void encode_partition_normally(CoderContext C, Partition p) {
			Partition partition = C.get_partition();
			C._c_subjtype.set_conditional(TermType.NAMED);
			for (int ix=0; ix<_nnamed; ix++) {
				int named_id = TermType.ix2id(TermType.NAMED, ix);
				C._c_subj_class.encode(C, partition.look_up(named_id).get_class());
			}
			C._c_subjtype.set_conditional(TermType.BNODE);
			C._c_urinode.set_conditional(null);
			for (int ix=0; ix<_nbnodes; ix++) {
				int bnode_id = TermType.ix2id(TermType.BNODE, ix);
				C._c_subj_class.encode(C, partition.look_up(bnode_id).get_class());
			}
			C._c_subjtype.set_conditional(TermType.NAMED);
			for (int ix=0; ix<_nlits; ix++) {
				int lit_id = TermType.ix2id(TermType.LITERAL, ix);
				C._c_subj_class.encode(C, partition.look_up(lit_id).get_class());
			}
		}			
	}

	public JsonArray get_json() {
		JsonBuilderFactory fact = Json.createBuilderFactory(null);
		JsonArrayBuilder jab = fact.createArrayBuilder();
		for (int cls=0; cls<_class_sizes.size(); cls++) {
			JsonArrayBuilder jb_named = fact.createArrayBuilder();
			JsonArrayBuilder jb_bnode = fact.createArrayBuilder();
			JsonArrayBuilder jb_lits  = fact.createArrayBuilder();
			for (Entry<Integer,ObjInfo> entry : _id2info.entrySet()) {
				if (entry.getValue().get_class() == cls) {
					int id = entry.getKey();
					int ix = TermType.id2ix(id);
					switch (TermType.id2type(id)) {
					case TermType.NAMED:   jb_named.add(ix); break;
					case TermType.BNODE:   jb_bnode.add(ix); break;
					case TermType.LITERAL: jb_lits.add(ix);  break;
					default: throw new RuntimeException("Should not happen!");
					}
				}
			}
			JsonArrayBuilder jb_all = fact.createArrayBuilder();
			jab.add(jb_all.add(jb_named.build()).add(jb_bnode.build()).add(jb_lits.build()).build());
		}
		return jab.build();
	}
}
