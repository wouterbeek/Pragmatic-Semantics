package org.data2semantics.RDFmodel;

import java.util.ArrayList;
import java.util.BitSet;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Queue;

import javax.json.Json;
import javax.json.JsonArray;
import javax.json.JsonArrayBuilder;

import org.apache.commons.lang3.tuple.Pair;

public class Boundary extends HashSet<StringTree> {

	static final long serialVersionUID = 1910746224041087621L;
	
	private BitSet _binary_representation = null;

	public Boundary() {}
	
	public Boundary(Boundary other) {
		addAll(other);
		_binary_representation = other._binary_representation;
	}

	public BitSet get_binary_representation() { return _binary_representation; }
	
	public Boundary expand(StringTree node) {
		assert !node.isLeaf() : "Cannot expand a leaf node in the boundary";
		Boundary nw = new Boundary(this);
		nw.remove(node);
		nw.addAll(node.getChildren());
		nw._binary_representation = null;
		return nw;
	}

	private List<String> sorted_prefixes() { 
		List<String> items = new ArrayList<String>();
		for (StringTree node : this)
			items.add(node.pathFromRoot());
		Collections.sort(items);
		return items;
	}
	
	public JsonArray asJsonArray() {
		JsonArrayBuilder jab = Json.createBuilderFactory(null).createArrayBuilder();
		for (String prefix : sorted_prefixes()) jab.add(prefix);
		return jab.build();
	}
	
	@Override public String toString() {
		StringBuilder sb = new StringBuilder();
		for (String p : sorted_prefixes()) {
			sb.append(p);
			sb.append('\n');
		}
		return sb.toString();
	}

	public static Pair<Boundary,Partition> find_best(RDFGraph G, List<String> uris) {
		StringTree ST = new StringTree(uris);
		System.out.println("Looking for best boundary in uri tree...");
		Boundary B = new Boundary();
		B.add(ST);
		Partition P = B.to_partition(G, ST, uris);
		
		Queue<StringTree> Btodo = new LinkedList<StringTree>();
		Btodo.add(ST);

		double cl = Double.MAX_VALUE;

		while (!Btodo.isEmpty()) {
			StringTree node = Btodo.remove();
			if (node.isLeaf()) continue;
			Boundary Bnw = B.expand(node);
			Partition Pnw = Bnw.to_partition(G,  ST,  uris);
			double ncl = RDFhelper.encode("test", G, Pnw).getResults().L();
			if (ncl < cl) {
				B = Bnw;
				P = Pnw;
				cl = ncl;
				Btodo.addAll(node.getChildren());
			}
		}
		System.out.println("Done, size is "+B.size());		
		return Pair.of(B,P);
	}
	
	// Look up a particular URI in the Boundary
	private StringTree lookup(StringTree ST, String uri) {
		StringTree pos = ST;
		String path = uri;
		while (!contains(pos) && !path.isEmpty()) {
			pos = pos.lookup(path);
			if (pos == null) break;
			path = path.substring(pos.getEdgeLabelFromParent().length());
		}
		assert path != null : "URI " + uri + " not encountered in the boundary";
		return pos;
	}		
		                       	
	// Should have ST = new StringTree(uris)
	public Partition to_partition(RDFGraph G, StringTree ST, List<String> uris) {
		Partition p = new Partition(create_tree_rep(uris, ST));
		Map<StringTree, Integer> class_map = new HashMap<StringTree,Integer>();
		for (int named_ix=0; named_ix < G._nnamed; named_ix++) {
			StringTree urinode = lookup(ST, uris.get(named_ix));
			int cls;
			if (class_map.containsKey(urinode)) {
				cls = class_map.get(urinode);
			} else {
				cls = p.new_class();
				class_map.put(urinode,  cls);
			}
			int named_id = TermType.named2id(named_ix);
			p.add(cls,  named_id);
		}
		
		if (G._nbnodes > 0) {
			int bnode_cls = p.new_class();
			for (int bnode_ix=0; bnode_ix<G._nbnodes; bnode_ix++) {
				p.add(bnode_cls, TermType.bnode2id(bnode_ix));
			}
		}
		if (G._nlits > 0) {
			int lit_cls = p.new_class();
			for (int lit_ix=0; lit_ix<G._nlits; lit_ix++) {
				p.add(lit_cls, TermType.lit2id(lit_ix));
			}
		}
		return p;
	}
	
	private BitSet create_tree_rep(List<String> uris, StringTree ST) {
		BitSet bs = new BitSet();
		create_tree_rep_rec(bs, uris, ST, 0);
		assert !bs.isEmpty() : "Boundary should contain at least one leaf";
		return bs;
	}
	
	// returns first unused index
	private int create_tree_rep_rec(BitSet bs, List<String> uris, StringTree node, int index) {
		if (contains(node)) { bs.set(index); return index+1; }
		index++;
		for (StringTree ch : node.getChildren()) {
			index = create_tree_rep_rec(bs, uris, ch, index);
		}
		return index;
	}

}


