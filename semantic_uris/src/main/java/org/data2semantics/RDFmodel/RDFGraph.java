package org.data2semantics.RDFmodel;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.SortedMap;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.JsonObjectBuilder;
import javax.json.JsonWriter;

public class RDFGraph implements Iterable<Integer> {
	public int _nnamed = 0;
	public int _nbnodes = 0;
	public int _nlits = 0;
	public Set<Integer> _preds = new HashSet<Integer>();
	public int _ntriples = 0;

	public List<Term> _n_subj2term = new SoftList<Term>();
	public List<Term> _b_subj2term = new SoftList<Term>();

	public static boolean _verbose = false;
	
	public Term get_term(int id) {
		int ix = TermType.id2ix(id);
		if (TermType.is_named(id)) return _n_subj2term.get(ix);
		if (TermType.is_bnode(id)) return _b_subj2term.get(ix);
		return null;
	}
	
	private void add_term(int id) {
		int type = TermType.id2type(id);
		int ix = TermType.id2ix(id);
		switch (type) {
		case TermType.NAMED:   if (ix >= _nnamed)  { _nnamed  = ix + 1; } break;
		case TermType.BNODE:   if (ix >= _nbnodes) { _nbnodes = ix + 1; } break;
		case TermType.LITERAL: if (ix >= _nlits)   { _nlits   = ix + 1; } break;
		default: throw new RuntimeException("Eddies in the space-time continuum");
		}
	}

	private void add_triple(List<Integer> triple) {
		int subj_id = triple.get(0), pred_id = triple.get(1), obj_id = triple
		    .get(2);
		add_term(subj_id);
		add_term(pred_id);
		_preds.add(pred_id);
		add_term(obj_id);
		_ntriples++;

		// insert this link into the data structure
		int subj_ix = TermType.id2ix(subj_id);
		List<Term> terms = TermType.is_named(subj_id) ? _n_subj2term : _b_subj2term;
		Term t = terms.get(subj_ix);
		if (t == null) {
			t = new Term();
			terms.set(subj_ix, t);
		}
		SortedSet<Integer> objs = t.get(pred_id);
		if (objs == null) {
			objs = new TreeSet<Integer>(new TermType.IdComparator());
			t.put(pred_id, objs);
		}
		objs.add(obj_id);
	}

	public static void counter(String name, int n) {
		if (_verbose) {
			System.err.printf("\r%s: %d", name, n);
			System.err.flush();
		}
	}

	public static void percentage(String name, double p) {
		if (_verbose) {
			System.err.printf("\r%s: %4.1f%%", name, p * 100);
			System.err.flush();
		}
	}

	public RDFGraph(Iterable<List<Integer>> triples) {
		int i = 0;
		for (List<Integer> triple : triples) {
			add_triple(triple);

			if (i % 100000 == 0)
				counter("Loading graph", i);
			i++;
		}

		// initialise null entries in the data structure
		for (int ix = 0; ix < _nnamed; ix++)
			if (_n_subj2term.get(ix) == null)
				_n_subj2term.set(ix, new Term());

		for (int ix = 0; ix < _nbnodes; ix++)
			if (_b_subj2term.get(ix) == null)
				_b_subj2term.set(ix, new Term());
	}

  public void json_object_to_file(JsonObject json_object, File output_file) {
		try {
			FileOutputStream output_stream = new FileOutputStream(output_file);
			JsonWriter json_writer = Json.createWriter(output_stream);
			json_writer.writeObject(json_object);
			json_writer.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
  }

	public void printSomeStats(JsonObjectBuilder jb) {
		jb.add("triples", _ntriples).add("uris", _nnamed)
  		  .add("blank_nodes", _nbnodes).add("predicates", _preds.size())
		  .add("subjects", _n_subj2term.size() + _b_subj2term.size())
		  .add("literals", _nlits);
		
		System.out.println("#triples     :" + _ntriples);
		System.out.println("#uris        :" + _nnamed);
		System.out.println("#blank nodes :" + _nbnodes);
		System.out.println("#predicates  :" + _preds.size());
		System.out.println("#subjects    :"
		    + (_n_subj2term.size() + _b_subj2term.size()));
		System.out.println("#literals    :" + _nlits);
	}

	public List<SortedMap<Integer, SortedSet<Integer>>> all_subjects_in_order() {
		List<SortedMap<Integer, SortedSet<Integer>>> all_subjects = new ArrayList<SortedMap<Integer, SortedSet<Integer>>>();
		all_subjects.addAll(_n_subj2term);
		all_subjects.addAll(_b_subj2term);
		return all_subjects;
	}

	// returns the number of incoming edges for each resource id
	public Map<Integer, Integer> num_incoming() {
		HashMap<Integer, Integer> count_links = new HashMap<Integer, Integer>();
		for (SortedMap<Integer, SortedSet<Integer>> links : all_subjects_in_order()) {
			for (SortedSet<Integer> objs : links.values()) {
				for (int obj : objs) {
					Integer num = count_links.get(obj);
					count_links.put(obj, num == null ? 1 : num + 1);
				}
			}
		}
		return count_links;
	}
	
	public Set<Integer> get_targets(int pred_id) {
		Set<Integer> results = new HashSet<Integer>();
		for (Term t : _n_subj2term) {
			SortedSet<Integer> objs = t.get(pred_id);
			if (objs!=null) results.addAll(objs);
		}
		for (Term t: _b_subj2term) {
			SortedSet<Integer> objs = t.get(pred_id);
			if (objs!=null) results.addAll(objs);
		}
		return results;
	}

	@Override public Iterator<Integer> iterator() {	return new IdIterator(this); }
	
	private static class IdIterator implements Iterator<Integer> {
	
		private int _nnamed, _nbnodes, _nlits;
		private int _id;
		
		public IdIterator(RDFGraph G) {
			_nnamed  = G._nnamed;
			_nbnodes = G._nbnodes;
			_nlits   = G._nlits;
			// point _id to the first available term
			if      (_nnamed>0)  _id = TermType.named2id(0);
			else if (_nbnodes>0) _id = TermType.bnode2id(0);
			else if (_nlits>0)   _id = TermType.lit2id(0);
			else _id = -1;
		}
		
		@Override public boolean hasNext() { return _id!=-1; }
			
		@Override public Integer next() {
			assert hasNext() : "No more ids";
			int id = _id;
			int type = TermType.id2type(_id);
			int ix   = TermType.id2ix(_id);
			if (type == TermType.NAMED) {
				ix++;
				if (ix==_nnamed) { ix = -1; type = TermType.BNODE; }
			}
			if (type == TermType.BNODE) {
				ix++;
				if (ix==_nbnodes) { ix = -1; type = TermType.LITERAL; }
			}
			if (type == TermType.LITERAL) {
				ix++;
				if (ix==_nlits) ix=-1;
			}
			if (ix==-1) _id=-1; else _id = TermType.ix2id(type,  ix);
			return id;
		}

		@Override public void remove() {
			assert false : "Not implemented";
		}
	}
}
