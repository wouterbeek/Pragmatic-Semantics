package org.data2semantics.RDFmodel;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.commons.lang3.tuple.Pair;

public class PredsetHeuristic {

	private SoftList<Set<Integer>> _predsets = new SoftList<Set<Integer>>();
	private SoftList<Set<Integer>> _termsets = new SoftList<Set<Integer>>();
	private Set<Integer> _classes = new HashSet<Integer>(); 
	private double [][] _similarities;
	private RDFGraph _G;
	
	// copy constructor
	public PredsetHeuristic(PredsetHeuristic other) {
		_G = other._G;
		_classes = new HashSet<Integer>(other._classes);
		_predsets = new SoftList<Set<Integer>>();
		_termsets = new SoftList<Set<Integer>>();
		for (int cls : _classes) {
			_predsets.set(cls, new HashSet<Integer>(other._predsets.get(cls)));
			_termsets.set(cls, new HashSet<Integer>(other._termsets.get(cls)));
		}
		int n = other._similarities.length;
		_similarities = new double[n][n];
		for (int x : _classes) {
			for (int y : _classes) {
				_similarities[x][y] = other._similarities[x][y];
			}
		}
	}
	
	public PredsetHeuristic(RDFGraph G) {
		_G = G;
		Map<Set<Integer>,Integer> predset2cls = new HashMap<Set<Integer>, Integer>();
		for (int id : G) {
			if (TermType.is_literal(id)) continue; // skip all literals
			Set<Integer> predset = G.get_term(id).get_property_set();
			Integer cls = predset2cls.get(predset);
			Set<Integer> termset;
			if (cls==null) {
				cls = _termsets.size();
				_classes.add(cls);
				termset = new HashSet<Integer>();
				_termsets.add(termset);
				_predsets.add(predset);
				predset2cls.put(predset, cls);
			} else {
				termset = _termsets.get(cls);
			}
			termset.add(id);
		}
		int ncls = _classes.size();
		_similarities = new double[ncls][ncls];
		System.out.println("There are "+ncls+" classes");
		for (int x : _classes) {
			for (int y : _classes) {
				_similarities[x][y] = similarity(_predsets.get(x), _predsets.get(y)); 
			}
		}
	}
	
	private double similarity(Set<Integer> s1, Set<Integer> s2) {
		int nshared = 0;
		for (int i : s1) if (s2.contains(i)) nshared++;
		int ndistinct = s1.size() + s2.size() - nshared;
		return (double)nshared / ndistinct;
	}
	
	private void join(int cls1, int cls2) {
		assert cls1!=cls2 : "Join requires distinct classes";
		assert _classes.contains(cls1) && _classes.contains(cls2) : "Class does not exist";
		Set<Integer> ps = _predsets.get(cls1);
		ps.retainAll(_predsets.get(cls2));
		_termsets.get(cls1).addAll(_termsets.get(cls2));
		_classes.remove(cls2);
		_predsets.set(cls2,  null);
		_termsets.set(cls2,  null);
		for (int cls : _classes) {
			_similarities[cls1][cls] = _similarities[cls][cls1] = similarity(ps, _predsets.get(cls));
		}
	}
	
	public Partition get_partition() {
		Partition p = new Partition();
		for (int cls : _classes) {
			int pcls = p.new_class();
			for (int id : _termsets.get(cls)) {
				p.add(pcls, id);
			}
		}
		if (_G._nlits>0) {
			int litcls = p.new_class();
			for (int ix=0; ix<_G._nlits; ix++) {
				p.add(litcls, TermType.lit2id(ix));
			}
		}
		return p;
	}
	
	private double codelength() {
		return RDFhelper.encode("test", _G, get_partition()).getResults().L();
	}
	
	public static Partition search(PredsetHeuristic ph) {
		double Lph = ph.codelength();
		boolean try_again = true;
		
		while (try_again) {
			try_again = false;
			List<Pair<Integer,Integer>> candidates = ph.get_candidates();
			for (int i=0; i<100 && i<candidates.size(); i++) {
				Pair<Integer,Integer> p = candidates.get(i);
				PredsetHeuristic ph_test = new PredsetHeuristic(ph);
				ph_test.join(p.getLeft(), p.getRight());
				double Lph_test = ph_test.codelength();
				if (Lph_test < Lph) {
					System.err.println("Joined "+p.getLeft()+" and "+p.getRight()+", cl from "+Lph+" to "+Lph_test);
					ph = ph_test;
					Lph = Lph_test;
					try_again = true;
					break;
				}
			}
		}
		return ph.get_partition();
	}
	
	public List<Pair<Integer,Integer>> get_candidates() {
		List<Pair<Integer,Integer>> candidates = new ArrayList<Pair<Integer,Integer>>();
		for (int cls1 : _classes) {
			for (int cls2: _classes) {
				if (cls1 != cls2) candidates.add(Pair.of(cls1,  cls2));
			}
		}
		Collections.sort(candidates, new ByDifference());
		return candidates;
	}
	
	private class ByDifference implements Comparator<Pair<Integer,Integer>> {

		@Override public int compare(Pair<Integer, Integer> p1,	Pair<Integer, Integer> p2) {
			double s1 = _similarities[p1.getLeft()][p1.getRight()];
			double s2 = _similarities[p2.getLeft()][p2.getRight()];
			double d = s1 - s2;
			return d<0 ? 1 : d>0 ? -1 : 0;
		}
		
	}
	
}
