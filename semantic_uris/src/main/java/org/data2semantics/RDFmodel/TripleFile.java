package org.data2semantics.RDFmodel;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;

public class TripleFile implements Iterable<List<Integer>> {

	private String _fn;

	public TripleFile(String fn) {
		_fn = fn;
	}

	public static class MyIterator implements Iterator<List<Integer>> {
		private BufferedReader _in;
		private String _line;

		public MyIterator(String fn) {
			try {
				_in = new BufferedReader(new FileReader(fn));
				cache_line();
			} catch (IOException e) {
				throw new RuntimeException("Cannot read triple file '" + fn + "'", e);
			}
		}

		private void cache_line() {
			try {
				_line = _in.readLine();
				if (_line == null) {
					_in.close();
				}
			} catch (IOException e) {
				throw new RuntimeException("Cannot read line in triple file", e);
			}
		}

		@Override public boolean hasNext() {
			return _line != null;
		}

		@Override public List<Integer> next() {
			String line = _line;
			cache_line();
			String[] parts = line.split("\\s+");
			assert parts.length == 3 : "Not a triple!";
			int subj_id = Integer.parseInt(parts[0]);
			int pred_id = Integer.parseInt(parts[1]);
			int obj_id = Integer.parseInt(parts[2]);
			return Arrays.asList(subj_id, pred_id, obj_id);
		}

		@Override public void remove() {
			throw new UnsupportedOperationException("Cannot remove triples");
		}
	}

	@Override public Iterator<List<Integer>> iterator() {
		return new MyIterator(_fn);
	}

}

// this version of the class pre-loads all triples, then permutes the whole
// bunch randomly
// and provides iteration access to that
class PermutedTripleFile implements Iterable<List<Integer>> {

	private List<List<Integer>> _triples = new ArrayList<List<Integer>>();
	private int _nnamed, _nbnodes, _nlits;

	private int[] _named_permutation;
	private int[] _bnode_permutation;
	private int[] _literal_permutation;

	private void add_term(int id) {
		int type = TermType.id2type(id);
		int ix = TermType.id2ix(id);
		switch (type) {
		case TermType.NAMED:
			if (ix >= _nnamed)
				_nnamed = ix + 1;
			break;
		case TermType.BNODE:
			if (ix >= _nbnodes)
				_nbnodes = ix + 1;
			break;
		case TermType.LITERAL:
			if (ix >= _nlits)
				_nlits = ix + 1;
			break;
		}
	}

	private static int[] create_permutation(int n) {
		int[] a = new int[n];
		a[0] = 0;
		for (int i = 0; i < n; i++) {
			int k;
			k = (int) (Math.random() * (i + 1));
			if (k < i)
				a[i] = a[k];
			a[k] = i;
		}
		return a;
	}

	public PermutedTripleFile(String fn) {

		_nnamed = _nlits = _nbnodes = 0;

		System.out.println("Reading...");

		for (List<Integer> triple : new TripleFile(fn)) {
			_triples.add(triple);
			add_term(triple.get(0));
			add_term(triple.get(1));
			add_term(triple.get(2));
		}

		System.out.println("Creating permutations...");

		_named_permutation = create_permutation(_nnamed);
		_bnode_permutation = create_permutation(_nbnodes);
		_literal_permutation = create_permutation(_nlits);

		System.out.println("PermutedTripleFile constructed!");
	}

	@Override public Iterator<List<Integer>> iterator() {
		return new PermutedIterator();
	}

	class PermutedIterator implements Iterator<List<Integer>> {

		private int map(int id) {
			int type = TermType.id2type(id);
			int ix = TermType.id2ix(id);
			int[] perm = type == TermType.NAMED ? _named_permutation
			    : type == TermType.BNODE ? _bnode_permutation : _literal_permutation;
			return TermType.ix2id(type, perm[ix]);
		}

		@Override public boolean hasNext() {
			return !_triples.isEmpty();
		}

		@Override public List<Integer> next() {
			List<Integer> triple = _triples.remove(_triples.size() - 1);
			/*
			 * int k = (int)(Math.random()*_triples.size()); List<Integer> triple =
			 * _triples.get(k); List<Integer> last =
			 * _triples.remove(_triples.size()-1); if (last!=triple) _triples.set(k,
			 * last);
			 */
			List<Integer> mapped_triple = new ArrayList<Integer>();
			mapped_triple.add(map(triple.get(0)));
			mapped_triple.add(map(triple.get(1)));
			mapped_triple.add(map(triple.get(2)));
			return mapped_triple;
		}

		@Override public void remove() {
			throw new UnsupportedOperationException("Cannot remove triples");
		}
	}

}
