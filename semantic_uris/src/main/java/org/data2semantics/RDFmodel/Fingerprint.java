package org.data2semantics.RDFmodel;

import java.util.ArrayList;
import java.util.BitSet;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.SortedSet;

import org.data2semantics.RDFmodel.Partition.ObjInfo;

public class Fingerprint extends HashSet<Link> {
	private static final long serialVersionUID = 1L;

	public Fingerprint(Partition partition, Term term) {
		assert term != null : "term may be empty but not null";
		for (Map.Entry<Integer, SortedSet<Integer>> pred : term.entrySet()) {
			int pred_id = pred.getKey();
			for (int obj_id : pred.getValue()) {
				ObjInfo objinfo = partition.look_up(obj_id);
				add(new Link(pred_id, objinfo.get_class()));
			}
		}
	}

	public static class FingerprintCoderFactory implements CoderFactory<Fingerprint> {
		private IndexMap<Link> _link_map = new IndexMap<Link>();
		private List<SignatureCoder> _coders = new ArrayList<SignatureCoder>();

		/*
		 * Implements a bijection between BitSets and Linksets. While we could use
		 * the Linkset itself as a key in the _map hash below, BitSets are more
		 * efficient.
		 */
		private BitSet getKey(Fingerprint bs) {
			BitSet key = new BitSet();
			for (Link l : bs)
				key.set(_link_map.map(l));
			return key;
		}

		public int get_nlinks() {
			return _link_map.size();
		}

		public List<SignatureCoder> get_coders() {
			return _coders;
		}

		@Override public Coder<Fingerprint> build() {
			SignatureCoder c = new SignatureCoder();
			_coders.add(c);
			return c;
		}

		public class SignatureCoder implements Coder<Fingerprint> {
			private BasicFingerprintCoder _lsc = new BasicFingerprintCoder();
			private IndexMap<BitSet> _map = new IndexMap<BitSet>();
			private PitmanYorCoder _ref = new PitmanYorCoder();

			@Override public void encode(CoderContext C, Fingerprint obj) {
				int ix = _map.map(getKey(obj));
				if (_ref.encode_test_new(C, ix))
					_lsc.encode(C, obj);
			}

			public int get_nlinksets() {
				return _map.size();
			}
		}

		private static class BasicFingerprintCoder implements Coder<Fingerprint> {
			@Override public void encode(CoderContext C, Fingerprint linkset) {
				for (Link lnk : linkset) {
					C._c_morelinks.encode(C, 1); // at least one more link
					C._c_link.encode(C, lnk);
				}
				C._c_morelinks.encode(C, 0); // no more links after the last one

				/*
				 * We redundantly encoded a set as a sequence; by combining all code
				 * words that represent the same set using a permutation of the sequence
				 * we can reclaim some bits.
				 */
				C.use_bits("Signature bonus", -Codes.lgfac(linkset.size()));
			}
		}

	}

	public static FingerprintCoderFactory getFactory() {
		return new FingerprintCoderFactory();
	}
}
