package org.data2semantics.RDFmodel;

import java.util.HashSet;
import java.util.Map.Entry;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeMap;

import org.data2semantics.RDFmodel.Partition.ObjInfo;

// A TermCoder encodes all triples with this term in the subject position.

public class Term extends TreeMap<Integer, SortedSet<Integer>> {
	private static final long serialVersionUID = -6221740719095752767L;

	public static CoderFactory<Term> getFactory() {
		return new CoderFactory<Term>() {
			@Override public Coder<Term> build() {
				return new TermCoder();
			}
		};
	}
	
	public Fingerprint get_fingerprint(Partition partition) { 
		return new Fingerprint(partition, this); 
	} 
	
	public Set<Integer> get_property_set() {
		Set<Integer> prop = new HashSet<Integer>();
		for (int prop_id : this.keySet()) prop.add(prop_id);
		return prop;
	}
	
	public SortedSet<Integer> get_obj_set(int pred_id) {
		return get(pred_id);
	}
	
	private static class TermCoder implements Coder<Term> {

		@Override public void encode(CoderContext C, Term term) {
			// encode the signature
			C._c_fingerprint.encode(C, term.get_fingerprint(C.get_partition()));

			// encode objects outside of the signature
			Link lnk = null;
			int n_same_linktype = 0;
			for (Entry<Integer, SortedSet<Integer>> map : term.entrySet()) {
				int pred_id = map.getKey();
				for (int obj_id : map.getValue()) {
					ObjInfo objinfo = C.get_partition().look_up(obj_id);
					Link next_lnk = new Link(pred_id, objinfo.get_class());
					if (next_lnk.equals(lnk)) {
						C._c_moreobjs.encode(C, 1);
						n_same_linktype++;
					} else {
						if (lnk != null) {
							C._c_moreobjs.encode(C, 0);
							C.use_bits("ABox bonus", -Codes.lgfac(n_same_linktype));
						}
						n_same_linktype = 1;
					}
					lnk = next_lnk;

					/*
					 * Fix conditioning information for this object. This is an ugly hack,
					 * necessary because of out-of-order coding. It is done here, not
					 * higher up in the code, because the /next/ C._c_more_objs.encode
					 * (called above) should use /this/ link to condition on.
					 */
					C._c_link.set_conditional(lnk);
					C._c_pred.set_conditional(pred_id);
					C._c_objclass.set_conditional(objinfo.get_class());

					if (TermType.is_literal(obj_id)) {
						// Special case for literals: they are always listed in order in the
						// first
						// part.
						C._c_objclassindex.get(objinfo.get_class()).encode(C,
						    objinfo.get_ix());
					}
				}
			}
			if (lnk != null) {
				C._c_moreobjs.encode(C, 0);
				C.use_bits("ABox bonus", -Codes.lgfac(n_same_linktype));
			}
		}
	}
}