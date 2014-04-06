package org.data2semantics.RDFmodel;



public class GraphCoderSigBased implements Coder<RDFGraph> {

	@Override public void encode(CoderContext C, RDFGraph G) {

		Partition p = C.get_partition();
		
		C._c_partition.encode(C, p);
		C._c_subjtype.set_conditional(TermType.NAMED);
		for (int named_ix = 0; named_ix < G._nnamed; named_ix++) {
			C._c_subj_class.set_conditional(p.look_up(TermType.named2id(named_ix)).get_class());
			C._c_term.encode(C, G._n_subj2term.get(named_ix));
		}
		
		C._c_subjtype.set_conditional(TermType.BNODE);
		for (int bnode_ix = 0; bnode_ix < G._nbnodes; bnode_ix++) {
			C._c_subj_class.set_conditional(p.look_up(TermType.bnode2id(bnode_ix)).get_class());
			C._c_term.encode(C, G._b_subj2term.get(bnode_ix));
		}

		C.use_bits("Bnode order bonus", -Codes.lgfac(G._b_subj2term.size()));
		
	}

	public static CoderFactory<RDFGraph> getFactory() {
		return new CoderFactory<RDFGraph>() {
			@Override public Coder<RDFGraph> build() {
				return new GraphCoderSigBased();
			}
		};
	}

}
