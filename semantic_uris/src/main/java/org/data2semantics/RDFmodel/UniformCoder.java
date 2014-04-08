package org.data2semantics.RDFmodel;

public class UniformCoder implements Coder<Integer> {
	private int _n;
	
	public UniformCoder(int n) { _n = n; }
	
	@Override public void encode(CoderContext C, Integer obj) {
		C.use_bits(Codes.uniform(_n));		
	}
}

// Each symbol in the initial alphabet can be encoded only ones.
// This coder is uniform over all remaining symbols.
class UniqueUniformCoder implements Coder<Integer> {
	
	private int _unseen;
	
	public UniqueUniformCoder(int unseen) { _unseen = unseen; }

	@Override public void encode(CoderContext C, Integer obj) {
		assert _unseen >= 1 : "No more symbols can be encoded";
		C.use_bits(Codes.uniform(_unseen));		
		_unseen--;
	}
}