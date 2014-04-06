package org.data2semantics.RDFmodel;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Stack;

import org.data2semantics.RDFmodel.Fingerprint.FingerprintCoderFactory;

public class CoderContext {
	private CLAccountant _acc;
	private Partition _partition;
	private Stack<CodeComponent<?>> _components = new Stack<CodeComponent<?>>();

	// --------------------------------- Top-level coders
	// ---------------------------------------------

	public final CodeComponent<Integer> _c_urinode = new CodeComponent<Integer>(null, null);
	public final CodeComponent<Integer> _c_subjtype = new CodeComponent<Integer>(null, null);
	
	public final CodeComponent<Integer> _c_boundary = new CodeComponent<Integer>("Boundary", KT.get_coder_factory(2));
	
	// GraphCoderSigBased
	// The graph coder has no internal state of its own, so it does not need to
	// be conditioned.
	public final CodeComponent<RDFGraph> _c_graph;

	// Term
	// A term has no internal state of its own, so it does not need to be
	// conditioned.
	public final CodeComponent<Term> _c_term = new CodeComponent<Term>("Term",
	    Term.getFactory());

	public final CodeComponent<Partition> _c_partition;
	public final CodeComponent<Integer> _c_subj_class;

	// --------------------------- Encoding non-signature objects
	// -------------------------------------

	// More objs with the same linktype?
	// Can usefully condition on: URINode, LinkSet, Predicate, ObjType
	public final CodeComponent<Integer> _c_moreobjs;
	public final List<CodeComponent<Integer>> _c_objclassindex;

	// -------------------------------- Encoding signatures
	// -------------------------------------------

	// Signature
	// Can only usefully condition on URINode
	public final FingerprintCoderFactory _f_fingerprint = Fingerprint.getFactory();
	public final CodeComponent<Fingerprint> _c_fingerprint;

	// Link
	// Can only usefully condition on URINode
	public final CodeComponent<Link> _c_link;

	// Predicate
	// Can only usefully condition on URINode
	public final CodeComponent<Integer> _c_pred;

	// Object type
	// Can usefully condition on: UriNode, Predicate
	public final CodeComponent<Integer> _c_objclass;

	// More links? (in signature)
	// Can usefully condition on: URINode.
	// Can NOT condition on the link:
	// - a linkset may be empty in which case there are no links
	// - since linkset ordering is undefined, so after which link the set is
	// done is arbitrary
	public final CodeComponent<Integer> _c_morelinks;

	// ================================================================================================

	public CoderContext(String name, Partition partition, int nnamed, int nbnodes, int nlits) {
		_acc = new CLAccountant(name);
		_partition = partition;

		// Coder<Integer> dummy_coder = new Coder<Integer>() {
		// 	 @Override public void encode(CoderContext C, Integer obj) {}
		// };
		
		_c_partition = new CodeComponent<Partition>("Partition", Partition.get_coder_factory(nnamed, nbnodes, nlits));
		_c_subj_class = new CodeComponent<Integer>("Partition", KT.get_coder_factory(partition.num_classes()), _c_subjtype);
		// _c_partition = new CodeComponent<Integer>("Partition", ObjPYCoder.getFactory(dummy_coder));
		
		_c_graph = new CodeComponent<RDFGraph>("Graph", GraphCoderSigBased.getFactory());
		_c_link = new CodeComponent<Link>("Link", Link.get_coder_factory(), _c_subj_class);
		_c_pred = new CodeComponent<Integer>("Predicates", ObjPYCoder.get_coder_factory(new UniqueUniformCoder(nnamed)));
		_c_objclass = new CodeComponent<Integer>("ObjClass", KT.get_coder_factory(partition.num_classes()));
		_c_objclassindex = new ArrayList<CodeComponent<Integer>>();
		for (int i = 0; i < partition.num_classes(); i++) {
			CoderFactory<Integer> fact = KT.get_coder_factory(partition.class_size(i));
			_c_objclassindex.add(new CodeComponent<Integer>("ObjClassIx", fact, _c_pred));
		}
		_c_moreobjs = new CodeComponent<Integer>("MoreObjs", KT.get_coder_factory(2), _c_link);
		_c_morelinks = new CodeComponent<Integer>("MoreLinks", KT.get_coder_factory(2), _c_subj_class);
		_c_fingerprint = new CodeComponent<Fingerprint>("Signature", _f_fingerprint, _c_subj_class);
	}

	public CLAccountant getResults() {
		return _acc;
	}

	public void use_bits(double L) {
		_acc.add(top_component().get_name(), L);
	}

	public void use_bits(String name, double L) {
		_acc.add(name, L);
	}

	public void spawned_new() {
		_acc.spawned_new(top_component().get_name());
	}

	public Partition get_partition() {
		return _partition;
	}

	public void push_component(CodeComponent<?> component) {
		_components.push(component);
	}

	public void pop_component() {
		_components.pop();
	}

	public CodeComponent<?> top_component() {
		return _components.peek();
	}

	static class CodeComponent<T> implements Coder<T> {
		private String _name;
		private CoderFactory<T> _fact;
		private CodeComponent<?>[] _condition_on;
		private Map<List<Object>, Coder<T>> _coders = new HashMap<List<Object>, Coder<T>>();
		private T _last = null;

		public CodeComponent(String name, CoderFactory<T> fact,
		    CodeComponent<?>... condition_on) {
			_name = name;
			_fact = fact;
			_condition_on = condition_on;
		}

		public String get_name() {
			return _name;
		}

		public void set_conditional(T last) {
			_last = last;
		}

		@Override public void encode(CoderContext C, T obj) {
			C.push_component(this);
			List<Object> key = new ArrayList<Object>();
			for (CodeComponent<?> cm : _condition_on)
				key.add(cm._last);

			Coder<T> c = _coders.get(key);
			if (c == null) {
				c = _fact.build();
				_coders.put(key, c);
				C.spawned_new();
			}
			c.encode(C, obj);
			_last = obj;
			C.pop_component();
		}
	}
}
