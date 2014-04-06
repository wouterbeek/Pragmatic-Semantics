package org.data2semantics.RDFmodel;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class LinkPrediction {

	private static Map<Integer, Integer> get_predicate_map(String input_dir,
	    int pred_id) {
		Iterable<List<Integer>> triples = new TripleFile(input_dir + "/triples.dat");
		Map<Integer, Integer> map = new HashMap<Integer, Integer>();
		for (List<Integer> triple : triples) {
			if (triple.get(1).equals(pred_id))
				map.put(triple.get(0), triple.get(2));
		}
		return map;
	}

	private static double compress_with_replacements(String input_dir,
	    Partition partition, Map<List<Integer>, List<Integer>> replacements) {
		RDFGraph G = new RDFGraph(new ReplacingTripleFile(input_dir
		    + "/triples.dat", replacements));
		CoderContext C = RDFhelper.encode("testing", G, partition);
		return C.getResults().L();
	}

	private static Partition get_partition(String input_dir) {
		RDFGraph G = new RDFGraph(new TripleFile(input_dir + "/triples.dat"));
		return Partition.greedy_search(G, null);
	}

	public static void main(String[] args) {
		if (args.length != 1) {
			System.err.println("usage: java LinkPrediction input_dir ");
			System.exit(1);
		}

		String input_dir = args[0];

		int[] pred_ids = RDFhelper.get_uri_ids(input_dir, "#affiliation",
		    "#employs");
		int affiliation_id = pred_ids[0];
		int employs_id = pred_ids[1];

		Map<Integer, Integer> people2employers = get_predicate_map(input_dir,
		    affiliation_id);
		Set<Integer> employer_ids = new HashSet<Integer>(people2employers.values());
		System.err.println("Number of people:    " + people2employers.size());
		System.err.println("Number of employers: " + employer_ids.size());

		Partition partition = get_partition(input_dir);

		int correct = 0, total = 0; // count number of correctly classified people
		for (Map.Entry<Integer, Integer> p2e : people2employers.entrySet()) {
			// predict affiliation for this person
			int person_id = p2e.getKey();
			int true_employer_id = p2e.getValue();
			int best_employer_id = -1;
			double best_L = -1;
			for (int try_employer_id : employer_ids) {
				Map<List<Integer>, List<Integer>> replacements = new HashMap<List<Integer>, List<Integer>>();
				replacements.put(
				    Arrays.asList(true_employer_id, employs_id, person_id),
				    Arrays.asList(try_employer_id, employs_id, person_id));
				replacements.put(
				    Arrays.asList(person_id, affiliation_id, true_employer_id),
				    Arrays.asList(person_id, affiliation_id, try_employer_id));
				double L = compress_with_replacements(input_dir, partition,
				    replacements);
				if (best_L == -1 || L < best_L) {
					best_L = L;
					best_employer_id = try_employer_id;
				}
			}
			System.err.println("best_id = " + best_employer_id + ", best_L = "
			    + best_L);
			if (true_employer_id == best_employer_id)
				correct++;
			total++;
			System.out.printf("Correctly classified %d out of %d people, %.2f%%\n",
			    correct, total, 100.0 * correct / total);
		}

		System.out.println("Correctly classified " + correct + " out of "
		    + people2employers.size() + " people.");
	}

	public static class ReplacingTripleFile implements Iterable<List<Integer>> {

		private String _fn;
		private Map<List<Integer>, List<Integer>> _replacements;

		public ReplacingTripleFile(String fn,
		    Map<List<Integer>, List<Integer>> replacements) {
			_fn = fn;
			_replacements = replacements;
		}

		@Override public Iterator<List<Integer>> iterator() {
			return new ReplacingIterator(_fn, _replacements);
		}

		private static class ReplacingIterator extends TripleFile.MyIterator {

			private Map<List<Integer>, List<Integer>> _replacements;

			public ReplacingIterator(String fn,
			    Map<List<Integer>, List<Integer>> replacements) {
				super(fn);
				_replacements = replacements;
			}

			@Override public List<Integer> next() {
				List<Integer> triple = super.next();
				if (triple != null) {
					List<Integer> mapped = _replacements.get(triple);
					if (mapped != null)
						triple = mapped;
				}
				return triple;
			}
		}

	}

}
