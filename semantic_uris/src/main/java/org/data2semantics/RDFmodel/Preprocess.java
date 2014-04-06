package org.data2semantics.RDFmodel;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.net.URL;
import java.util.ArrayList;
import java.util.List;
import java.util.Map.Entry;

import org.openrdf.model.BNode;
import org.openrdf.model.Literal;
import org.openrdf.model.Statement;
import org.openrdf.model.URI;
import org.openrdf.model.Value;
import org.openrdf.rio.ParserConfig;
import org.openrdf.rio.RDFFormat;
import org.openrdf.rio.RDFHandlerException;
import org.openrdf.rio.RDFParseException;
import org.openrdf.rio.RDFParser;
import org.openrdf.rio.Rio;
import org.openrdf.rio.helpers.BasicParserSettings;
import org.openrdf.rio.helpers.RDFHandlerBase;

public class Preprocess {

	public static void load(String input_dir) {
		load(input_dir, input_dir);
	}

	public static void load(String input_dir, String output_dir) {
		PrepLoader L;
		try {
			L = new PrepLoader(input_dir, output_dir + "/triples.dat");
			System.out.println("Saving uris");
			L.save_uris(output_dir + "/uris.txt");
			System.out.println("Saving literals");
			L.save_lits(output_dir + "/lits.txt");
			System.out.println("Done");
		} catch (FileNotFoundException e) {
			System.err.println("Could not load input: " + e);
			System.exit(1);
		}

	}

	private static class PrepLoader extends RDFHandlerBase {

		private IndexMap<URI> _uri_map = new IndexMap<URI>();
		private IndexMap<BNode> _bnode_map = new IndexMap<BNode>();
		private List<Literal> _literals = new ArrayList<Literal>();

		private PrintWriter _triples_out;

		public PrepLoader(String fn_in, String fn_triples_out)
		    throws FileNotFoundException {
			File f = new File(fn_in);
			_triples_out = new PrintWriter(fn_triples_out);
			try {
				if (f.isDirectory()) {
					System.out.println("Loading directory");
					for (File fc : f.listFiles()) {
						System.out.println("Reading '" + fc + "'...");
						load(fc.getAbsolutePath());
					}
				} else {
					System.out.println("Reading '" + fn_in + "'...");
					load(fn_in);
				}
			} catch (Exception e) {
				System.err.println("Cannot load '" + fn_in + "': " + e);
				System.exit(1);
			}
			_triples_out.close();
		}

		public void load(String filename) throws RDFParseException,
		    RDFHandlerException, FileNotFoundException, IOException {
			RDFFormat format = RDFFormat.forFileName(filename);
			RDFParser parser = Rio.createParser(format);
			parser.setRDFHandler(this);
			ParserConfig cfg = new ParserConfig();
			cfg.set(BasicParserSettings.VERIFY_DATATYPE_VALUES, false);
			cfg.set(BasicParserSettings.FAIL_ON_UNKNOWN_DATATYPES, false);
			cfg.set(BasicParserSettings.VERIFY_LANGUAGE_TAGS, false);
			parser.setParserConfig(cfg);
			URL url = new URL("file:/" + filename);
			parser.parse(new FileReader(filename), url.toString());
		}

		int add(Value item) {
			if (item instanceof BNode)
				return TermType.ix2id(TermType.BNODE, _bnode_map.map((BNode) item));
			if (item instanceof URI)
				return TermType.ix2id(TermType.NAMED, _uri_map.map((URI) item));
			assert item instanceof Literal : "Unrecognised value type";
			_literals.add((Literal) item);
			return TermType.ix2id(TermType.LITERAL, _literals.size() - 1);
		}

		@Override public void handleStatement(Statement smt) {
			// TODO do something with the context
			int subj_id = add(smt.getSubject());
			int pred_id = add(smt.getPredicate());
			int obj_id = add(smt.getObject());
			_triples_out.printf("%d %d %d\n", subj_id, pred_id, obj_id);
		}

		@Override public void endRDF() {
		}

		public void save_uris(String fn) throws FileNotFoundException {
			URI[] uris = new URI[_uri_map.size()];
			for (Entry<URI, Integer> entry : _uri_map.entrySet()) {
				uris[entry.getValue()] = entry.getKey();
			}
			PrintWriter out = new PrintWriter(fn);
			for (URI uri : uris)
				out.println(uri.stringValue());
			out.close();
		}

		public void save_lits(String fn) throws FileNotFoundException {
			PrintWriter out = new PrintWriter(fn);
			for (Literal lit : _literals) {
				out.println(lit.stringValue());
			}
			out.close();
		}

	}

}
