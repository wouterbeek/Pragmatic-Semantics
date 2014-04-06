package org.data2semantics.RDFmodel;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStream;
import java.io.PipedInputStream;
import java.io.PipedOutputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.List;
import java.util.Map.Entry;
import java.util.zip.GZIPOutputStream;

public class RDFhelper {

	// given a sequence of uris, returns their ids, or -1 if they are not found
	public static int[] get_uri_ids(String input_dir, String... which_uris) {

		List<String> uris = null;
		try {
			uris = RDFhelper.load_strings(input_dir + "/uris.txt");
		} catch (IOException e) {
			throw new RuntimeException("Can't load URIs", e);
		}

		final int n = which_uris.length;

		int[] ids = new int[n];
		for (int j = 0; j < n; j++)
			ids[j] = -1;

		for (int i = 0; i < uris.size(); i++) {
			String s = uris.get(i);
			int id = TermType.named2id(i);
			for (int j = 0; j < n; j++) {
				if (s.equals(which_uris[j]))
					ids[j] = id;
			}
		}

		return ids;
	}

	public static CoderContext encode(String name, RDFGraph G, Partition partition) {
		CoderContext C = new CoderContext(name, partition, G._nnamed, G._nbnodes, G._nlits);
		C._c_graph.encode(C, G);
		return C;
	}

	public static List<String> load_strings(String fn) throws IOException {
		BufferedReader in = new BufferedReader(new FileReader(fn));
		String s;
		List<String> res = new ArrayList<String>();
		while ((s = in.readLine()) != null)
			res.add(s);
		in.close();
		return res;
	}

	public static String set2string(Collection<?> items) {
		StringBuilder sb = new StringBuilder();
		for (Object o : items) {
			sb.append(o.toString());
			sb.append('\n');
		}
		return sb.toString();
	}

	public static int gzip(String data) {
		int L = 0;
		try {
			PipedInputStream pis = new PipedInputStream();
			new WriterThread(data, new GZIPOutputStream(new PipedOutputStream(pis)))
			    .start();
			final int buflen = 16384;
			byte[] buf = new byte[buflen];
			while (true) {
				int nread = pis.read(buf, 0, buflen);
				if (nread == -1)
					break;
				L += nread;
			}
			pis.close();
		} catch (IOException e) {
			System.err.println("gzip failed: " + e);
			System.exit(1);
		}
		return L * 8; // size in bits
	}

	public static class WriterThread extends Thread {

		private byte[] _data;
		private OutputStream _os;

		public WriterThread(String data, OutputStream os) {
			_data = data.getBytes();
			_os = os;
		}

		@Override public void run() {
			try {
				_os.write(_data);
				_os.close();
			} catch (IOException e) {
				System.err.println("Error writing to pipe: " + e);
				System.exit(1);
			}
		}
	}
}

class ByValue<T> implements Comparator<Entry<T, Integer>> {
	@Override public int compare(Entry<T, Integer> e1, Entry<T, Integer> e2) {
		return e1.getValue() - e2.getValue();
	}
}

