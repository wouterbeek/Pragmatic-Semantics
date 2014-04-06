package org.data2semantics.RDFmodel;

public class Link {

	private int _pred_id;
	private int _obj_class;

	public Link(int pred_id, int obj_class) {
		_pred_id = pred_id;
		_obj_class = obj_class;
	}

	public int getPredId() {
		return _pred_id;
	}

	public int getObjClass() {
		return _obj_class;
	}

	@Override public String toString() {
		return "[" + _pred_id + "," + _obj_class + "]";
	}

	@Override public int hashCode() {
		return _pred_id + 31 * _obj_class;
	}

	@Override public boolean equals(Object obj) {
		if (this == obj) return true;
		if (obj == null) return false;
		if (getClass() != obj.getClass()) return false;
		Link o = (Link) obj;
		return o._pred_id == _pred_id && o._obj_class == _obj_class;
	}

	public static CoderFactory<Link> get_coder_factory() {
		return new CoderFactory<Link>() {
			@Override public Coder<Link> build() {
				return new ObjPYCoder<Link>(new BasicLinkCoder());
			}
		};
	}

	private static class BasicLinkCoder implements Coder<Link> {
		@Override public void encode(CoderContext C, Link lnk) {
			C._c_pred.encode(C, lnk._pred_id);
			C._c_objclass.encode(C, lnk._obj_class);
		}
	}
}