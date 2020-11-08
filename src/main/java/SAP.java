import edu.princeton.cs.algs4.BreadthFirstDirectedPaths;
import edu.princeton.cs.algs4.Digraph;

public class SAP {
    private final Digraph G;
    private int anc = -1;
    // constructor takes a digraph (not necessarily a DAG)
    public SAP(Digraph G) {
        if(G == null) throw new IllegalArgumentException();
        this.G = new Digraph(G);
    }

    // length of shortest ancestral path between v and w; -1 if no such path
    public int length(int v, int w) {
        if(v < 0 || v > G.V() - 1 || w < 0 || w > G.V() - 1)
            throw new IllegalArgumentException();
        anc = -1;

        BreadthFirstDirectedPaths bv = new BreadthFirstDirectedPaths(G, v);
        BreadthFirstDirectedPaths bw = new BreadthFirstDirectedPaths(G, w);

        int minLength = Integer.MAX_VALUE;

        for(int i = 0; i < G.V(); i++) {
            if(bv.hasPathTo(i) && bw.hasPathTo(i)) {
                int l = bv.distTo(i) + bw.distTo(i);
                if(l < minLength) {
                    minLength = l;
                    anc = i;
                }
            }
        }

        if(minLength == Integer.MAX_VALUE) return -1;
        else return minLength;

    }

    // a common ancestor of v and w that participates in a shortest ancestral path; -1 if no such path
    public int ancestor(int v, int w) {
        length(v, w);
        return anc;
    }

    // length of shortest ancestral path between any vertex in v and any vertex in w; -1 if no such path
    public int length(Iterable<Integer> v, Iterable<Integer> w) {
        if(v == null || w == null)
            throw new IllegalArgumentException();
        anc = -1;

        for(int i : v) {
            if(i < 0 || i > G.V() - 1)
                throw new IllegalArgumentException();
        }
        for(int i : w) {
            if(i < 0 || i > G.V() - 1)
                throw new IllegalArgumentException();
        }

        BreadthFirstDirectedPaths bv = new BreadthFirstDirectedPaths(G, v);
        BreadthFirstDirectedPaths bw = new BreadthFirstDirectedPaths(G, w);

        int minLength = Integer.MAX_VALUE;

        for(int i = 0; i < G.V(); i++) {
            if(bv.hasPathTo(i) && bw.hasPathTo(i)) {
                int l = bv.distTo(i) + bw.distTo(i);
                if(l < minLength) {
                    minLength = l;
                    anc = i;
                }
            }
        }

        if(minLength == Integer.MAX_VALUE) return -1;
        else return minLength;
    }

    // a common ancestor that participates in shortest ancestral path; -1 if no such path
    public int ancestor(Iterable<Integer> v, Iterable<Integer> w) {
        length(v, w);
        return anc;
    }

    // do unit testing of this class
    public static void main(String[] args) {

    }
}