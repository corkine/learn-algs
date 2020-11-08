import edu.princeton.cs.algs4.Bag;
import edu.princeton.cs.algs4.Digraph;
import edu.princeton.cs.algs4.DirectedCycle;
import edu.princeton.cs.algs4.In;
import edu.princeton.cs.algs4.ST;

import java.util.ArrayList;

public class WordNet {
    private final ST<String, Bag<Integer>> st;
    private final ArrayList<String> idList;
    private final Digraph G;

    // constructor takes the name of the two input files
    public WordNet(String synsets, String hypernyms) {
        if(synsets == null || hypernyms == null) throw new IllegalArgumentException();

        st = new ST<String, Bag<Integer>>();
        idList = new ArrayList<String>();

        int count = 0;
        In in1 = new In(synsets);
        while(in1.hasNextLine()) {
            String[] a = in1.readLine().split(",");
            String[] a2 = a[1].split(" ");

            for(int i = 0; i < a2.length; i++) {
                if(st.contains(a2[i])) st.get(a2[i]).add(Integer.parseInt(a[0]));
                else {
                    Bag<Integer> b = new Bag<Integer>();
                    b.add(Integer.parseInt(a[0]));
                    st.put(a2[i], b);
                }
            }
            count++;
            idList.add(a[1]);
        }

        G = new Digraph(count);
        In in2 = new In(hypernyms);
        boolean[] isNotRoot = new boolean[count];
        int rootNumber = 0;

        while(in2.hasNextLine()) {
            String[] a = in2.readLine().split(",");
            isNotRoot[Integer.parseInt(a[0])] = true;
            for(int i = 1; i < a.length; i++)
                G.addEdge(Integer.parseInt(a[0]), Integer.parseInt(a[i]));
        }

        for(int i = 0; i < count; i++) {
            if(!isNotRoot[i]) rootNumber++;
        }
        DirectedCycle d = new DirectedCycle(G);
        if(rootNumber > 1 || d.hasCycle()) throw new IllegalArgumentException();
    }

    // returns all WordNet nouns
    public Iterable<String> nouns() {
        return st.keys();
    }

    // is the word a WordNet noun?
    public boolean isNoun(String word) {
        if(word == null) throw new IllegalArgumentException();
        return st.contains(word);
    }

    // distance between nounA and nounB (defined below)
    public int distance(String nounA, String nounB) {
        if(nounA == null || nounB == null || !isNoun(nounA) || !isNoun(nounB))
            throw new IllegalArgumentException();
        SAP s = new SAP(G);
        Bag<Integer> ida = st.get(nounA);
        Bag<Integer> idb = st.get(nounB);

        return s.length(ida, idb);
    }

    // a synset (second field of synsets.txt) that is the common ancestor of nounA and nounB
    // in a shortest ancestral path (defined below)
    public String sap(String nounA, String nounB) {
        if(nounA == null || nounB == null || !isNoun(nounA) || !isNoun(nounB))
            throw new IllegalArgumentException();
        SAP s = new SAP(G);
        Bag<Integer> ida = st.get(nounA);
        Bag<Integer> idb = st.get(nounB);

        int root = s.ancestor(ida, idb);
        return idList.get(root);
    }

    // do unit testing of this class
    public static void main(String[] args) {


    }
}