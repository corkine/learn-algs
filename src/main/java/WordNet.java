import edu.princeton.cs.algs4.Bag;
import edu.princeton.cs.algs4.Digraph;
import edu.princeton.cs.algs4.DirectedCycle;
import edu.princeton.cs.algs4.In;
import edu.princeton.cs.algs4.ST;

import java.util.ArrayList;

public class WordNet {
    private final ST<String, Bag<Integer>> st;
    private final ArrayList<String> idWords;
    private final Digraph G;

    // constructor takes the name of the two input files
    public WordNet(String synsets, String hypernyms) {
        if(synsets == null || hypernyms == null) throw new IllegalArgumentException();

        st = new ST<>();
        idWords = new ArrayList<>();

        int count = 0;
        In synsetsIn = new In(synsets);
        while(synsetsIn.hasNextLine()) {
            String[] lines = synsetsIn.readLine().split(",");
            int wordsId = Integer.parseInt(lines[0]);
            String words = lines[1];
            String[] wordList = words.split(" ");

            for (String s : wordList) {
                if (st.contains(s)) st.get(s).add(wordsId);
                else { Bag<Integer> b = new Bag<>(); b.add(wordsId); st.put(s, b); }
            }
            count += 1;
            idWords.add(words);
        }

        G = new Digraph(count);
        In hypernymsIn = new In(hypernyms);
        boolean[] isNotRoot = new boolean[count];
        int rootNumber = 0;

        while(hypernymsIn.hasNextLine()) {
            String[] a = hypernymsIn.readLine().split(",");
            int id = Integer.parseInt(a[0]);
            isNotRoot[id] = true;
            int relatedId = -1;
            for(int i = 1; i < a.length; i++)
                relatedId = Integer.parseInt(a[i]);
                G.addEdge(id, relatedId);
        }

        for(int i = 0; i < count; i++) {
            if(!isNotRoot[i]) rootNumber += 1;
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
        return idWords.get(root);
    }

    // do unit testing of this class
    public static void main(String[] args) {


    }
}