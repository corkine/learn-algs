import edu.princeton.cs.algs4.In;
import edu.princeton.cs.algs4.Queue;

import java.util.*;

public class WordNet1 {

    private final HashMap<Integer, HashSet<Integer>> graph = new HashMap<>();
    private final HashMap<Integer, Word> dict = new HashMap<>();
    private final HashMap<String, Word> reverseDict = new HashMap<>();

    private static class Word {
        public Integer id;
        public String[] words;
        public String desc;
        public Word(Integer id, String[] word, String desc) {
            this.id = id; this.words = word; this.desc = desc;
        }
        public Word(Integer id, String desc) {
            this.id = id; this.desc = desc;
        }
    }

    private Iterable<Integer> adj(Integer v) {
        return graph.get(v);
    }

    private Integer V() { return graph.size();  }

    private class Path {
        private final int s;
        private final WordNet1 Graph;
        private final boolean[] marked;
        private final int[] cached;
        private final Queue<Integer> queue;

        public Path(int start) {
            this.s = start; this.Graph = WordNet1.this;
            this.marked = new boolean[Graph.V()];
            this.cached = new int[Graph.V()];
            this.queue = new Queue<>();
            bfs();
        }
        private void bfs() {
            queue.enqueue(s);
            marked[s] = true;
            int v = queue.dequeue();
            Graph.adj(v).forEach(w -> {
                if (!marked[w]) {
                    queue.enqueue(w);
                    marked[w] = true;
                    cached[w] = v;
                }
            });
        }
        boolean disConnected(int target) {
            return !marked[target];
        }
        Iterable<Integer> pathTo(int target) {
            if (disConnected(target)) return null;
            Stack<Integer> res = new Stack<>();
            for (int i = target; i != s; i = cached[i]) {
                res.push(i);
            }
            res.push(s);
            return res;
        }
        int pathCountTo(int target) {
            if (disConnected(target)) return -1;
            int count = 0;
            for (int i = target; i != s; i = cached[i]) {
                count += 1;
            }
            return count;
        }
    }

    private class DirectedCycle {
        private final boolean[] marked;
        private final int[] edgeTo;
        private final boolean[] onStack;
        private final Stack<Integer> cycle;
        DirectedCycle() {
            WordNet1 G = WordNet1.this;
            int V = G.V();
            cycle = new Stack<>();
            onStack = new boolean[V];
            edgeTo = new int[V];
            marked = new boolean[V];
            for (int i = 0; i < V; i ++ ){
                if (!marked[i]) dfs(G,i);
            }
        }
        private void dfs(WordNet1 G, int v) {
            onStack[v] = true;
            marked[v] = true;
            for (Integer w : G.adj(v)) {
                if (!cycle.isEmpty()) return;
                else if (!marked[w]) {
                    edgeTo[w] = v;
                    dfs(G, w);
                } else if (onStack[w]) {
                    int now = v;
                    while (now != w) {
                        cycle.push(now);
                        now = edgeTo[now];
                    }
                    cycle.push(w);
                    cycle.push(v);
                }
            }
            onStack[v] = false;
        }
        public boolean hasCycle() {
            return !cycle.isEmpty();
        }
    }

    private static class ShortestAncestralPath {
        int from; int to; WordNet1 net; int V;
        private final Queue<Integer> qA;
        private final Queue<Integer> qB;
        private final boolean[] mA;
        private final boolean[] mB;
        private final int[] cA;
        private final int[] cB;
        ShortestAncestralPath(WordNet1 net, int from, int to) {
            this.from = from; this.to = to; this.net = net;
            this.V = net.V();
            this.mA = new boolean[V];
            this.mB = new boolean[V];
            this.cA = new int[V];
            this.cB = new int[V];
            this.qB = new Queue<>();
            this.qA = new Queue<>();
            bfs();
        }
        private void bfs() {
            qA.enqueue(from);
            qB.enqueue(to);
            mA[from] = true;
            mB[to] = true;
            int father = -1;
            while (!qA.isEmpty() && !qB.isEmpty()) {
                int w = qA.dequeue();
                if (mB[w]) {
                    father = w;
                    break;
                }
                net.adj(w).forEach(p -> {
                    if (!mA[p]) {
                        qA.enqueue(p);
                        mA[p] = true;
                        cA[p] = w;
                    }
                });
                int w2 = qB.dequeue();
                if (mA[w]) {
                    father = w;
                    break;
                }
                net.adj(w2).forEach(p -> {
                    if (!mB[p]) {
                        qB.enqueue(p);
                        mB[p] = true;
                        cB[p] = w2;
                    }
                });
            }
            if (father != -1) {
                Deque<Integer> dq = new ArrayDeque<>();
                for (int a = father; a != from; a = cA[a]) {
                    count += 1;
                    dq.addFirst(a);
                }
                dq.addFirst(from);
                for (int b = father; b != to; b = cB[b]) {
                    count += 1;
                    if (b != father) dq.addLast(b);
                }
                path = dq;
            } else path = null;
        }
        private int count = 0;
        private Iterable<Integer> path;

        int distance() { return count; }

        Iterable<Integer> path() { return path; }
    }

    // constructor takes the name of the two input files
    public WordNet1(String synsets, String hypernyms) {
        if (synsets == null || hypernyms == null) throw new IllegalArgumentException("Param is null");
        In sy = new In(synsets);
        while (!sy.isEmpty()) {
            String line = sy.readLine();
            //System.out.println("Now Reading" + line);
            String[] d = line.split(",");
            Integer id = Integer.parseInt(d[0]);
            String[] words = d[1].split(" "); String desc = d[2];
            Word obj = new Word(id, words, desc);
            dict.put(id, obj);
            for (String word : words) {
                reverseDict.put(word, obj);
            }
        }
        sy.close();
        In hy = new In(hypernyms);
        while (!hy.isEmpty()) {
            String line = hy.readLine();
            String[] d = line.split(",");
            Integer id = Integer.parseInt(d[0]);
            graph.computeIfAbsent(id, k -> new HashSet<>());
            HashSet<Integer> set = graph.get(id);
            for (int i = 1; i < d.length; i++) {
                Integer now = Integer.parseInt(d[i]);
                set.add(now);
            }
        }
        hy.close();
        if (new DirectedCycle().hasCycle())
            throw new IllegalStateException("Input has cycle, can't process on.");
    }

    // returns all WordNet nouns
    public Iterable<String> nouns() {
        return reverseDict.keySet();
    }

    // is the word a WordNet noun?
    public boolean isNoun(String word) {
        if (word == null) throw new IllegalArgumentException("Param is null.");
        return reverseDict.containsKey(word);
    }

    private int[] checkParam(String nounA, String nounB) {
        if (nounA == null || nounB == null) throw new IllegalArgumentException("Null Param");
        Word wdA = reverseDict.get(nounA);
        Word wdB = reverseDict.get(nounB);
        if (wdA == null || wdB == null) throw new IllegalArgumentException("Can't find this word in system.");
        int[] rs = new int[2];
        rs[0] = wdA.id;
        rs[1] = wdB.id;
        return rs;
    }

    // distance between nounA and nounB (defined below)
    public int distance(String nounA, String nounB) {
        int[] ck = checkParam(nounA, nounB);
        return new ShortestAncestralPath(this, ck[0], ck[1]).distance();
    }

    // a synset (second field of synsets.txt) that is the common ancestor of nounA and nounB
    // in a shortest ancestral path (defined below)
    public String sap(String nounA, String nounB) {
        int[] ck = checkParam(nounA, nounB);
        StringBuilder sb = new StringBuilder();
        for (int i : new ShortestAncestralPath(this, ck[0], ck[1]).path()) {
            sb.append(dict.get(i).words[0]).append(" ");
        } return sb.toString();
    }

    // do unit testing of this class
    public static void main(String[] args) {
        WordNet1 net = new WordNet1("data/synsets.txt", "data/hypernyms.txt");
        System.out.println(net);
        int distance = net.distance("horse", "table");
        System.out.println(distance);
    }
}