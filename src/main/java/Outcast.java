public class Outcast {
    private final WordNet wordnet;

    // constructor takes a WordNet object
    public Outcast(WordNet wordnet) {
        this.wordnet = wordnet;
    }
    // given an array of WordNet nouns, return an outcast
    public String outcast(String[] nouns) {
        int length = nouns.length;
        int[][] distance = new int[length][length];

        for(int i = 0; i < length; i++) {
            for(int j = i; j < length; j++) {
                distance[i][j] = wordnet.distance(nouns[i], nouns[j]);
            }
        }

        int maxDistance = 0;
        int sum = 0;
        int num = 0;
        for(int i = 0; i < nouns.length; i++) {
            sum = 0;
            for(int j = 0; j < nouns.length; j++) {
                if(i < j)
                    sum += distance[i][j];
                else
                    sum += distance[j][i];
            }

            if(sum > maxDistance) {
                maxDistance = sum;
                num = i;
            }
        }

        return nouns[num];
    }
    // see test client below
    public static void main(String[] args) {
    }
}