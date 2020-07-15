package week4;

import edu.princeton.cs.algs4.MinPQ;
import edu.princeton.cs.algs4.Stack;

public class Solver {
    private SearchNode currentNode;

    private static class SearchNode implements Comparable<SearchNode> {
        final Board board;
        final SearchNode preNode;
        final int moves;
        final int priority;

        public SearchNode(Board board, SearchNode pNode) {
            this.board = board;
            this.preNode = pNode;
            if (preNode == null) moves = 0;
            else moves = preNode.moves + 1;
            priority = board.manhattan() + moves;
        }

        public int compareTo(SearchNode otherNode) {
            return Integer.compare(this.priority,otherNode.priority);
        }
    }

    public Solver(Board initial) {
        if (initial == null)
            throw new java.lang.IllegalArgumentException("The initial Board is null");

        currentNode = new SearchNode(initial, null);
        MinPQ<SearchNode> minInitialPQ = new MinPQ<>();
        minInitialPQ.insert(currentNode);

        SearchNode currentTwinNode = new SearchNode(initial.twin(), null);
        MinPQ<SearchNode> minTwinNode = new MinPQ<>();
        minTwinNode.insert(currentTwinNode);

        boolean find = false;
        while (!find) {
            currentNode = minInitialPQ.delMin();
            if (currentNode.board.isGoal()) find = true;
            else putNeighborsBoardToPQ(currentNode, minInitialPQ);

            currentTwinNode = minTwinNode.delMin();
            if (currentTwinNode.board.isGoal()) find = true;
            else putNeighborsBoardToPQ(currentTwinNode, minTwinNode);
        }
    }

    private void putNeighborsBoardToPQ(SearchNode node, MinPQ<SearchNode> pq) {
        node.board.neighbors().forEach((neighborBoard) -> {
            if (node.preNode == null || !neighborBoard.equals(node.preNode.board))
                pq.insert(new SearchNode(neighborBoard, node));
        });
    }

    public boolean isSolvable() { return currentNode.board.isGoal(); }

    public int moves() {
        if (isSolvable()) return currentNode.moves;
        else return -1;
    }

    public Iterable<Board> solution() {
        if (!isSolvable()) return null;
        else {
            Stack<Board> stackBoard = new Stack<>();
            SearchNode nowNode = currentNode;
            while (nowNode != null) {
                stackBoard.push(nowNode.board);
                nowNode = nowNode.preNode;
            }
            return stackBoard;
        }
    }

    public static void main(String[] args) {
        int[][] blocks = new int[3][3];

        blocks[0][0] = 8;
        blocks[0][1] = 1;
        blocks[0][2] = 3;

        blocks[1][0] = 4;
        blocks[1][1] = 0;
        blocks[1][2] = 2;

        blocks[2][0] = 7;
        blocks[2][1] = 6;
        blocks[2][2] = 5;
        Board board = new Board(blocks);

        Solver solver = new Solver(board);
        System.out.println(solver.currentNode.preNode == null);
        System.out.println(solver.currentNode.preNode);
        if (!solver.isSolvable()) {
            System.out.println("this board is can't resolve");
        }

        Iterable<Board> bIterable = solver.solution();
        System.out.println(bIterable.toString());

        for (Board it : bIterable) {
            System.out.println(it.toString());
        }

    }
}