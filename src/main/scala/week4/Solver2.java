package week4;

import edu.princeton.cs.algs4.MinPQ;
import edu.princeton.cs.algs4.StdOut;

import java.util.Iterator;

public class Solver2 {

    private MinPQ<Board> pq;
    private final Board initBoard;
    private Board prevBoard;
    private transient int moveCache = -2; //-2 for initial, -1 for unsolved

    private void initPQ(Board initial) {
        // System.out.println("Init pq now with " + initial);
        pq = new MinPQ<>((o1, o2) -> {
            int o1h = o1.manhattan();
            int o2h = o2.manhattan();
            return Integer.compare(o1h,o2h);
        });
        pq.insert(initial);
        prevBoard = initial;
    }

    // find a solution to the initial board (using the A* algorithm)
    public Solver2(Board initial) {
        if (initial == null) throw new IllegalArgumentException("input Board is null");
        initPQ(initial);
        initBoard = initial;
    }

    // is the initial board solvable? (see below)
    public boolean isSolvable() throws InterruptedException {
        // if not init, init moves and get moveCache, it may be -1(unsolved) or n(n>0)
        if (moveCache == -2) {
            moveCache = moves();
        }
        return moveCache != -1;
    }

    private boolean isRun() {
        boolean run;
        if (prevBoard.equals(initBoard)) run = true;
        else run = pq.min().manhattan() - 1 <= prevBoard.manhattan();
        return run;
    }

    // min number of moves to solve initial board; -1 if unsolvable
    public int moves() throws InterruptedException {
        if (moveCache != -2) return moveCache;
        else {
            int count = -1;
            initPQ(initBoard);
            boolean run = true;
            while (isRun()) {
                Thread.sleep(1000);
                Board board = pq.delMin();
                System.out.println("now is " + board);
                System.out.println("curr manh " + board.manhattan());
                System.out.println("prev manh " + prevBoard.manhattan());
                count += 1;
                board.neighbors().forEach(neighbor -> {
                    if (!neighbor.equals(prevBoard)) {
                        pq.insert(neighbor);
                    }
                });
                prevBoard = board;
            }
            if (prevBoard.manhattan() == 0) {
                moveCache = count;
                return count;
            }
            else return -1;
        }
    }

    // sequence of boards in a shortest solution; null if unsolvable
    public Iterable<Board> solution() throws InterruptedException {
        int moves = moves();
        if (moves == -1) return null;
        else {
            Board[] boardResult = new Board[moves + 1];
            int boardResultIndex = 0;

            initPQ(initBoard);

            while (pq.min().manhattan() <= prevBoard.manhattan()) {
                Board board = pq.delMin();
                // System.out.println("min manhattan is " + board.manhattan());
                // System.out.println("now board is " + board);
                board.neighbors().forEach(neighbor -> {
                    if (!neighbor.equals(prevBoard)) {
                        // System.out.println("insert board " + neighbor);
                        pq.insert(neighbor);
                    }
                });
                boardResult[boardResultIndex++] = board;
                prevBoard = board;
            }
            // System.out.println("result " + prevBoard);
            // System.out.println("result manhattan " + prevBoard.manhattan());
            return () -> new Iterator<Board>() {
                private int pointer = 0;
                @Override public boolean hasNext() {
                    return pointer != boardResult.length;
                }
                @Override public Board next() {
                    return boardResult[pointer++];
                }
            };
        }
    }

    // test client (see below)
    public static void main(String[] args) throws InterruptedException {
        /*int[] a = new int[]{0,1,3};
        int[] b = new int[]{4,2,5};
        int[] c = new int[]{7,8,6};*/
        /*int[] a = new int[]{1,2,3};
        int[] b = new int[]{4,5,6};
        int[] c = new int[]{8,7,0};*/
        int[] a = new int[]{5,2,3};
        int[] b = new int[]{4,7,0};
        int[] c = new int[]{8,6,1};
        int[][] initArray = new int[][]{a,b,c};
        Board board = new Board(initArray);
        // solve the puzzle
        Solver2 solver = new Solver2(board);

        // print solution to standard output
        if (!solver.isSolvable())
            StdOut.println("No solution possible");
        else {
            StdOut.println("Minimum number of moves = " + solver.moves());
            for (Board bo : solver.solution())
                StdOut.println(bo);
        }
    }

}