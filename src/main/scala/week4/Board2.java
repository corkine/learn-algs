package week4;

import java.util.Arrays;
import java.util.Iterator;

public class Board2 {

    private final int[][] tiles;
    private final int N;
    private int zeroCol;
    private int zeroRow;
    private int manhattan = -1;
    private int hamming = -1;

    // create a board from an n-by-n array of tiles,
    // where tiles[row][col] = tile at (row, col)
    public Board2(int[][] tiles) {
        int mayN = tiles.length;
        int[][] result = new int[mayN][mayN];
        // check if is a cube and find zero position
        for (int row = 0; row < tiles.length; row++) {
            int colMax = tiles[row].length;
            assert (colMax == mayN);
            for (int col = 0; col < colMax; col++) {
                int now = tiles[row][col];
                result[row][col] = now;
                if (now == 0) {
                    this.zeroRow = row;
                    this.zeroCol = col;
                }
            }
        }
        this.tiles = result;
        this.N = this.tiles.length;
    }

    // string representation of this board
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(tiles.length).append("\n");
        for (int[] rowArray : tiles) {
            for (int col: rowArray) {
                sb.append(" ").append(col);
            }
            sb.append("\n");
        }
        return sb.toString();
    }

    // board dimension n
    public int dimension() {
        return N;
    }

    // number of tiles out of place
    public int hamming() {
        if (hamming == -1) {
            int count = 0;
            for (int row = 0; row < N; row++) {
                for (int col = 0; col < N; col++) {
                    int rightAnswer = row * N + col + 1;
                    int now = tiles[row][col];
                    if (rightAnswer == N * N) continue;
                    if (rightAnswer != now) count += 1;
                }
            }
            hamming = count;
            return count;
        }
        else return hamming;
    }

    // sum of Manhattan distances between tiles and goal
    public int manhattan() {
        if (manhattan == -1) {
            int count = 0;
            for (int row = 0; row < N; row++) {
                for (int col = 0; col < N; col++) {
                    int rightAnswer = row * N + col + 1;
                    int now = tiles[row][col];
                    if (now != 0 && rightAnswer != now) {
                        int nowReallyRow = (now - 1) / N;
                        int nowReallyCol = (now - 1) % N;
                        count += (Math.abs(col - nowReallyCol) + Math.abs(row - nowReallyRow));
                    }
                }
            }
            manhattan = count;
            return count;
        }
        else return manhattan;
    }

    // is this board the goal board?
    public boolean isGoal() {
        /*int[] data = new int[N*N];
        int index = 0;
        for (int row = 0; row < N; row++) {
            for (int col = 0; col < N; col++) {
                data[index++] = tiles[row][col];
            }
        }
        for (int i = 1; i < data.length; i++) {
            if (i == N * N - 1) {
                if (data[i-1] - data[i] != (N*N-1)) return false;
            } else {
                if (data[i] - data[i-1] != 1) return false;
            }
        }
        return true;*/
        return hamming() == 0;
    }

    // does this board equal y?
    public boolean equals(Object y) {
        if (y instanceof Board2) {
            Board2 o = (Board2)y;
            if (o.N != N) return false;
            for (int row = 0; row < N; row++) {
                for (int col = 0; col < N; col++) {
                    if (o.tiles[row][col] != this.tiles[row][col]) return false;
                }
            }
            return true;
        } else return false;
    }

    private int[][] exchange(int toRow, int toCol) {
        int[][] changed = new int[N][N];
        for (int row = 0; row < N; row++) {
            System.arraycopy(tiles[row], 0, changed[row], 0, N);
        }
        // System.out.println("exchange row " + toRow + ", col " + toCol);
        int toValue = changed[toRow][toCol];
        changed[zeroRow][zeroCol] = toValue;
        changed[toRow][toCol] = 0;
        return changed;
    }

    // all neighboring boards
    public Iterable<Board2> neighbors() {
        // find the zero position
        int[][] around = new int[4][2];
        if (zeroCol - 1 > -1) {
            around[0][0] = zeroRow;
            around[0][1] = zeroCol - 1;
        } else {
            around[0][0] = Integer.MAX_VALUE;
            around[0][1] = Integer.MAX_VALUE;
        }
        if (zeroCol + 1 < N) {
            around[1][0] = zeroRow;
            around[1][1] = zeroCol + 1;
        } else {
            around[1][0] = Integer.MAX_VALUE;
            around[1][1] = Integer.MAX_VALUE;
        }
        if (zeroRow - 1 > -1) {
            around[2][0] = zeroRow - 1;
            around[2][1] = zeroCol;
        } else {
            around[2][0] = Integer.MAX_VALUE;
            around[2][1] = Integer.MAX_VALUE;
        }
        if (zeroRow + 1 < N) {
            around[3][0] = zeroRow + 1;
            around[3][1] = zeroCol;
        } else {
            around[3][0] = Integer.MAX_VALUE;
            around[3][1] = Integer.MAX_VALUE;
        }
        // compute the possible move from others to zero
        // gen boards and make it to Iterable<Board>
        Arrays.sort(around, (o1, o2) -> {
            int a = o1[0];
            int b = o2[0];
            return Integer.compare(a, b);
        });
        return () -> new Iterator<Board2>() {
            private int index = 0;
            @Override public boolean hasNext() {
                return index < around.length && around[index][0] != Integer.MAX_VALUE;
            }
            @Override public Board2 next() {
                int[][] changed = exchange(around[index][0],around[index][1]);
                index++;
                return new Board2(changed);
            }
        };
    }

    // a board that is obtained by exchanging any pair of tiles
    public Board2 twin() {
        Board2 twinBoard = this;
        int row = 0;
        int col = 0;
        if (twinBoard.tiles[0][0]  == 0) {
            col = 1;
        }
        for (int i = 0; i < N; i++) {
            for (int j = 0; j < N; j++) {
                if (twinBoard.tiles[i][j] != twinBoard.tiles[row][col] &&
                        twinBoard.tiles[i][j] != 0) {
                    int tmp = twinBoard.tiles[i][j];
                    twinBoard.tiles[i][j] = twinBoard.tiles[row][col];
                    twinBoard.tiles[row][col] = tmp;
                    return twinBoard;
                }
            }
        }
        return this;
    }

    // unit testing (not graded)
    public static void main(String[] args) {
        int[] a = new int[]{1,0,3};
        int[] b = new int[]{4,2,5};
        int[] c = new int[]{7,8,6};
        int[][] initArray = new int[][] {a,b,c};
        Board2 board = new Board2(initArray);
        System.out.println(board);

        int[] a2 = new int[]{8,1,3};
        int[] b2 = new int[]{4,0,2};
        int[] c2 = new int[]{7,6,5};
        int[][] initArray2 = new int[][]{a2,b2,c2};
        Board2 board2 = new Board2(initArray2);
        System.out.println(board2);
        System.out.println(board2.hamming());
        System.out.println(board2.manhattan());

        int[] a3 = new int[]{8,1,3};
        int[] b3 = new int[]{4,2,0};
        int[] c3 = new int[]{7,6,5};
        int[][] initArray3 = new int[][] {a3,b3,c3};
        Board2 board3 = new Board2(initArray3);
        System.out.println(board3);
        board3.neighbors().iterator().forEachRemaining(System.out::println);

        System.out.println(board3.isGoal());

        int[] a4 = new int[]{1,2,3};
        int[] b4 = new int[]{4,5,6};
        int[] c4 = new int[]{7,8,0};
        int[][] initArray4 = new int[][] {a4,b4,c4};
        Board2 board4 = new Board2(initArray4);
        System.out.println(board4.isGoal());
    }

}