import java.util.ArrayList;

public class Board {
    private final int[][] blocks;
    private static final int BLANK = 0;
    private final int N;

    public Board(int[][] blocks) {
        if (blocks == null)
            throw new java.lang.IllegalArgumentException("the blocks is null");
        N = blocks.length;
        this.blocks = new int[N][N];
        for (int i = 0; i < N; i++) {
            System.arraycopy(blocks[i], 0, this.blocks[i], 0, N);
        }
    }

    public int dimension() { return N; }

    private int getIndex(int i, int j) { return i * N + j + 1; }

    private int getRow(int value) { return (value - 1) / N; }

    private int getCol(int value) { return (value - 1) % N; }

    public int hamming() {
        int wrongNum = 0;
        for (int i = 0; i < N; i++)
            for (int j = 0; j < N; j++)
                if (blocks[i][j] != BLANK && blocks[i][j] != getIndex(i, j)) wrongNum++;
        return wrongNum;
    }

    public int manhattan() {
        int wrongNum = 0;
        for (int i = 0; i < N; i++) {
            for (int j = 0; j < N; j++)
                if (blocks[i][j] != BLANK && blocks[i][j] != getIndex(i, j)) {
                    int righti = getRow(blocks[i][j]);
                    int rightj = getCol(blocks[i][j]);
                    wrongNum = wrongNum + Math.abs(righti - i) + Math.abs(rightj - j);
                }
        }
        return wrongNum;
    }

    public boolean isGoal() {
        for (int i = 0; i < N; i++)
            for (int j = 0; j < N; j++)
                if (blocks[i][j] != BLANK && blocks[i][j] != getIndex(i, j)) return false;
        return true;
    }

    private int[][] copy() {
        int[][] newblocks = new int[N][N];
        for (int i1 = 0; i1 < N; i1++)
            System.arraycopy(this.blocks[i1], 0, newblocks[i1], 0, N);
        return newblocks;
    }

    private Board swap(int i1, int j1, int i2, int j2) {
        int[][] newblocks = copy();
        int temp = newblocks[i1][j1];
        newblocks[i1][j1] = newblocks[i2][j2];
        newblocks[i2][j2] = temp;
        return new Board(newblocks);
    }

    public Board twin() {
        int i1 = 0, j1 = 0, i2 = 1, j2 = 1;
        if (blocks[i1][j1] == BLANK) {
            i1 = 1;
            j1 = 0;
        }
        if (blocks[i2][j2] == BLANK) {
            i2 = 1;
            j2 = 0;
        }
        return swap(i1, j1, i2, j2);
    }

    public boolean equals(Object y) {
        if (y == null) return false;
        if (y == this) return true;
        if (y.getClass().isInstance(this)) {
            Board boardY = (Board) y;
            if (boardY.N != this.N)
                return false;
            for (int i = 0; i < N; i++)
                for (int j = 0; j < N; j++)
                    if (this.blocks[i][j] != boardY.blocks[i][j])
                        return false;
        } else { return false; }
        return true;
    }

    public Iterable<Board> neighbors() {
        ArrayList<Board> boards = new ArrayList<>();
        for (int i = 0; i < N; i++) {
            for (int j = 0; j < N; j++) {
                if (blocks[i][j] == BLANK) {
                    // 上
                    if (i > 0) {
                        Board upBoard = swap(i, j, i - 1, j);
                        boards.add(upBoard);
                    }
                    // 下
                    if (i < N - 1) {
                        Board lowBoard = swap(i, j, i + 1, j);
                        boards.add(lowBoard);
                    }
                    // 左
                    if (j > 0) {
                        Board leftBoard = swap(i, j, i, j - 1);
                        boards.add(leftBoard);
                    }
                    // 右
                    if (j < N - 1) {
                        Board rightBoard = swap(i, j, i, j + 1);
                        boards.add(rightBoard);
                    }
                }

            }

        }
        return boards;
    }

    public String toString() {
        StringBuilder sBuilder = new StringBuilder();
        sBuilder.append(N).append("\n");
        for (int i = 0; i < N; i++) {
            for (int j = 0; j < N; j++) {
                sBuilder.append(blocks[i][j]).append("\t");
            }
            sBuilder.append("\n");
        }
        return sBuilder.toString();
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

        System.out.println(board.manhattan());
        System.out.println(board.toString());
        for (Board it : board.neighbors()) {
            System.out.println(it.toString());
        }

        System.out.println(board.twin().toString());

    }
}