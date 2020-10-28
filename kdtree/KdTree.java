import edu.princeton.cs.algs4.Point2D;
import edu.princeton.cs.algs4.RectHV;
import edu.princeton.cs.algs4.Stack;
import edu.princeton.cs.algs4.StdDraw;

public class KdTree {

    private static class Node {
        Point2D value;
        Node left;
        Node right;
        int level;
        RectHV rect;
        boolean isVertical() {
            return level % 2 != 0;
        }
        static Node of(Point2D value, RectHV rect, int level) {
            Node n = new Node();
            n.value = value;
            n.level = level;
            n.rect = rect;
            return n;
        }

        @Override
        public String toString() {
            return "Node{" +
                    "value=" + value +
                    ", level=" + level +
                    ", rect=" + rect +
                    '}';
        }
    }

    private Node root;

    private int size;

    private boolean insertSameGot = false;

    private Point2D nearestMinPoint;

    private double nearestMinDistance;

    public boolean isEmpty() { return size == 0; }

    public int size() { return size; }

    public void insert(Point2D p) {
        if (p == null) throw new IllegalArgumentException("Point2D argument is null.");
        insertSameGot = false;
        root = insert(root, p, new RectHV(0,0,1,1), 1);
        if (!insertSameGot) size += 1;
        ////System.out.println("Finish insert, root is " + root);
    }

    private RectHV leftRect(RectHV rect, Point2D p) {
        return new RectHV(rect.xmin(), rect.ymin(), p.x(), rect.ymax());
    }

    private RectHV rightRect(RectHV rect, Point2D p) {
        return new RectHV(p.x(), rect.ymin(), rect.xmax(), rect.ymax());
    }

    private RectHV topRect(RectHV rect, Point2D p) {
        return new RectHV(rect.xmin(), p.y(), rect.xmax(), rect.ymax());
    }

    private RectHV bottomRect(RectHV rect, Point2D p) {
        return new RectHV(rect.xmin(), rect.ymin(), rect.xmax(), p.y());
    }

    private Node insert(Node n, Point2D p,  RectHV rect, int level) {
        //System.out.println("Inserting " + n + " with point " + p + " @level#" + level + ", rect " + rect);
        if (n == null) return Node.of(p, rect, level);
        if (n.value.equals(p)) {
            insertSameGot = true; return n;
        }
        //System.out.println("For node " + n + ", p " + p + " go Left?" + goLeft(n,p));
        if (goLeft(n,p)) n.left = insert(n.left,p,
                n.isVertical() ? leftRect(rect,n.value) : bottomRect(rect,n.value), level + 1);
        else n.right = insert(n.right,p,
                n.isVertical() ? rightRect(rect,n.value) : topRect(rect,n.value), level + 1);
        return n;
    }

    private boolean goLeft(Node n, Point2D p) {
        if (n.isVertical()) return p.x() < n.value.x();
        else return p.y() < n.value.y();
    }

    public void draw() {
        if (root == null) return;
        drawNode(root, root);
    }

    private void drawNode(Node n, Node f) {
        if (n == null) return;
        StdDraw.setPenRadius(0.03);
        StdDraw.setPenColor(StdDraw.BLACK);
        n.value.draw();
        StdDraw.text(n.value.x() + 0.07,n.value.y() - 0.03,n.value.toString());
        StdDraw.setPenRadius(0.01);
        if (n.isVertical()) {
            StdDraw.setPenColor(StdDraw.RED);
            if (f != n) {
                if (f.left == n) StdDraw.line(n.value.x(), 0, n.value.x(), f.value.y());
                else if (f.right == n) StdDraw.line(n.value.x(), f.value.y(), n.value.x(), 1);
            } else StdDraw.line(n.value.x(), 0, n.value.x(), 1);
        } else {
            StdDraw.setPenColor(StdDraw.BLUE);
            if (f != n) {
                if (f.left == n) StdDraw.line(0, n.value.y(), f.value.x(), n.value.y());
                else if (f.right == n) StdDraw.line(f.value.x(), n.value.y(), 1, n.value.y());
            } else StdDraw.line(0, n.value.y(), 1, n.value.y());
        }
        drawNode(n.left, n);
        drawNode(n.right, n);
    }

    public boolean contains(Point2D p) {
        if (p == null) throw new IllegalArgumentException("Point2D argument is null.");
        return contains(root, p);
    }

    private boolean contains(Node n, Point2D p) {
        if (n == null) return false;
        if (n.value.equals(p)) return true;
        else {
            if (goLeft(n,p)) return contains(n.left, p);
            else return contains(n.right, p);
        }
    }

    public Iterable<Point2D> range(RectHV rect) {
        if (rect == null) throw new IllegalArgumentException("RectHV is null.");
        Stack<Point2D> t = new Stack<>();
        if (root == null) return t;
        else range(root, rect, t);
        return t;
    }

    private void range(Node n, RectHV rect, Stack<Point2D> t) {
        if (n == null) return;
        if (rect.contains(n.value)) t.push(n.value);
        if (n.left != null && n.left.rect.intersects(rect)) range(n.left,rect,t);
        if (n.right != null && n.right.rect.intersects(rect)) range(n.right,rect,t);
    }

    public Point2D nearest(Point2D p) {
        if (p == null) throw new IllegalArgumentException("Point2D argument is null.");
        if (root == null) return null;
        nearestMinDistance = Integer.MAX_VALUE;
        nearestMinPoint = root.value;
        nearest(p, root);
        return nearestMinPoint;
    }

private void nearest(Point2D p, Node n) {
    double distance = p.distanceSquaredTo(n.value);
    if (distance < nearestMinDistance) {
        nearestMinDistance = distance; nearestMinPoint = n.value;
    }
    if (goLeft(n,p)) {
        if (n.left != null && n.left.rect.distanceSquaredTo(p) < nearestMinDistance) nearest(p, n.left);
        if (n.right != null && n.right.rect.distanceSquaredTo(p) < nearestMinDistance) nearest(p, n.right);
    } else {
        if (n.right != null && n.right.rect.distanceSquaredTo(p) < nearestMinDistance) nearest(p, n.right);
        if (n.left != null && n.left.rect.distanceSquaredTo(p) < nearestMinDistance) nearest(p, n.left);
    }
}

    public static void main(String[] args) {
        Point2D p1 = new Point2D(0.7, 0.2);
        Point2D p2 = new Point2D(0.5, 0.4);
        Point2D p3 = new Point2D(0.2, 0.3);
        Point2D p4 = new Point2D(0.4, 0.7);
        Point2D p5 = new Point2D(0.9, 0.6);
        KdTree set = new KdTree();
        StdDraw.setPenRadius(0.05);
        set.insert(p1);
        set.insert(p2);
        set.insert(p3);
        set.insert(p4);
        set.insert(p5);
        if (!(set.contains(p1) && set.contains(p4))) throw new RuntimeException("包含错误");
        if (set.contains(new Point2D(Integer.MAX_VALUE, Integer.MAX_VALUE))) throw new RuntimeException("包含错误");
        set.draw();
        RectHV rect = new RectHV(0.1,  0.1,  0.45,  0.8);
        Iterable<Point2D> in = set.range(rect);
        in.forEach(p -> System.out.println("Point " + p + " in Rect " + rect));
        Point2D pp = set.nearest(new Point2D(0.25,  0.35));
        System.out.println("Nearest 0.25*0.35 is " + pp);
        if (!p3.equals(pp)) throw new RuntimeException("nearest impl error.");
    }
}