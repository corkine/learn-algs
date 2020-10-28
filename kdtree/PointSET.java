import edu.princeton.cs.algs4.Point2D;
import edu.princeton.cs.algs4.RectHV;
import edu.princeton.cs.algs4.StdDraw;

import java.util.TreeSet;

public class PointSET {

    private final TreeSet<Point2D> set;
    private double minDistance = Integer.MAX_VALUE;
    private Point2D minPoint = null;

    public PointSET() {
        set = new TreeSet<>();
    }

    public boolean isEmpty() {
        return set.isEmpty();
    }

    public int size() {
        return set.size();
    }

    public void insert(Point2D p) {
        if (p == null) throw new IllegalArgumentException("Point2D argument is null.");
        set.add(p);
    }

    public boolean contains(Point2D p) {
        if (p == null) throw new IllegalArgumentException("Point2D argument is null.");
        return set.contains(p);
    }

    public void draw() {
        set.forEach(Point2D::draw);
    }

    public Iterable<Point2D> range(RectHV rect) {
        if (rect == null) throw new IllegalArgumentException("RectHV is null.");
        TreeSet<Point2D> res = new TreeSet<>();
        set.forEach(p -> {
            if (rect.contains(p)) res.add(p);
        });
        return res;
    }

    public Point2D nearest(Point2D p) {
        if (p == null) throw new IllegalArgumentException("Point2D argument is null.");
        if (set.isEmpty()) return null;
        set.forEach(c -> {
            double distance = c.distanceSquaredTo(p);
            if (distance < minDistance) {
                minPoint = c;
                minDistance = distance;
            }
        });
        return minPoint;
    }

    public static void main(String[] args) {
        Point2D p1 = new Point2D(0.1, 0.4);
        Point2D p2 = new Point2D(0.0, 0.0);
        Point2D p4 = new Point2D(0.6, 0.5);
        PointSET set = new PointSET();
        StdDraw.setPenRadius(0.05);
        set.insert(p1);
        set.insert(p2);
        set.insert(p4);
        if (!(set.contains(p1) && set.contains(p4))) throw new RuntimeException("包含错误");
        if (set.contains(new Point2D(Integer.MAX_VALUE, Integer.MAX_VALUE))) throw new RuntimeException("包含错误");
        set.draw();
        RectHV rect = new RectHV(0.4,  0.3,  0.8,  0.6);
        Iterable<Point2D> in = set.range(rect);
        in.forEach(p -> System.out.println("Point " + p + " in Rect " + rect));
        Point2D pp = set.nearest(new Point2D(0.1,  0.5));
        System.out.println("Nearest 0.1*0.5 is " + pp);
        if (!p1.equals(pp)) throw new RuntimeException("nearest impl error.");
    }
}