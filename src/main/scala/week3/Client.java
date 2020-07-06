package week3;

import edu.princeton.cs.algs4.StdDraw;
import edu.princeton.cs.algs4.StdOut;

import java.awt.*;

public class Client {
    public static void main(String[] args) {

        Point[] points = new Point[6];
        points[0] = new Point(10,10);

        new BruteCollinearPoints(points);

        /*Point[] points = new Point[6];
        points[0] = new Point(19000,10000);
        points[1] = new Point(18000,10000);
        points[2] = new Point(32000,10000);
        points[3] = new Point(21000,10000);
        points[4] = new Point(1234,5678);
        points[5] = new Point(14000,10000);*/

        // draw the points
        StdDraw.setPenRadius(.02);
        StdDraw.setPenColor(Color.RED);
        StdDraw.enableDoubleBuffering();
        StdDraw.setXscale(0, 32768);
        StdDraw.setYscale(0, 32768);
        for (Point p : points) {
            p.draw();
        }
        StdDraw.show();

        // print and draw the line segments
        BruteCollinearPoints collinear = new BruteCollinearPoints(points);
        for (LineSegment segment : collinear.segments()) {
            StdOut.println(segment);
            segment.draw();
        }
        StdDraw.show();
    }
}
