package week3;

import java.util.Arrays;

public class BruteCollinearPoints {

    private final Point[] points;

    public BruteCollinearPoints(Point[] points) {
        if (points == null) throw new IllegalArgumentException("input argument is null");
        for (Point now : points) {
            if (now == null) throw new IllegalArgumentException("Point in array have null");
        }
        this.points = points.clone();
        Arrays.sort(this.points);
        for (int i = 1; i < this.points.length; i += 1) {
            Point last = this.points[i - 1];
            Point now = this.points[i];
            if (now.equals(last)) throw new IllegalArgumentException("Point duplicated");
        }
    }    // finds all line segments containing 4 points

    public int numberOfSegments() {
        int count = 0;
        for (int i = 0; i < points.length; i++) {
            Point iP = points[i];
            for (int j = i + 1; j < points.length; j++) {
                Point jP = points[j];
                double ij = iP.slopeTo(jP);
                for (int m = j + 1; m < points.length; m++) {
                    Point mP = points[m];
                    double im = iP.slopeTo(mP);
                    if (ij != im) continue;
                    for (int n = m + 1; n < points.length; n++) {
                        Point nP = points[n];
                        double in = iP.slopeTo(nP);
                        if (ij == in) count ++;
                    }
                }
            }
        }
        return count;
    }   // the number of line segments

    public LineSegment[] segments() {
        LineSegment[] lines = new LineSegment[numberOfSegments()];
        int index = -1;
        for (int i = 0; i < points.length; i++) {
            Point iP = points[i];
            for (int j = i + 1; j < points.length; j++) {
                Point jP = points[j];
                double ij = iP.slopeTo(jP);
                // System.out.println("for " + iP + " vs. " + jP + " :slope is " + ij);
                for (int m = j + 1; m < points.length; m++) {
                    Point mP = points[m];
                    double im = iP.slopeTo(mP);
                    if (ij != im) continue;
                    for (int n = m + 1; n < points.length; n++) {
                        Point nP = points[n];
                        double in = iP.slopeTo(nP);
                        if (ij == in) {
                            index += 1;
                            //因为从小到大排过序，因此 iP 似乎总是小于 nP
                            lines[index] = new LineSegment(iP,nP);
                        }
                    }
                }
            }
        }
        return lines;
    }   // the line segments
}
