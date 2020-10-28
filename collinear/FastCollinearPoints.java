import java.util.Arrays;

public class FastCollinearPoints {
    private final Point[] ps;
    public FastCollinearPoints(Point[] points) {
        if (points == null) throw new IllegalArgumentException("input argument is null.");
        ps = points.clone();
        for (Point p : ps) {
            if (p == null) throw new IllegalArgumentException("point in Point[] have null");
        }
        Arrays.sort(ps);
        for (int i = 1; i < ps.length; i++) {
            Point now = ps[i];
            Point last = ps[i - 1];
            if (now.equals(last)) throw new IllegalArgumentException("point in Point[] is duplicated");
        }
    }
    public int numberOfSegments() {
        Point[] temp;
        int segmentCount = 0;
        int start;

        /*System.out.print("ORIGIN [[[");
        for (Point point : ps) {
            System.out.print(point);
        }
        System.out.println(" ]]]");*/

        for (int i = 0; i < ps.length; i++) {
            Point now = ps[i];
            temp = new Point[ps.length - i - 1];
            start = 0;
            for (int j = i + 1; j < ps.length; j++) {
                Point other = ps[j];
                temp[start++] = other;
            }
            Arrays.sort(temp,now.slopeOrder());


            /*System.out.print("TEMP [[[");
            for (Point point : temp) {
                System.out.print(point);
                System.out.print(String.format("-> %.2f ", now.slopeTo(point)));
            }
            System.out.println(" ]]]");*/

            if (temp.length == 0) continue;
            int count = 1;
            for (int m = 0; m < temp.length - 1; m++) {
                Point a = temp[m];
                Point b = temp[m + 1];
                // System.out.print("Finding from index" + m + String.format(" now %s, next %s", a, b));
                if (now.slopeTo(a) != now.slopeTo(b)) {
                    // System.out.println("=========== BREAK");
                    if (count >= 2) {
                        segmentCount += 1;
                        // System.out.println("COUNT SEGMENTCOUNT NOW " + segmentCount);
                    }
                    count = 1;
                } else if (m == temp.length - 2) {
                    if (count >= 2) {
                        segmentCount += 1;
                    }
                } else {
                    // System.out.println("");
                    count += 1;
                }
            }
        }
        return segmentCount;
    }
    public LineSegment[] segments() {
        int length = numberOfSegments();
        //System.out.println("Length" + length);
        Point[] temp;
        LineSegment[] ls = new LineSegment[length];
        int lsStart = 0;
        int start;
        for (int i = 0; i < ps.length; i++) {
            Point now = ps[i];
            temp = new Point[ps.length - i - 1];
            start = 0;
            for (int j = i + 1; j < ps.length; j++) {
                Point other = ps[j];
                temp[start++] = other;
            }
            Arrays.sort(temp,now.slopeOrder());

            /*System.out.print("TEMP [[[");
            for (Point point : temp) {
                System.out.print(point);
            }
            System.out.println(" ]]]");*/

            if (temp.length == 0) continue;
            int count = 1;
            for (int m = 0; m < temp.length - 1; m++) {
                Point a = temp[m];
                Point b = temp[m + 1];
                if (now.slopeTo(a) != now.slopeTo(b)) {
                    if (count >= 2) {
                        ls[lsStart++] = new LineSegment(now,a);
                    }
                    count = 1;
                } else if (m == temp.length - 2) {
                    if (count >= 2) {
                        ls[lsStart++] = new LineSegment(now,b);
                    }
                } else {
                    count += 1;
                }
            }
        }
        return ls;
    }
}