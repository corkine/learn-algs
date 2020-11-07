package chapter4;

import edu.princeton.cs.algs4.In;
import edu.princeton.cs.algs4.Queue;
import edu.princeton.cs.algs4.SET;
import edu.princeton.cs.algs4.StdOut;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class WebsiteSearch {
    public static void main(String[] args) {
        Queue<String> queue = new Queue<>();
        SET<String> marked = new SET<>();
        String root = "https://blog.mazhangjing.com";
        queue.enqueue(root);
        marked.add(root);
        String now = "";
        while (!queue.isEmpty())
        {
            String v = queue.dequeue();
            now = v;
            StdOut.println(v);
            try {
                In in = new In(v);
                String input = in.readAll();
                String regexp = "http://(\\w+\\.)*(\\w+)|https://(\\w+\\.)*(\\w+)";
                Pattern pattern = Pattern.compile(regexp);
                Matcher matcher = pattern.matcher(input);
                while (matcher.find())
                {
                    String w = matcher.group();
                    if (!marked.contains(w))
                    {
                        marked.add(w);
                        queue.enqueue(w);
                    }
                }
            } catch (Exception ignored) {
                System.out.println("Error in reading " + now);
            }
        }
    }
}
