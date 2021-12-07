package day01;

import java.io.File;
import java.io.FileNotFoundException;
import java.util.ArrayDeque;
import java.util.Deque;
import java.util.Scanner;

public class Sonar {
    public static void main(String args[]) throws FileNotFoundException {
        File file = new File(args[0]);
        System.out.println(measure(file, 1));
        System.out.println(measure(file, 3));
    }

    public static int measure(File file, int window_size) throws FileNotFoundException {
        Scanner input = new Scanner(file);
        int depth_increase_count = 0;
        Deque<Integer> depth_window = new ArrayDeque<>();
        while (window_size > 0) {
            depth_window.add(input.nextInt());
            window_size--;
        }
        while (input.hasNextInt()) {
            int new_depth = input.nextInt();
            if (new_depth > depth_window.pop()) {
                depth_increase_count++;
            }
            depth_window.add(new_depth);
        }
        input.close();
        return depth_increase_count;
    }
}
