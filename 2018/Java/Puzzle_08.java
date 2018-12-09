import java.util.*;

public class Puzzle_08 {

    private class Node {
        int[] meta;
        Node[] children;
        Node(int children, int metas) {
            meta = new int[metas];
            this.children = new Node[children];
        }
        int getChecksum() {
            int checksum = 0;
            for (int meta : this.meta) {
                checksum += meta;
            }
            for (Node child : children) {
                checksum += child.getChecksum();
            }
            return checksum;
        }
        int getChecksum2() {
            int checksum = 0;
            if (children.length == 0) {
                for (int meta : this.meta) {
                    checksum += meta;
                }
            } else {
                for (int meta : this.meta) {
                    if (meta <= children.length)
                        checksum += children[meta-1].getChecksum2();
                }
            }
            return checksum;
        }
    }

    private void solve() {
        Node root = extractNode(parseInput());
        System.out.println(root.getChecksum());
        System.out.println(root.getChecksum2());
    }

    private Node extractNode(Iterator<Integer> nodes) {
        int children = nodes.next();
        int metas = nodes.next();
        Node node = new Node(children, metas);
        for (int i = 0; i < children; i++ )
            node.children[i] = extractNode(nodes);
        for (int i = 0; i < metas; i++) {
            node.meta[i] = nodes.next();
        }
        return node;
    }

    private Iterator<Integer> parseInput() {
        List<Integer> nodes = new LinkedList<>();
        Scanner input = new Scanner(System.in);
        while (input.hasNextInt()) {
            nodes.add(input.nextInt());
        }
        return nodes.iterator();
    }

    /**
     * Run with 'java Puzzle_08 < input_08.txt'.
     * @param args
     */
    public static void main(String[] args) {
        new Puzzle_08().solve();
    }

}
