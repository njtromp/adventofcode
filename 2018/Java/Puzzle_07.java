import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.InputStream;
import java.util.*;
import java.util.stream.Collectors;

public class Puzzle_07 {
    class Edge {
        final char from;
        final char to;
        Edge(char from, char to) {
            this.from = from;
            this.to = to;
        }
    }
    class Node implements Comparable<Node> {
        final char id;
        final int timeNeeded;
        int timeFinished;
        boolean finished;
        Node(char id) {
            this.id = id;
            timeNeeded = 61 + id - 'A';
        }
        List<Node> connections = new ArrayList<>();
        List<Node> prerequisites = new ArrayList<>();

        @Override
        public int hashCode() {
            return id;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == this)
                return true;
            else {
                if (obj.getClass().equals(Node.class)) {
                    return id == ((Node) obj).id;
                } else
                    return false;
             }
        }

        @Override
        public int compareTo(Node other) {
            return timeNeeded - other.timeNeeded;
        }

        @Override
        public String toString() {
            return Character.toString(id);
        }
    }

    public static void main(String[] args) throws FileNotFoundException {
        new Puzzle_07().solve(new FileInputStream(args[0]), 5);
    }

    private void solve(InputStream in, int workers) {
        List<Edge> edges = parseInput(in);
        List<Node> roots = createTree(edges);
        System.out.printf("%s%n", roots);
        int time = 0;
        List<Node> active = new ArrayList<>();
        Set<Node> queue = new HashSet<>();
        queue.addAll(roots);
        while (!active.isEmpty() || !queue.isEmpty()) {
            if (active.size() < workers) {
                List<Node> candicates = queue.stream().filter(n -> n.prerequisites.stream().allMatch(m -> m.finished)).sorted().collect(Collectors.toList());
                while (active.size() < workers && !candicates.isEmpty()) {
                    active.add(candicates.get(0));
                    queue.remove(candicates.remove(0));
                }
            }
            for (Node node : active) {
                if (node.timeFinished == 0) {
                    node.timeFinished = time + node.timeNeeded;
                }
            }
            active.sort(new Comparator<Node>() {
                @Override
                public int compare(Node o1, Node o2) {
                    return o1.timeFinished - o2.timeFinished;
                }
            });
            Node nearest = active.get(0);
            time = nearest.timeFinished;
            for (Node node : active) {
                if (node.timeFinished == time) {
                    node.finished = true;
                    queue.addAll(node.connections);
                }
            }
            active = active.stream().filter(n -> !n.finished).collect(Collectors.toList());
        }
        System.out.printf("%d%n", time);
    }

    private List<Node> createTree(List<Edge> edges) {
        Map<Character, Node> nodes = new HashMap<>();
        for(Edge edge : edges) {
            Node from = nodes.get(edge.from);
            if (from == null) {
                from = new Node(edge.from);
                nodes.put(edge.from, from);
            }
            Node to = nodes.get(edge.to);
            if (to == null) {
                to = new Node(edge.to);
                nodes.put(edge.to, to);
            }
            from.connections.add(to);
            to.prerequisites.add(from);
        }
        return nodes.values().stream().filter(n -> n.prerequisites.size() == 0).collect(Collectors.toList());
    }

    private List<Edge> parseInput(InputStream in) {
        List<Edge> edges = new ArrayList<>();
        Scanner input = new Scanner(in);
        while (input.hasNextLine()) {
            String edge = input.nextLine();
            edges.add(new Edge(edge.charAt(0), edge.charAt(1)));
        }
        return edges;
    }

}
