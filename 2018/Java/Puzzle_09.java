public class Puzzle_09 {
    private class Marble {
        final int value;
        Marble cw; // Clock-wise
        Marble cc; // Counter clock-wise

        Marble(int value) {
            this.value = value;
        }

        Marble cw(int skip) {
            if (skip == 0)
                return this;
            else
                return cw.cw(skip - 1);
        }

        Marble cc(int skip) {
            if (skip == 0)
                return this;
            else
                return cc.cc(skip - 1);
        }

        public Marble insert(int i) {
            Marble newMarble = new Marble(i);
            newMarble.cw = this.cw;
            newMarble.cc = this;
            this.cw.cc = newMarble;
            this.cw = newMarble;
            return newMarble;
        }

        public Marble remove() {
            Marble toBeRemoved = this;
            this.cc.cw = this.cw;
            this.cw.cc = this.cc;
            try {
                return this.cw;
            } finally {
                toBeRemoved.cw = null;
                toBeRemoved.cc = null;
            }
        }
    }

    public static void main(String[] args) {
        new Puzzle_09().solve();
    }

    private void solve() {
        System.out.println(maxScore(10, 1618));
        System.out.println(maxScore(13, 7999));
        System.out.println(maxScore(17, 1104));
        System.out.println(maxScore(21, 6111));
        System.out.println(maxScore(30, 5807));
        System.out.println(maxScore(470, 72170));
        long start = System.currentTimeMillis();
        System.out.println(maxScore(470, 100 * 72170));
        long finish = System.currentTimeMillis();
        System.out.printf("Solved in %d milliseconds.%n", finish - start);
    }

    private long maxScore(int players, int maxMarble) {
        long[] scores = new long[players];
        Marble current = new Marble(0);
        current.cw = current;
        current.cc = current;
        for (int i = 1; i <= maxMarble; i++) {
            if (i % 23 == 0) {
                scores[i % players] += i;
                current = current.cc(7);
                scores[i % players] += current.value;
                current = current.remove();
            } else {
                current = current.cw(2 - 1);
                current = current.insert(i);
            }
        }
        long max = 0L;
        for (long score : scores) {
            if (max < score) {
                max = score;
            }
        }
        return max;
    }

}
