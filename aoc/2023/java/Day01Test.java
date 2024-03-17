import org.junit.Test;

import java.util.List;

import org.junit.Assert;

public class Day01Test {

        @Test
        public void part1Filter() {
                Assert.assertEquals(
                                List.of(1, 2, 3, 8),
                                Day01.filterNums1("1abc2pqr3stu8vwx"));

                Assert.assertEquals(
                                List.of(2),
                                Day01.filterNums1("abc.one2threeight"));
        }

        @Test
        public void part2Filter() {
                Assert.assertEquals(
                                List.of(1, 2, 3, 8),
                                Day01.filterNums2("abc.one2threeight"));

                Assert.assertEquals(
                                List.of(1, 2, 3, 8),
                                Day01.filterNums1("1abc2pqr3stu8vwx"));
        }

        @Test
        public void processLines() {
                var input = List.of(
                                "1abc2pqr3stu8vwx", // process1: 18, process2: 18
                                "abc.one2threeight" // process1: 22, process2: 18
                );

                Assert.assertEquals((18 + 22), Day01.processLines1(input));
                Assert.assertEquals((18 + 18), Day01.processLines2(input));
        }

        private void testOffByOne(List<Integer> exp, String input) {
                Assert.assertEquals(
                                exp,
                                Day01.filterNums2(input));
        }

        @Test
        public void offByOne() {
                testOffByOne(List.of(1), "one");
                testOffByOne(List.of(2), "two");
                testOffByOne(List.of(3), "three");
                testOffByOne(List.of(4), "four");
                testOffByOne(List.of(5), "five");
                testOffByOne(List.of(6), "six");
                testOffByOne(List.of(7), "seven");
                testOffByOne(List.of(8), "eight");
                testOffByOne(List.of(9), "nine");

                testOffByOne(List.of(1), "oneon");
                testOffByOne(List.of(2), "twotw");
                testOffByOne(List.of(3), "threethre");
                testOffByOne(List.of(4), "fourfou");
                testOffByOne(List.of(5), "fivefiv");
                testOffByOne(List.of(6), "sixsi");
                testOffByOne(List.of(7), "sevenseve");
                testOffByOne(List.of(8), "eighteigh");
                testOffByOne(List.of(9), "ninenin");

        }

}
