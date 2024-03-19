namespace AocTests;

// rg --files --type cs | entr -c dotnet test

[TestClass]
public class Day01Test
{


    [TestMethod]
    public void TestFilterMap1()
    {
        List<int> expected = [1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9];

        CollectionAssert.AreEqual(expected,
            Aoc.Day01.FilterMap1("1.abc.oneight--nineightseveninessixfivefourthreetwone0123456789")
        );

        CollectionAssert.AreEqual(expected,
            Aoc.Day01.FilterMap1Func("1.abc.oneight--nineightseveninessixfivefourthreetwone0123456789")
        );
    }

    [TestMethod]
    public void TestFilterMap2()
    {
        const string input = "1.abc.oneight--nineightseveninessixfivefourthreetwone0123456789";
        List<int> expected = [1, 1, 8, 9, 8, 7, 9, 6, 5, 4, 3, 2, 1, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9];

        CollectionAssert.AreEqual(expected, Aoc.Day01.FilterMap2(input));
        CollectionAssert.AreEqual(expected, Aoc.Day01.FilterMap2Func(input));
    }


}