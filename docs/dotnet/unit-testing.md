---
metaTitle: "Unit testing"
description: "Adding MSTest unit testing project to an existing solution, Creating a sample test method"
---

# Unit testing



## Adding MSTest unit testing project to an existing solution


- Right click on the solution, Add new project
- From the Test section, select an Unit Test Project
- Pick a name for the assembly - if you are testing project `Foo`, the name can be `Foo.Tests`
- Add a reference to the tested project in the unit test project references



## Creating a sample test method


MSTest (the default testing framework) requires you to have your test classes decorated by a `[TestClass]` attribute, and the test methods with a `[TestMethod]` attribute, and to be public.

```dotnet
[TestClass]
public class FizzBuzzFixture
{
    [TestMethod]
    public void Test1()
    {
        //arrange
        var solver = new FizzBuzzSolver();
        //act
        var result = solver.FizzBuzz(1);
        //assert
        Assert.AreEqual("1",result);
    }
}

```

