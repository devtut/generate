---
metaTitle: "C# | Parallel LINQ (PLINQ)"
description: "Simple example, WithDegreeOfParallelism, AsOrdered, AsUnordered"
---

# Parallel LINQ (PLINQ)



## Simple example


This example shows how PLINQ can be used to calculate the even numbers between 1 and 10,000 using multiple threads. Note that the resulting list will won't be ordered!

```cs
var sequence = Enumerable.Range(1, 10000);
var evenNumbers = sequence.AsParallel()
                          .Where(x => x % 2 == 0)
                          .ToList();

// evenNumbers = { 4, 26, 28, 30, ... }
// Order will vary with different runs

```



## WithDegreeOfParallelism


The degree of parallelism is the maximum number of concurrently executing tasks that will be used to process the query.

```cs
var sequence = Enumerable.Range(1, 10000);
var evenNumbers = sequence.AsParallel()
                          .WithDegreeOfParallelism(4)
                          .Where(x => x % 2 == 0);

```



## AsOrdered


This example shows how PLINQ can be used to calculate the even numbers between 1 and 10,000 using multiple threads. Order will be maintained in the resulting list, however keep in mind that `AsOrdered` may hurt performance for a large numbers of elements, so un-ordered processing is preferred when possible.

```cs
var sequence = Enumerable.Range(1, 10000);
var evenNumbers = sequence.AsParallel()
                          .AsOrdered()
                          .Where(x => x % 2 == 0)
                          .ToList();

// evenNumbers = { 2, 4, 6, 8, ..., 10000 }

```



## AsUnordered


Ordered sequences may hurt performance when dealing with a large number of elements. To mitigate this, it's possible to call `AsUnordered` when the sequence order is no longer necessary.

```cs
var sequence = Enumerable.Range(1, 10000).Select(x => -1 * x); // -1, -2, ...
var evenNumbers = sequence.AsParallel()
                          .OrderBy(x => x)
                          .Take(5000)
                          .AsUnordered()
                          .Where(x => x % 2 == 0) // This line won't be affected by ordering
                          .ToList();

```



#### Syntax


- ParallelEnumerable.Aggregate(func)
- ParallelEnumerable.Aggregate(seed, func)
- ParallelEnumerable.Aggregate(seed, updateAccumulatorFunc, combineAccumulatorsFunc, resultSelector)
- ParallelEnumerable.Aggregate(seedFactory, updateAccumulatorFunc, combineAccumulatorsFunc, resultSelector)
- ParallelEnumerable.All(predicate)
- ParallelEnumerable.Any()
- ParallelEnumerable.Any(predicate)
- ParallelEnumerable.AsEnumerable()
- ParallelEnumerable.AsOrdered()
- ParallelEnumerable.AsParallel()
- ParallelEnumerable.AsSequential()
- ParallelEnumerable.AsUnordered()
- ParallelEnumerable.Average(selector)
- ParallelEnumerable.Cast()
- ParallelEnumerable.Concat(second)
- ParallelEnumerable.Contains(value)
- ParallelEnumerable.Contains(value, comparer)
- ParallelEnumerable.Count()
- ParallelEnumerable.Count(predicate)
- ParallelEnumerable.DefaultIfEmpty()
- ParallelEnumerable.DefaultIfEmpty(defaultValue)
- ParallelEnumerable.Distinct()
- ParallelEnumerable.Distinct(comparer)
- ParallelEnumerable.ElementAt(index)
- ParallelEnumerable.ElementAtOrDefault(index)
- ParallelEnumerable.Empty()
- ParallelEnumerable.Except(second)
- ParallelEnumerable.Except(second, comparer)
- ParallelEnumerable.First()
- ParallelEnumerable.First(predicate)
- ParallelEnumerable.FirstOrDefault()
- ParallelEnumerable.FirstOrDefault(predicate)
- ParallelEnumerable.ForAll(action)
- ParallelEnumerable.GroupBy(keySelector)
- ParallelEnumerable.GroupBy(keySelector, comparer)
- ParallelEnumerable.GroupBy(keySelector, elementSelector)
- ParallelEnumerable.GroupBy(keySelector, elementSelector, comparer)
- ParallelEnumerable.GroupBy(keySelector, resultSelector)
- ParallelEnumerable.GroupBy(keySelector, resultSelector, comparer)
- ParallelEnumerable.GroupBy(keySelector, elementSelector, ruleSelector)
- ParallelEnumerable.GroupBy(keySelector, elementSelector, ruleSelector, comparer)
- ParallelEnumerable.GroupJoin(inner, outerKeySelector, innerKeySelector, resultSelector)
- ParallelEnumerable.GroupJoin(inner, outerKeySelector, innerKeySelector, resultSelector, comparer)
- ParallelEnumerable.Intersect(second)
- ParallelEnumerable.Intersect(second, comparer)
- ParallelEnumerable.Join(inner, outerKeySelector, innerKeySelector, resultSelector)
- ParallelEnumerable.Join(inner, outerKeySelector, innerKeySelector, resultSelector, comparer)
- ParallelEnumerable.Last()
- ParallelEnumerable.Last(predicate)
- ParallelEnumerable.LastOrDefault()
- ParallelEnumerable.LastOrDefault(predicate)
- ParallelEnumerable.LongCount()
- ParallelEnumerable.LongCount(predicate)
- ParallelEnumerable.Max()
- ParallelEnumerable.Max(selector)
- ParallelEnumerable.Min()
- ParallelEnumerable.Min(selector)
- ParallelEnumerable.OfType()
- ParallelEnumerable.OrderBy(keySelector)
- ParallelEnumerable.OrderBy(keySelector, comparer)
- ParallelEnumerable.OrderByDescending(keySelector)
- ParallelEnumerable.OrderByDescending(keySelector, comparer)
- ParallelEnumerable.Range(start, count)
- ParallelEnumerable.Repeat(element, count)
- ParallelEnumerable.Reverse()
- ParallelEnumerable.Select(selector)
- ParallelEnumerable.SelectMany(selector)
- ParallelEnumerable.SelectMany(collectionSelector, resultSelector)
- ParallelEnumerable.SequenceEqual(second)
- ParallelEnumerable.SequenceEqual(second, comparer)
- ParallelEnumerable.Single()
- ParallelEnumerable.Single(predicate)
- ParallelEnumerable.SingleOrDefault()
- ParallelEnumerable.SingleOrDefault(predicate)
- ParallelEnumerable.Skip(count)
- ParallelEnumerable.SkipWhile(predicate)
- ParallelEnumerable.Sum()
- ParallelEnumerable.Sum(selector)
- ParallelEnumerable.Take(count)
- ParallelEnumerable.TakeWhile(predicate)
- ParallelEnumerable.ThenBy(keySelector)
- ParallelEnumerable.ThenBy(keySelector, comparer)
- ParallelEnumerable.ThenByDescending(keySelector)
- ParallelEnumerable.ThenByDescending(keySelector, comparer)
- ParallelEnumerable.ToArray()
- ParallelEnumerable.ToDictionary(keySelector)
- ParallelEnumerable.ToDictionary(keySelector, comparer)
- ParallelEnumerable.ToDictionary(elementSelector)
- ParallelEnumerable.ToDictionary(elementSelector, comparer)
- ParallelEnumerable.ToList()
- ParallelEnumerable.ToLookup(keySelector)
- ParallelEnumerable.ToLookup(keySelector, comparer)
- ParallelEnumerable.ToLookup(keySelector, elementSelector)
- ParallelEnumerable.ToLookup(keySelector, elementSelector, comparer)
- ParallelEnumerable.Union(second)
- ParallelEnumerable.Union(second, comparer)
- ParallelEnumerable.Where(predicate)
- ParallelEnumerable.WithCancellation(cancellationToken)
- ParallelEnumerable.WithDegreeOfParallelism(degreeOfParallelism)
- ParallelEnumerable.WithExecutionMode(executionMode)
- ParallelEnumerable.WithMergeOptions(mergeOptions)
- ParallelEnumerable.Zip(second, resultSelector)

