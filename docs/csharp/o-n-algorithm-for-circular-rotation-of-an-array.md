---
metaTitle: "C# | O(n) Algorithm for circular rotation of an array"
description: "Example of a generic method that rotates an array by a given shift"
---

# O(n) Algorithm for circular rotation of an array


In my path to studying programming there have been simple, but interesting problems to solve as exercises. One of those problems was to rotate an array(or another collection) by a certain value. Here I will share with you a simple formula to do it.



## Example of a generic method that rotates an array by a given shift


I would like to point out that we rotate left when the shifting value is negative and we rotate right when the value is positive.

```cs

   public static void Main()
    {
        int[] array = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
        int shiftCount = 1;
        Rotate(ref array, shiftCount);
        Console.WriteLine(string.Join(", ", array));
        // Output: [10, 1, 2, 3, 4, 5, 6, 7, 8, 9]

        array = new []{ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
        shiftCount = 15;
        Rotate(ref array, shiftCount);
        Console.WriteLine(string.Join(", ", array));
        // Output: [6, 7, 8, 9, 10, 1, 2, 3, 4, 5]

        array = new[] { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
        shiftCount = -1;
        Rotate(ref array, shiftCount);
        Console.WriteLine(string.Join(", ", array));
        // Output: [2, 3, 4, 5, 6, 7, 8, 9, 10, 1]

        array = new[] { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
        shiftCount = -35;
        Rotate(ref array, shiftCount);
        Console.WriteLine(string.Join(", ", array));
        // Output: [6, 7, 8, 9, 10, 1, 2, 3, 4, 5]
    }

    private static void Rotate<T>(ref T[] array, int shiftCount)
    {
        T[] backupArray= new T[array.Length];

        for (int index = 0; index < array.Length; index++)
        {
            backupArray[(index + array.Length + shiftCount % array.Length) % array.Length] = array[index];
        }

        array = backupArray;
    }

```

The thing that is important in this code is the formula with which we find the new index value after the rotation.

**(index + array.Length + shiftCount % array.Length) % array.Length**

Here is a little more information about it:

**(shiftCount % array.Length)** -> we normalize the shifting value to be in the length of the array (since in an array with length 10, shifting 1 or 11 is the same thing, the same goes for -1 and -11).

**array.Length + (shiftCount % array.Length)** -> this is done due to left rotations to make sure we do not go into a negative index, but rotate it to the end of the array. Without it for an array with length 10 for index 0 and a rotation -1 we would go into a negative number (-1) and not get the real rotation index value, which is 9. (10 + (-1 % 10) = 9)

**index + array.Length + (shiftCount % array.Length)** -> not much to say here as we apply the rotation to the index to get the new index. (0 + 10 + (-1 % 10) = 9)

**index + array.Length + (shiftCount % array.Length) % array.Length** -> the second normalization is making sure that the new index value does not go outside of the array, but rotates the value in the beginning of the array. It is for right rotations, since in an array with length 10 without it for index 9 and a rotation 1 we would go into index 10, which is outside of the array, and not get the real rotation index value is 0. ((9 + 10 + (1 % 10)) % 10 = 0)

