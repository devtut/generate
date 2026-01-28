---
metaTitle: "Java - Random Number Generation"
description: "Pseudo Random Numbers, Pseudo Random Numbers in Specific Range, Generating cryptographically secure pseudorandom numbers, Generating Random Numbers with a Specified Seed, Select random numbers without duplicates, Generating Random number using apache-common lang3"
---

# Random Number Generation




## Pseudo Random Numbers


Java provides, as part of the `utils` package, a basic pseudo-random number generator, appropriately named `Random`. This object can be used to generate a pseudo-random value as any of the built-in numerical datatypes (`int`, `float`, etc). You can also use it to generate a random Boolean value, or a random array of bytes. An example usage is as follows:

```java
import java.util.Random;  

...
  
Random random = new Random();
int randInt = random.nextInt();
long randLong = random.nextLong();

double randDouble = random.nextDouble(); //This returns a value between 0.0 and 1.0
float randFloat = random.nextFloat(); //Same as nextDouble

byte[] randBytes = new byte[16];
random.nextBytes(randBytes); //nextBytes takes a user-supplied byte array, and fills it with random bytes. It returns nothing.

```

NOTE: This class only produces fairly low-quality pseudo-random numbers, and should never be used to generate random numbers for cryptographic operations or other situations where higher-quality randomness is critical (For that, you would want to use the `SecureRandom` class, as noted below). An explanation for the distinction between "secure" and "insecure" randomness is beyond the scope of this example.



## Pseudo Random Numbers in Specific Range


The method `nextInt(int bound)` of `Random` accepts an upper exclusive boundary, i.e. a number that the returned random value must be less than. However, only the `nextInt` method accepts a bound; `nextLong`, `nextDouble` etc. do not.

```java
Random random = new Random();
random.nextInt(1000); // 0 - 999

int number = 10 + random.nextInt(100); // number is in the range of 10 to 109

```

Starting in Java 1.7, you may also use `ThreadLocalRandom` ([source](http://stackoverflow.com/questions/363681/generating-random-integers-in-a-specific-range)). This class provides a thread-safe PRNG (pseudo-random number generator). Note that the `nextInt` method of this class accepts both an upper and lower bound.

```java
import java.util.concurrent.ThreadLocalRandom;

// nextInt is normally exclusive of the top value,
// so add 1 to make it inclusive
ThreadLocalRandom.current().nextInt(min, max + 1);

```

Note that [the official documentation](https://docs.oracle.com/javase/8/docs/api/java/util/Random.html) states that `nextInt(int bound)` can do weird things when `bound` is near 2<sup>30</sup>+1 (emphasis added):

> 
The algorithm is slightly tricky. **It rejects values that would result in an uneven distribution** (due to the fact that 2^31 is not divisible by n). The probability of a value being rejected depends on n. **The worst case is n=2^30+1, for which the probability of a reject is 1/2, and the expected number of iterations before the loop terminates is 2.**


In other words, specifying a bound will (slightly) decrease the performance of the `nextInt` method, and this performance decrease will become more pronounced as the `bound` approaches half the max int value.



## Generating cryptographically secure pseudorandom numbers


`Random` and `ThreadLocalRandom` are good enough for everyday use, but they have a big problem: They are based on a [linear congruential generator](https://en.wikipedia.org/wiki/Linear_congruential_generator), an algorithm whose output can be predicted rather easily. Thus, these two classes are **not** suitable for cryptographic uses (such as key generation).

One can use `java.security.SecureRandom` in situations where a PRNG with an output that is very hard to predict is required. Predicting the random numbers created by instances of this class is hard enough to label the class as **cryptographically secure**.

```java
import java.security.SecureRandom;
import java.util.Arrays;

public class Foo {
    public static void main(String[] args) {
        SecureRandom rng = new SecureRandom();
        byte[] randomBytes = new byte[64];
        rng.nextBytes(randomBytes); // Fills randomBytes with random bytes (duh)
        System.out.println(Arrays.toString(randomBytes));
    }
}

```

Besides being cryptographically secure, `SecureRandom` has a gigantic period of 2<sup>160</sup>, compared to `Random`s period of 2<sup>48</sup>. It has one drawback of being considerably slower than `Random` and other linear PRNGs such as [Mersenne Twister](https://en.wikipedia.org/wiki/Mersenne_Twister) and [Xorshift](https://en.wikipedia.org/wiki/Xorshift), however.

Note that SecureRandom implementation is both platform and provider dependent. The default `SecureRandom` (given by `SUN` provider in `sun.security.provider.SecureRandom`):

- on Unix-like systems, seeded with data from `/dev/random` and/or `/dev/urandom`.
- on Windows, seeded with calls to `CryptGenRandom()` in [CryptoAPI](https://en.wikipedia.org/wiki/Microsoft_CryptoAPI).



## Generating Random Numbers with a Specified Seed


```java
//Creates a Random instance with a seed of 12345.
Random random = new Random(12345L);

//Gets a ThreadLocalRandom instance
ThreadLocalRandom tlr = ThreadLocalRandom.current();

//Set the instance's seed.
tlr.setSeed(12345L);

```

Using the same seed to generate random numbers will return the same numbers every time, so setting a different seed for every `Random` instance is a good idea if you don't want to end up with duplicate numbers.

A good method to get a `Long` that is different for every call is [`System.currentTimeMillis()`](https://docs.oracle.com/javase/7/docs/api/java/lang/System.html#currentTimeMillis()):

```java
Random random = new Random(System.currentTimeMillis());
ThreadLocalRandom.current().setSeed(System.currentTimeMillis());

```



## Select random numbers without duplicates


```java
/**
 * returns a array of random numbers with no duplicates
 * @param range the range of possible numbers for ex. if 100 then it can be anywhere from 1-100
 * @param length the length of the array of random numbers
 * @return array of random numbers with no duplicates.
 */
public static int[] getRandomNumbersWithNoDuplicates(int range, int length){
    if (length<range){
        // this is where all the random numbers
        int[] randomNumbers = new int[length];
        
        // loop through all the random numbers to set them
        for (int q = 0; q < randomNumbers.length; q++){
            
            // get the remaining possible numbers
            int remainingNumbers = range-q;
            
            // get a new random number from the remainingNumbers
            int newRandSpot = (int) (Math.random()*remainingNumbers);
            
            newRandSpot++;
            
            // loop through all the possible numbers
            for (int t = 1; t < range+1; t++){
                
                // check to see if this number has already been taken
                boolean taken = false;
                for (int number : randomNumbers){
                    if (t==number){
                        taken = true;
                        break;
                    }
                }
                
                // if it hasnt been taken then remove one from the spots
                if (!taken){
                    newRandSpot--;
                    
                    // if we have gone though all the spots then set the value
                    if (newRandSpot==0){
                        randomNumbers[q] = t;
                    }
                }
            }
        }
        return randomNumbers;
    } else {
        // invalid can't have a length larger then the range of possible numbers
    }
    return null;
}

```

The method works by looping though an array that has the size of the requested length and finds the remaining length of possible numbers. It sets a random number of those possible numbers `newRandSpot` and finds that number within the non taken number left. It does this by looping through the range and checking to see if that number has already been taken.

For example if the range is 5 and the length is 3 and we have already chosen the number 2. Then we have 4 remaining numbers so we get a random number between 1 and 4 and we loop through the range(5) skipping over any numbers that we have already used(2).

Now let's say the next number chosen between 1 & 4 is 3. On the first loop we get 1 which has not yet been taken so we can remove 1 from 3 making it 2. Now on the second loop we get 2 which has been taken so we do nothing. We follow this pattern until we get to 4 where once we remove 1 it becomes 0 so we set the new randomNumber to 4.



## Generating Random number using apache-common lang3


We can use `org.apache.commons.lang3.RandomUtils` to generate random numbers using a single line.

```java
int x = RandomUtils.nextInt(1, 1000);

```

The method `nextInt(int startInclusive, int endExclusive)` takes a range.

Apart from int, we can generate random `long`, `double`, `float` and `bytes` using this class.

`RandomUtils` class contains the following methods-

```java
static byte[] nextBytes(int count) //Creates an array of random bytes.
static double nextDouble() //Returns a random double within 0 - Double.MAX_VALUE
static double nextDouble(double startInclusive, double endInclusive) //Returns a random double within the specified range.
static float nextFloat() //Returns a random float within 0 - Float.MAX_VALUE
static float nextFloat(float startInclusive, float endInclusive) //Returns a random float within the specified range.
static int nextInt() //Returns a random int within 0 - Integer.MAX_VALUE
static int nextInt(int startInclusive, int endExclusive) //Returns a random integer within the specified range.
static long nextLong() //Returns a random long within 0 - Long.MAX_VALUE
static long nextLong(long startInclusive, long endExclusive) //Returns a random long within the specified range.

```



#### Remarks


Nothing is really random and thus the javadoc calls those numbers pseudorandom. Those numbers are created with a [pseudorandom number generator](https://en.wikipedia.org/wiki/Pseudorandom_number_generator).

