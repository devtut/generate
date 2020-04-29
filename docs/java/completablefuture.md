---
metaTitle: "CompletableFuture"
description: "Simple Example of CompletableFuture, Convert blocking method to asynchonous"
---

# CompletableFuture


CompletableFuture is a class added to Java SE 8 which implements the Future interface from Java SE 5. In addition to supporting the Future interface it adds many methods that allow asynchronous callback when the future is completed.



## Simple Example of CompletableFuture


In the example below, the `calculateShippingPrice` method calculates shipping cost, which takes some processing time. In a real world example, this would e.g. be contacting another server which returns the price based on the weight of the product and the shipping method.

By modeling this in an async way via `CompletableFuture`, we can continue different work in the method (i.e. calculating packaging costs).

```java
public static void main(String[] args) {
    int price = 15; // Let's keep it simple and work with whole number prices here
    int weightInGrams = 900;
    
    calculateShippingPrice(weightInGrams) // Here, we get the future
        .thenAccept(shippingPrice -> { // And then immediately work on it!
            // This fluent style is very useful for keeping it concise
            System.out.println("Your total price is: " + (price + shippingPrice));
        });
    System.out.println("Please stand by. We are calculating your total price.");
}


public static CompletableFuture<Integer> calculateShippingPrice(int weightInGrams) {
    return CompletableFuture.supplyAsync(() -> { 
        // supplyAsync is a factory method that turns a given 
        // Supplier<U> into a CompletableFuture<U>

        // Let's just say each 200 grams is a new dollar on your shipping costs
        int shippingCosts = weightInGrams / 200;
        
        try {
            Thread.sleep(2000L); // Now let's simulate some waiting time...
        } catch(InterruptedException e) { /* We can safely ignore that */ }

        return shippingCosts; // And send the costs back!
    });
}

```



## Convert blocking method to asynchonous


The following method will take a second or two depending on your connection to retrieve a web page and count the text length. Whatever thread calls it will block
for that period of time. Also it rethrows an exception which is useful later on.

```java
public static long blockingGetWebPageLength(String urlString) {
    try (BufferedReader br = new BufferedReader(new InputStreamReader(new URL(urlString).openConnection().getInputStream()))) {
        StringBuilder sb = new StringBuilder();
        String line;
        while ((line = br.readLine()) != null) {
            sb.append(line);
        }
        return sb.toString().length();
    } catch (IOException ex) {
        throw new RuntimeException(ex);
    }
}

```

This converts it to a method that will return immediately by moving the blocking method call to another thread. By default the supplyAsync method will run the  supplier on the common pool. For a blocking method this is probably not a good choice since one might exhaust the threads in that pool which is why I added the optional service parameter.

```java
static private ExecutorService service = Executors.newCachedThreadPool();

static public CompletableFuture<Long> asyncGetWebPageLength(String url) {
    return CompletableFuture.supplyAsync(() -> blockingGetWebPageLength(url), service);
}

```

To use the function in an asynchronous fashion one should use on of the methods that accepts a lamda to be called with the result of the of the supplier when it completes such as thenAccept. Also it is important to use exceptionally or handle method to log any exceptions that might have happened.

```java
public static void main(String[] args) {

    asyncGetWebPageLength("https://stackoverflow.com/")
            .thenAccept(l -> {
                System.out.println("Stack Overflow returned " + l);
            })
            .exceptionally((Throwable throwable) -> {
                Logger.getLogger("myclass").log(Level.SEVERE, "", throwable);
                return null;
            });

}

```

