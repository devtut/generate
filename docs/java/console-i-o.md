---
metaTitle: "Java - Console I/O"
description: "Reading user input from the console, Implementing Basic Command-Line Behavior, Aligning strings in console"
---

# Console I/O



## Reading user input from the console


### Using `BufferedReader`:

```java
System.out.println("Please type your name and press Enter.");

BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
try {
    String name = reader.readLine();
    System.out.println("Hello, " + name + "!");
} catch(IOException e) {
    System.out.println("An error occurred: " + e.getMessage());
}

```

The following imports are needed for this code:

```java
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

```

### Using `Scanner`:

```java
System.out.println("Please type your name and press Enter");

Scanner scanner = new Scanner(System.in);
String name = scanner.nextLine();

System.out.println("Hello, " + name + "!");

```

The following import is needed for this example:

```java
import java.util.Scanner;

```

To read more than one line, invoke `scanner.nextLine()` repeatedly:

```java
System.out.println("Please enter your first and your last name, on separate lines.");
    
Scanner scanner = new Scanner(System.in);
String firstName = scanner.nextLine();
String lastName = scanner.nextLine();
    
System.out.println("Hello, " + firstName + " " + lastName + "!");

```

There are two methods for obtaining `Strings`, `next()` and `nextLine()`. `next()` returns text up until the first space (also known as a "token"), and `nextLine()` returns all text that the user inputted until pressing enter.

`Scanner` also provides utility methods for reading data types other than `String`. These include:

```java
scanner.nextByte();
scanner.nextShort();
scanner.nextInt();
scanner.nextLong();
scanner.nextFloat();
scanner.nextDouble();
scanner.nextBigInteger();
scanner.nextBigDecimal();

```

Prefixing any of these methods with `has` (as in `hasNextLine()`, `hasNextInt()`) returns `true` if the stream has any more of the request type. Note: These methods will crash the program if the input is not of the requested type (for example, typing "a" for `nextInt()` ). You can use a `try {} catch() {}` to prevent this (see: [Exceptions](https://stackoverflow.com/documentation/java/89/exceptions))

```java
Scanner scanner = new Scanner(System.in); //Create the scanner
scanner.useLocale(Locale.US); //Set number format excepted
System.out.println("Please input a float, decimal separator is .");
if (scanner.hasNextFloat()){ //Check if it is a float
    float fValue = scanner.nextFloat(); //retrive the value directly as float
    System.out.println(fValue + " is a float");
}else{
    String sValue = scanner.next(); //We can not retrive as float
    System.out.println(sValue + " is not a float");
}

```

### Using `System.console`:

```java
String name = System.console().readLine("Please type your name and press Enter%n");

System.out.printf("Hello, %s!", name);

//To read passwords (without echoing as in unix terminal)
char[] password = System.console().readPassword();

```

**Advantages**:

- Reading methods are synchronized
- Format string syntax can be used

**Note**: This will only work if the program is run from a real command line without redirecting the standard input and output streams. It does not work when the program is run from within certain IDEs, such as Eclipse. For code that works within IDEs and with stream redirection, see the other examples.



## Implementing Basic Command-Line Behavior


For basic prototypes or basic command-line behavior, the following loop comes in handy.

```java
public class ExampleCli {

    private static final String CLI_LINE   = "example-cli>"; //console like string

    private static final String CMD_QUIT   = "quit";    //string for exiting the program
    private static final String CMD_HELLO  = "hello";    //string for printing "Hello World!" on the screen
    private static final String CMD_ANSWER = "answer";    //string for printing 42 on the screen

    public static void main(String[] args) {
        ExampleCli claimCli = new ExampleCli();    // creates an object of this class

        try {
            claimCli.start();    //calls the start function to do the work like console
        }
        catch (IOException e) {
            e.printStackTrace();    //prints the exception log if it is failed to do get the user input or something like that
        }
    }

    private void start() throws IOException {
        String cmd = "";
        
        BufferedReader reader = new BufferedReader(new InputStreamReader(System.in));
        while (!cmd.equals(CMD_QUIT)) {    // terminates console if user input is "quit"
            System.out.print(CLI_LINE);    //prints the console-like string 

            cmd = reader.readLine();    //takes input from user. user input should be started with "hello",  "answer" or "quit"
            String[] cmdArr = cmd.split(" ");

            if (cmdArr[0].equals(CMD_HELLO)) {    //executes when user input starts with "hello"
                hello(cmdArr);
            }
            else if (cmdArr[0].equals(CMD_ANSWER)) {    //executes when user input starts with "answer"
                answer(cmdArr);
            }
        }
    }
    
    // prints "Hello World!" on the screen if user input starts with "hello"
    private void hello(String[] cmdArr) {
        System.out.println("Hello World!");
    }
    
    // prints "42" on the screen if user input starts with "answer"
    private void answer(String[] cmdArr) {
        System.out.println("42");
    }
}

```



## Aligning strings in console


The method [`PrintWriter.format`](https://docs.oracle.com/javase/8/docs/api/java/io/PrintWriter.html#format-java.lang.String-java.lang.Object...-) (called through `System.out.format`) can be used to print aligned strings in console. The method receives a `String` with the format information and a series of objects to format:

```java
String rowsStrings[] = new String[] {"1", 
                                     "1234", 
                                     "1234567", 
                                     "123456789"};

String column1Format = "%-3s";    // min 3 characters, left aligned
String column2Format = "%-5.8s";  // min 5 and max 8 characters, left aligned
String column3Format = "%6.6s";   // fixed size 6 characters, right aligned
String formatInfo = column1Format + " " + column2Format + " " + column3Format;

for(int i = 0; i < rowsStrings.length; i++) {
    System.out.format(formatInfo, rowsStrings[i], rowsStrings[i], rowsStrings[i]);
    System.out.println();
}

```

**Output:**

```java
1   1          1
1234 1234    1234
1234567 1234567 123456
123456789 12345678 123456

```

Using format strings with fixed size permits to print the strings in a table-like appearance with fixed size columns:

```java
String rowsStrings[] = new String[] {"1", 
                                     "1234", 
                                     "1234567", 
                                     "123456789"};

String column1Format = "%-3.3s";  // fixed size 3 characters, left aligned
String column2Format = "%-8.8s";  // fixed size 8 characters, left aligned
String column3Format = "%6.6s";   // fixed size 6 characters, right aligned
String formatInfo = column1Format + " " + column2Format + " " + column3Format;

for(int i = 0; i < rowsStrings.length; i++) {
    System.out.format(formatInfo, rowsStrings[i], rowsStrings[i], rowsStrings[i]);
    System.out.println();
}

```

**Output:**

```java
1   1             1
123 1234       1234
123 1234567  123456
123 12345678 123456

```

### Format strings examples

- `%s`: just a string with no formatting
- `%5s`: format the string with a **minimum** of 5 characters; if the string is shorter it will be **padded** to 5 characters and **right** aligned
- `%-5s`: format the string with a **minimum** of 5 characters; if the string is shorter it will be **padded** to 5 characters and **left** aligned
- `%5.10s`: format the string with a **minimum** of 5 characters and a **maximum** of 10 characters; if the string is shorter than 5 it will be **padded** to 5 characters and **right** aligned; if the string is longer than 10 it will be **truncated** to 10 characters and **right** aligned
- `%-5.5s`: format the string with a **fixed** size of 5 characters (minimum and maximum are equals); if the string is shorter than 5 it will be **padded** to 5 characters and **left** aligned; if the string is longer than 5 it will be **truncated** to 5 characters and **left** aligned

