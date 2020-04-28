---
metaTitle: "Test Doubles (Mocks and Stubs)"
description: "Simple mocking, Simple stubbing"
---

# Test Doubles (Mocks and Stubs)



## Simple mocking


### Introduction

The [PHPUnit Manual](https://phpunit.de/manual/current/en/test-doubles.html#test-doubles.mock-objects) describes mocking as such:

> 
<p>The practice of replacing an object with a test double that verifies
expectations, for instance asserting that a method has been called, is
referred to as mocking.</p>


So instead of stubbing out code, an observer is created that not only replaces the code that needs to be silenced, but observes that a specific activity would have happened in the real world.

### Setup

Let's start with a simple logger class that for the sake of clarity, simply displays the text sent into the parameter (normally it would do something that can be problematic to unit testing, such as updating a database):

```php
class Logger {
    public function log($text) {
        echo $text;
    }
}

```

Now, let's create an Application class.  It accepts a Logger object as a parameter to the **run** method, which in turn invokes the Logger's **log** method to capture that the application has started.

```php
class Application {
  public function run(Logger $logger) {
    // some code that starts up the application

    // send out a log that the application has started
    $logger->log('Application has started');
  }
}

```

If the following code was executed as written:

```php
$logger = new Logger();
$app = new Application();
$app->run($logger);

```

Then the text **"Application has started"** would be displayed as per the **log** method inside of the Logger.

### Unit Testing with Mocking

The Application class unit testing does not need to verify what happens within the Logger **log** method, it only needs to verify that it was called.

In the PHPUnit test, an observer is created to replace the Logger class.  That observer is set up to ensure that the **log** method is invoked only once, with the parameter value **"Application has started"**.

Then, the observer is sent into the run method, which verifies that in fact the log method was called just once and the test case passes, but no text was displayed.

```php
class ApplicationTest extends \PHPUnit_Framework_TestCase {

  public function testThatRunLogsApplicationStart() {

    // create the observer
    $mock = $this->createMock(Logger::class);
    $mock->expects($this->once())
        ->method('log')
        ->with('Application has started');
    
    // run the application with the observer which ensures the log method was called
    $app = new Application();
    $app->run($mock);
  
  }
}

```



## Simple stubbing


Sometimes there are sections of code that are difficult to test, such as accessing a database, or interacting with the user.  You can stub out those sections of code, allowing the rest of the code to be tested.

Let's start with a class that prompts the user.  For simplicity, it has only two methods, one that actually prompts the user (which would be used by all the other methods) and the one we are going to test, which prompts and filters out only yes and no answers.  Please note this code is overly simplistic for demonstration purposes.

```php
class Answer
{
    // prompt the user and check if the answer is yes or no, anything else, return null
    public function getYesNoAnswer($prompt) {

        $answer = $this->readUserInput($prompt);

        $answer = strtolower($answer);
        if (($answer === "yes") || ($answer === "no")) {
            return $answer;
        } else {
            return null;
        }

    }

    // Simply prompt the user and return the answer
    public function readUserInput($prompt) {
        return readline($prompt);
    }

}

```

To test `getYesNoAnswer`, the `readUserInput` needs to be stubbed out to mimic answers from a user.

```php
class AnswerTest extends PHPUnit_Framework_TestCase
{

    public function test_yes_no_answer() {

        $stub = $this->getMockBuilder(Answer::class)
                ->setMethods(["readUserInput"])
                ->getMock();

        $stub->method('readUserInput')
            ->will($this->onConsecutiveCalls("yes","junk"));

        // stub will return "yes"
        $answer = $stub->getYesNoAnswer("Student? (yes/no)");
        $this->assertSame("yes",$answer);

        // stub will return "junk"
        $answer = $stub->getYesNoAnswer("Student? (yes/no)");
        $this->assertNull($answer);


    }

}

```

The first line of code creates the stub and it uses `getMockBuilder` instead of `createMock`.  `createMock` is a shortcut for calling `getMockBuilder` with defaults.  One of these defaults is to stub out all the methods.  For this example, we want to test `getYesNoAnswer`, so it can't be stubbed out.  The `getMockBuilder` invokes `setMethods` to request that only `readUserInput` be stubbed out.

The second line of code creates the stubbing behavior.  It stubs out `readUserInput` method and sets up two return values upon subsequent calls, "yes" followed by "junk".

The third and fourth lines of code test `getYesNoAnswer`. The first time, the fake person responds with "yes" and the tested code correctly returns "yes", since it is a valid selection.  The second time, the fake person responds with "junk" and the tested code correctly returns null.

