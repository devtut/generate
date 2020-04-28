---
metaTitle: "Unit Testing"
description: "Testing class rules, PHPUnit Data Providers, Test exceptions"
---

# Unit Testing



## Testing class rules


Let's say, we have a simple `LoginForm` class with rules() method (used in login page as framework template):

```
class LoginForm {
    public $email;
    public $rememberMe;
    public $password;

    /* rules() method returns an array with what each field has as a requirement.
     * Login form uses email and password to authenticate user.
     */
    public function rules() {
        return [
            // Email and Password are both required
            [['email', 'password'], 'required'],

            // Email must be in email format
            ['email', 'email'],

            // rememberMe must be a boolean value
            ['rememberMe', 'boolean'],

            // Password must match this pattern (must contain only letters and numbers)
            ['password', 'match', 'pattern' => '/^[a-z0-9]+$/i'],
        ];
    }

    /** the validate function checks for correctness of the passed rules */
    public function validate($rule) {
        $success = true;
        list($var, $type) = $rule;
        foreach ((array) $var as $var) {
            switch ($type) {
                case "required":
                    $success = $success && $this->$var != "";
                    break;
                case "email":
                    $success = $success && filter_var($this->$var, FILTER_VALIDATE_EMAIL);
                    break;
                case "boolean":
                    $success = $success && filter_var($this->$var, FILTER_VALIDATE_BOOLEAN, FILTER_NULL_ON_FAILURE) !== null;
                    break;
                case "match":
                    $success = $success && preg_match($rule["pattern"], $this->$var);
                    break;
                default:
                    throw new \InvalidArgumentException("Invalid filter type passed")
            }
        }
        return $success;
    }
}

```

In order to perform tests on this class, we use **Unit** tests (checking source code to see if it fits our expectations):

```
class LoginFormTest extends TestCase {
    protected $loginForm;

    // Executing code on the start of the test
    public function setUp() {
        $this->loginForm = new LoginForm;
    }

    // To validate our rules, we should use the validate() method

    /**
     * This method belongs to Unit test class LoginFormTest and
     * it's testing rules that are described above.
     */
    public function testRuleValidation() {
        $rules = $this->loginForm->rules();

        // Initialize to valid and test this
        $this->loginForm->email = "valid@email.com";
        $this->loginForm->password = "password";
        $this->loginForm->rememberMe = true;
        $this->assertTrue($this->loginForm->validate($rules), "Should be valid as nothing is invalid");

        // Test email validation
        // Since we made email to be in email format, it cannot be empty
        $this->loginForm->email = '';
        $this->assertFalse($this->loginForm->validate($rules), "Email should not be valid (empty)");

        // It does not contain "@" in string so it's invalid
        $this->loginForm->email = 'invalid.email.com';
        $this->assertFalse($this->loginForm->validate($rules), "Email should not be valid (invalid format)");

        // Revert email to valid for next test
        $this->loginForm->email = 'valid@email.com';

        // Test password validation
        // Password cannot be empty (since it's required)
        $this->loginForm->password = '';
        $this->assertFalse($this->loginForm->validate($rules), "Password should not be valid (empty)");

        // Revert password to valid for next test
        $this->loginForm->password = 'ThisIsMyPassword';

        // Test rememberMe validation
        $this->loginForm->rememberMe = 999;
        $this->assertFalse($this->loginForm->validate($rules), "RememberMe should not be valid (integer type)");

        // Revert remeberMe to valid for next test
        $this->loginForm->rememberMe = true;
    }
}

```

How exactly `Unit` tests can help with (excluding general examples) in here? For example, it fits very well when we get unexpected results. For example, let's take this rule from earlier:

```
['password', 'match', 'pattern' => '/^[a-z0-9]+$/i'],

```

Instead, if we missed one important thing and wrote this:

```
['password', 'match', 'pattern' => '/^[a-z0-9]$/i'],

```

With dozens of different rules (assuming we are using not just email and password), it's difficult to detect mistakes. This unit test:

```
// Initialize to valid and test this
$this->loginForm->email = "valid@email.com";
$this->loginForm->password = "password";
$this->loginForm->rememberMe = true;
$this->assertTrue($this->loginForm->validate($rules), "Should be valid as nothing is invalid");

```

Will pass our **first** example but not **second**. Why? Because in 2nd example we wrote a pattern with a typo (missed `+` sign), meaning it only accepts one letter/number.

**Unit** tests can be run in console with command: `phpunit [path_to_file]`. If everything is OK, we should be able to see that all tests are in `OK` state, else we will see either `Error` (syntax errors) or `Fail` (at least one line in that method did not pass).

With additional parameters like `--coverage` we can also see visually how many lines in backend code were tested and which passed/failed. This applies to any framework that has installed [PHPUnit](https://phpunit.de/).

Example how `PHPUnit` test looks like in console (general look, not according to this example):

[<img src="https://i.stack.imgur.com/9za6b.png" alt="enter image description here" />](https://i.stack.imgur.com/9za6b.png)



## PHPUnit Data Providers


Test methods often need data to be tested with. To test some methods completely you need to provide different data sets for every possible test condition. Of course, you can do it manually using loops, like this:

```
...
public function testSomething()
{
    $data = [...];
    foreach($data as $dataSet) {
       $this->assertSomething($dataSet);
    }
}
... 

```

And someone can find it convenient. But there are some drawbacks of this approach. First, you'll have to perform additional actions to extract data if your test function accepts several parameters. Second, on failure it would be difficult to distinguish the failing data set without additional messages and debugging. Third, PHPUnit provides automatic way to deal with test data sets using [data providers](https://phpunit.de/manual/current/en/writing-tests-for-phpunit.html#writing-tests-for-phpunit.data-providers).

Data provider is a function, that should return data for your particular test case.

> 
A data provider method must be public and either return an **array of arrays** or an object that implements the **Iterator** interface and **yields an array** for each iteration step. For each array that is part of the collection the test method will be called with the contents of the array as its arguments.


To use a data provider with your test, use `@dataProvider` annotation with the name of data provider function specified:

```
/**
* @dataProvider dataProviderForTest
*/
public function testEquals($a, $b)
{
    $this->assertEquals($a, $b);
}

public function dataProviderForTest()
{
    return [
        [1,1],
        [2,2],
        [3,2] //this will fail
    ];
}

```

### Array of arrays

> 
Note that `dataProviderForTest()` returns array of arrays. Each nested array has two elements and they will fill necessary parameters for `testEquals()` one by one. Error like this will be thrown `Missing argument 2 for Test::testEquals()` if there are not enough elements. PHPUnit will automatically loop through data and run tests:


```
public function dataProviderForTest()
{
    return [
        [1,1], // [0] testEquals($a = 1, $b = 1)
        [2,2], // [1] testEquals($a = 2, $b = 2)
        [3,2]  // [2] There was 1 failure: 1) Test::testEquals with data set #2 (3, 4)
    ];
}

```

Each data set can be **named** for convenience. It will be easier to detect failing data:

```
public function dataProviderForTest()
{
    return [
        'Test 1' => [1,1], // [0] testEquals($a = 1, $b = 1)
        'Test 2' => [2,2], // [1] testEquals($a = 2, $b = 2)
        'Test 3' => [3,2]  // [2] There was 1 failure: 
                           //     1) Test::testEquals with data set "Test 3" (3, 4)
    ];
}

```

### Iterators

```
class MyIterator implements Iterator {
    protected $array = [];

    public function __construct($array) {
        $this->array = $array;
    }

    function rewind() {
        return reset($this->array);
    }

    function current() {
        return current($this->array);
    }

    function key() {
        return key($this->array);
    }

    function next() {
        return next($this->array);
    }

    function valid() {
        return key($this->array) !== null;
    }
}
...

class Test extends TestCase
{
    /**
     * @dataProvider dataProviderForTest
     */
    public function testEquals($a)
    {
        $toCompare = 0;

        $this->assertEquals($a, $toCompare);
    }

    public function dataProviderForTest()
    {
        return new MyIterator([
            'Test 1' => [0],
            'Test 2' => [false],
            'Test 3' => [null]
        ]);
    }
}

```

As you can see, simple iterator also works.

> 
Note that even for a **single** parameter, data provider must return an array `[$parameter]`


Because if we change our `current()` method (which actually return data on every iteration) to this:

```
function current() {
    return current($this->array)[0];
}

```

Or change actual data:

```
return new MyIterator([
            'Test 1' => 0,
            'Test 2' => false,
            'Test 3' => null
        ]);

```

We'll get an error:

```
There was 1 warning:

1) Warning
The data provider specified for Test::testEquals is invalid.

```

> 
<p>Of course, it is not useful to use `Iterator` object over a simple
array. It should implement some specific logic for your case.</p>


### Generators

It is not explicitly noted and shown in manual, but you can also use a [generator](http://stackoverflow.com/documentation/php/1684/generators#t=201607260816011891273) as data provider. Note that `Generator` class actually implements `Iterator` interface.

So here's an example of using `DirectoryIterator` combined with `generator`:

```
/**
 * @param string $file
 *
 * @dataProvider fileDataProvider
 */
public function testSomethingWithFiles($fileName)
{
    //$fileName is available here
    
    //do test here
}

public function fileDataProvider()
{
    $directory = new DirectoryIterator('path-to-the-directory');

    foreach ($directory as $file) {
        if ($file->isFile() && $file->isReadable()) {
            yield [$file->getPathname()]; // invoke generator here.
        }
    }
}

```

> 
Note provider `yield`s an array. You'll get an invalid-data-provider warning instead.




## Test exceptions


Let's say you want to test method which throws an exception

```
class Car
{
    /**
     * @throws \Exception
     */
    public function drive()
    {
        throw new \Exception('Useful message', 1);
    }
}

```

You can do that by enclosing the method call into a try/catch block and making assertions on execption object's properties, but more conveniently you can use exception assertion methods. As of [PHPUnit 5.2](https://github.com/sebastianbergmann/phpunit/wiki/Release-Announcement-for-PHPUnit-5.2.0) you have expectX() methods available for asserting exception type, message & code

```
class DriveTest extends PHPUnit_Framework_TestCase
{
    public function testDrive()
    {
        // prepare
        $car = new \Car();
        $expectedClass = \Exception::class;
        $expectedMessage = 'Useful message';
        $expectedCode = 1;

        // test
        $this->expectException($expectedClass);
        $this->expectMessage($expectedMessage);
        $this->expectCode($expectedCode);

        // invoke
        $car->drive();
    }
}

```

If you are using earlier version of PHPUnit, method setExpectedException can be used in stead of expectX() methods, but keep in mind that it's deprecated and will be removed in version 6.

```
class DriveTest extends PHPUnit_Framework_TestCase
{
    public function testDrive()
    {
        // prepare
        $car = new \Car();
        $expectedClass = \Exception::class;
        $expectedMessage = 'Useful message';
        $expectedCode = 1;

        // test
        $this->setExpectedException($expectedClass, $expectedMessage, $expectedCode);

        // invoke
        $car->drive();
    }
}

```



#### Syntax


- [Complete list of assertions](https://phpunit.de/manual/current/en/phpunit-book.html#appendixes.assertions). Examples:
- `assertTrue(bool $condition[, string $messageIfFalse = '']);`
- `assertEquals(mixed $expected, mixed $actual[, string $messageIfNotEqual = '']);`



#### Remarks


`Unit` tests are used for testing source code to see if it contains deals with inputs as we expect. `Unit` tests are supported by the majority of frameworks. There are several different [PHPUnit tests](https://en.wikipedia.org/wiki/List_of_unit_testing_frameworks#PHP) and they might differ in syntax. In this example we are using `PHPUnit`.

