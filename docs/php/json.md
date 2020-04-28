---
metaTitle: "JSON"
description: "Decoding a JSON string, Encoding a JSON string, Debugging JSON errors, Using JsonSerializable in an Object, Header json and the returned response"
---

# JSON


[JSON](http://www.json.org) ([JavaScript Object Notation](https://en.wikipedia.org/wiki/JSON)) is a platform and language independent way of serializing objects into plaintext. Because it is often used on web and so is PHP, there is a [basic extension](https://secure.php.net/manual/en/book.json.php) for working with JSON in PHP.



## Decoding a JSON string


The [`json_decode()`](http://php.net/manual/en/function.json-decode.php) function takes a JSON-encoded string as its first parameter and parses it into a PHP variable.

Normally, `json_decode()` will return an **object of [\stdClass](http://php.net/manual/en/reserved.classes.php)** if the top level item in the JSON object is a dictionary or an **indexed array** if the JSON object is an array. It will also return scalar values or `NULL` for certain scalar values, such as simple strings, `"true"`, `"false"`, and `"null"`. It also returns `NULL` on any error.

```
// Returns an object (The top level item in the JSON string is a JSON dictionary)
$json_string = '{"name": "Jeff", "age": 20, "active": true, "colors": ["red", "blue"]}';
$object = json_decode($json_string);
printf('Hello %s, You are %s years old.', $object->name, $object->age);
#> Hello Jeff, You are 20 years old.

// Returns an array (The top level item in the JSON string is a JSON array)
$json_string = '["Jeff", 20, true, ["red", "blue"]]';
$array = json_decode($json_string);
printf('Hello %s, You are %s years old.', $array[0], $array[1]);

```

Use [`var_dump()`](http://php.net/manual/en/function.var-dump.php) to view the types and values of each property on the object we decoded above.

```
// Dump our above $object to view how it was decoded
var_dump($object);

```

Output (note the variable types):

```
class stdClass#2 (4) {
 ["name"] => string(4) "Jeff"
 ["age"] => int(20)
 ["active"] => bool(true)
 ["colors"] =>
   array(2) {
     [0] => string(3) "red"
     [1] => string(4) "blue"
   }
}

```

**Note:** The variable **types** in JSON were converted to their PHP equivalent.

To return an [associative array](http://php.net/manual/en/language.types.array.php) for JSON objects instead of returning an object, pass `true` as the [second parameter](http://php.net/manual/en/function.json-decode.php#refsect1-function.json-decode-parameters) to `json_decode()`.

```
$json_string = '{"name": "Jeff", "age": 20, "active": true, "colors": ["red", "blue"]}';
$array = json_decode($json_string, true); // Note the second parameter
var_dump($array);

```

Output (note the array associative structure):

```
array(4) {
  ["name"] => string(4) "Jeff"
  ["age"] => int(20)
  ["active"] => bool(true)
  ["colors"] =>
  array(2) {
    [0] => string(3) "red"
    [1] => string(4) "blue"
  }
}

```

The second parameter (`$assoc`) has no effect if the variable to be returned is not an object.

**Note:** If you use the `$assoc` parameter, you will lose the distinction between an empty array and an empty object. This means that running `json_encode()` on your decoded output again, will result in a different JSON structure.

If the JSON string has a "depth" more than 512 elements (**20 elements in versions older than 5.2.3, or 128 in version 5.2.3**) in recursion, the function `json_decode()` returns `NULL`. In versions 5.3 or later, this limit can be controlled using the third parameter (`$depth`), as discussed below.

According to the manual:

> 
<p>PHP implements a superset of JSON as specified in the original [» RFC 4627](http://www.faqs.org/rfcs/rfc4627) - it will also encode and decode scalar types and NULL. RFC 4627 only supports these values when they are nested inside an array or an object.
Although this superset is consistent with the expanded definition of "JSON text" in the newer [» RFC 7159](http://www.faqs.org/rfcs/rfc7159) (which aims to supersede RFC 4627) and [» ECMA-404](http://www.ecma-international.org/publications/files/ECMA-ST/ECMA-404.pdf), this may cause interoperability issues with older JSON parsers that adhere strictly to RFC 4627 when encoding a single scalar value.</p>


This means, that, for example, a simple string will be considered to be a valid JSON object in PHP:

```
$json = json_decode('"some string"', true);
var_dump($json, json_last_error_msg());

```

Output:

```
string(11) "some string"
string(8) "No error"

```

But simple strings, not in an array or object, are not part of the [RFC 4627](http://www.faqs.org/rfcs/rfc4627) standard. As a result, such online checkers as [JSLint](http://www.jslint.com/), [JSON Formatter & Validator](https://jsonformatter.curiousconcept.com/) (in RFC 4627 mode) will give you an error.

There is a third `$depth` parameter for the depth of recursion (the default value is `512`), which means the amount of nested objects inside the original object to be decoded.

There is a fourth `$options` parameter. It currently accepts only one value, `JSON_BIGINT_AS_STRING`.  The default behavior (which leaves off this option) is to cast large integers to floats instead of strings.

> 
<p>Invalid non-lowercased variants of the true, false and null literals
are no longer accepted as valid input.</p>


So this example:

```
var_dump(json_decode('tRue'), json_last_error_msg());
var_dump(json_decode('tRUe'), json_last_error_msg());
var_dump(json_decode('tRUE'), json_last_error_msg());
var_dump(json_decode('TRUe'), json_last_error_msg());
var_dump(json_decode('TRUE'), json_last_error_msg());
var_dump(json_decode('true'), json_last_error_msg());

```

Before PHP 5.6:

```
bool(true)
string(8) "No error"
bool(true)
string(8) "No error"
bool(true)
string(8) "No error"
bool(true)
string(8) "No error"
bool(true)
string(8) "No error"
bool(true)
string(8) "No error"

```

And after:

```
NULL
string(12) "Syntax error"
NULL
string(12) "Syntax error"
NULL
string(12) "Syntax error"
NULL
string(12) "Syntax error"
NULL
string(12) "Syntax error"
bool(true)
string(8) "No error"

```

Similar behavior occurs for `false` and `null`.

Note that `json_decode()` will return `NULL` if the string cannot be converted.

```
$json = "{'name': 'Jeff', 'age': 20 }" ;  // invalid json 

$person = json_decode($json);
echo $person->name;    //  Notice: Trying to get property of non-object: returns null
echo json_last_error();     
#  4 (JSON_ERROR_SYNTAX)
echo json_last_error_msg(); 
#  unexpected character 

```

It is not safe to rely only on the return value being `NULL` to detect errors. For example, if the JSON string contains nothing but `"null"`, `json_decode()` will return `null`, even though no error occurred.



## Encoding a JSON string


The [`json_encode`](http://php.net/manual/en/function.json-encode.php) function will convert a PHP array (or, since PHP 5.4, an object which implements the `JsonSerializable` interface) to a JSON-encoded string.  It returns a JSON-encoded string on success or FALSE on failure.

```
$array = [
    'name' => 'Jeff',
    'age' => 20,
    'active' => true,
    'colors' => ['red', 'blue'],
    'values' => [0=>'foo', 3=>'bar'],
];

```

During encoding, the PHP data types string, integer, and boolean are converted to their JSON equivalent. Associative arrays are encoded as JSON objects, and – when called with default arguments – indexed arrays are encoded as JSON arrays. (Unless the array keys are not a continuous numeric sequence starting from 0, in which case the array will be encoded as a JSON object.)

```
echo json_encode($array);

```

Output:

```
{"name":"Jeff","age":20,"active":true,"colors":["red","blue"],"values":{"0":"foo","3":"bar"}}

```

### Arguments

Since PHP 5.3, the second argument to `json_encode` is a bitmask which can be one or more of the following.

As with any bitmask, they can be combined with the binary OR operator `|`.

### [`JSON_FORCE_OBJECT`](http://php.net/manual/en/json.constants.php#constant.json-force-object)

Forces the creation of an object instead of an array

```
$array = ['Joel', 23, true, ['red', 'blue']];
echo json_encode($array);
echo json_encode($array, JSON_FORCE_OBJECT);

```

Output:

```
["Joel",23,true,["red","blue"]]
{"0":"Joel","1":23,"2":true,"3":{"0":"red","1":"blue"}}

```

### [`JSON_HEX_TAG`](http://php.net/manual/en/json.constants.php#constant.json-hex-tag), [`JSON_HEX_AMP`](http://php.net/manual/en/json.constants.php#constant.json-hex-amp), [`JSON_HEX_APOS`](http://php.net/manual/en/json.constants.php#constant.json-hex-apos), [`JSON_HEX_QUOT`](http://php.net/manual/en/json.constants.php#constant.json-hex-quot)

Ensures the following conversions during encoding:

|Constant|Input|Output
|------
|`JSON_HEX_TAG`|`<`|`\u003C`
|`JSON_HEX_TAG`|`>`|`\u003E`
|`JSON_HEX_AMP`|`&`|`\u0026`
|`JSON_HEX_APOS`|`'`|`\u0027`
|`JSON_HEX_QUOT`|`"`|`\u0022`

```
$array = ["tag"=>"<>", "amp"=>"&", "apos"=>"'", "quot"=>"\""];
echo json_encode($array);
echo json_encode($array, JSON_HEX_TAG | JSON_HEX_AMP | JSON_HEX_APOS | JSON_HEX_QUOT);

```

Output:

```
{"tag":"<>","amp":"&","apos":"'","quot":"\""}
{"tag":"\u003C\u003E","amp":"\u0026","apos":"\u0027","quot":"\u0022"}

```

### [`JSON_NUMERIC_CHECK`](http://php.net/manual/en/json.constants.php#constant.json-numeric-check)

Ensures numeric strings are converted to integers.

```
$array = ['23452', 23452];
echo json_encode($array);
echo json_encode($array, JSON_NUMERIC_CHECK);

```

Output:

```
["23452",23452]    
[23452,23452]

```

### [`JSON_PRETTY_PRINT`](http://php.net/manual/en/json.constants.php#constant.json-pretty-print)

Makes the JSON easily readable

```
$array = ['a' => 1, 'b' => 2, 'c' => 3, 'd' => 4];
echo json_encode($array);
echo json_encode($array, JSON_PRETTY_PRINT);

```

Output:

```
{"a":1,"b":2,"c":3,"d":4}
{
    "a": 1,
    "b": 2,
    "c": 3,
    "d": 4
}

```

### [`JSON_UNESCAPED_SLASHES`](http://php.net/manual/en/json.constants.php#constant.json-unescaped-slashes)

Includes unescaped `/` forward slashes in the output

```
$array = ['filename' => 'example.txt', 'path' => '/full/path/to/file/'];
echo json_encode($array);
echo json_encode($array, JSON_UNESCAPED_SLASHES);

```

Output:

```
{"filename":"example.txt","path":"\/full\/path\/to\/file"}
{"filename":"example.txt","path":"/full/path/to/file"}

```

### [`JSON_UNESCAPED_UNICODE`](http://php.net/manual/en/json.constants.php#constant.json-unescaped-unicode)

Includes UTF8-encoded characters in the output instead of `\u`-encoded strings

```
$blues = ["english"=>"blue", "norwegian"=>"blå", "german"=>"blau"];
echo json_encode($blues);
echo json_encode($blues, JSON_UNESCAPED_UNICODE);

```

Output:

```
{"english":"blue","norwegian":"bl\u00e5","german":"blau"}
{"english":"blue","norwegian":"blå","german":"blau"}

```

### [`JSON_PARTIAL_OUTPUT_ON_ERROR`](http://php.net/manual/en/json.constants.php#constant.json-partial-output-on-error)

Allows encoding to continue if some unencodable values are encountered.

```
$fp = fopen("foo.txt", "r");
$array = ["file"=>$fp, "name"=>"foo.txt"];
echo json_encode($array); // no output
echo json_encode($array, JSON_PARTIAL_OUTPUT_ON_ERROR);

```

Output:

```
{"file":null,"name":"foo.txt"}

```

### [`JSON_PRESERVE_ZERO_FRACTION`](http://php.net/manual/en/json.constants.php#constant.json-preserve-zero-fraction)

Ensures that floats are always encoded as floats.

```
$array = [5.0, 5.5];
echo json_encode($array);
echo json_encode($array, JSON_PRESERVE_ZERO_FRACTION);

```

Output:

```
[5,5.5]
[5.0,5.5]

```

### [`JSON_UNESCAPED_LINE_TERMINATORS`](http://php.net/manual/en/json.constants.php#constant.json-unescaped-line-terminators)

When used with `JSON_UNESCAPED_UNICODE`, reverts to the behaviour of older PHP versions, and **does not** escape the characters U+2028 LINE SEPARATOR and U+2029 PARAGRAPH SEPARATOR. Although valid in JSON, these characters are not valid in JavaScript, so the default behaviour of `JSON_UNESCAPED_UNICODE` was changed in version 7.1.

```
$array = ["line"=>"\xe2\x80\xa8", "paragraph"=>"\xe2\x80\xa9"];
echo json_encode($array, JSON_UNESCAPED_UNICODE);
echo json_encode($array, JSON_UNESCAPED_UNICODE | JSON_UNESCAPED_LINE_TERMINATORS);

```

Output:

```
{"line":"\u2028","paragraph":"\u2029"}
{"line":" ","paragraph":" "}

```



## Debugging JSON errors


When `json_encode` or `json_decode` fails to parse the string provided, it will return `false`. PHP itself will not raise any errors or warnings when this happens, the onus is on the user to use the [json_last_error()](http://php.net/manual/en/function.json-last-error.php) and [json_last_error_msg()](http://php.net/manual/en/function.json-last-error-msg.php) functions to check if an error occurred and act accordingly in your application (debug it, show an error message, etc.).

The following example shows a common error when working with JSON, a failure to decode/encode a JSON string **(due to the passing of a bad UTF-8 encoded string, for example)**.

```
// An incorrectly formed JSON string
$jsonString = json_encode("{'Bad JSON':\xB1\x31}");

if (json_last_error() != JSON_ERROR_NONE) {
    printf("JSON Error: %s", json_last_error_msg());
}

#> JSON Error: Malformed UTF-8 characters, possibly incorrectly encoded

```

### [json_last_error_msg](http://php.net/manual/en/function.json-last-error-msg.php)

[`json_last_error_msg()`](http://php.net/manual/en/function.json-last-error-msg.php) returns a human readable message of the last error that occurred when trying to encode/decode a string.

<li>This function will **always return a string**, even if no error occurred.<br />
The default **non-error** string is `No Error`</li>
- It will return `false` if some other (unknown) error occurred
- Careful when using this in loops, as [json_last_error_msg](http://php.net/manual/en/function.json-last-error-msg.php) will be overridden on each iteration.

You should only use this function to get the message for display, **not** to test against in control statements.

```
// Don't do this:
if (json_last_error_msg()){} // always true (it's a string)
if (json_last_error_msg() != "No Error"){} // Bad practice

// Do this: (test the integer against one of the pre-defined constants)
if (json_last_error() != JSON_ERROR_NONE) {
    // Use json_last_error_msg to display the message only, (not test against it)
    printf("JSON Error: %s", json_last_error_msg());
}

```

This function doesn't exist before PHP 5.5. Here is a polyfill implementation:

```
if (!function_exists('json_last_error_msg')) {
    function json_last_error_msg() {
        static $ERRORS = array(
            JSON_ERROR_NONE => 'No error',
            JSON_ERROR_DEPTH => 'Maximum stack depth exceeded',
            JSON_ERROR_STATE_MISMATCH => 'State mismatch (invalid or malformed JSON)',
            JSON_ERROR_CTRL_CHAR => 'Control character error, possibly incorrectly encoded',
            JSON_ERROR_SYNTAX => 'Syntax error',
            JSON_ERROR_UTF8 => 'Malformed UTF-8 characters, possibly incorrectly encoded'
        );

        $error = json_last_error();
        return isset($ERRORS[$error]) ? $ERRORS[$error] : 'Unknown error';
    }
}

```

### [json_last_error](http://php.net/manual/en/function.json-last-error.php)

[`json_last_error()`](http://php.net/manual/en/function.json-last-error.php) returns an **integer** mapped to one of the pre-defined constants provided by PHP.

|Constant|Meaning
|------
|`JSON_ERROR_NONE`|No error has occurred
|`JSON_ERROR_DEPTH`|The maximum stack depth has been exceeded
|`JSON_ERROR_STATE_MISMATCH`|Invalid or malformed JSON
|`JSON_ERROR_CTRL_CHAR`|Control character error, possibly incorrectly encoded
|`JSON_ERROR_SYNTAX`|Syntax error **(since PHP 5.3.3)**
|`JSON_ERROR_UTF8`|Malformed UTF-8 characters, possibly incorrectly encoded **(since PHP 5.5.0)**
|`JSON_ERROR_RECURSION`|One or more recursive references in the value to be encoded
|`JSON_ERROR_INF_OR_NAN`|One or more NAN or INF values in the value to be encoded
|`JSON_ERROR_UNSUPPORTED_TYPE`|A value of a type that cannot be encoded was given



## Using JsonSerializable in an Object


When you build REST API's, you may need to reduce the information of an object to be passed to the client application. For this purpose, this example illustrates how to use the `JsonSerialiazble` interface.

In this example, the class `User` actually extends a DB model object of a hypotetical ORM.

```
class User extends Model implements JsonSerializable {
    public $id;
    public $name;
    public $surname;
    public $username;
    public $password;
    public $email;
    public $date_created;
    public $date_edit;
    public $role;
    public $status;

    public function jsonSerialize() {
        return [
            'name' => $this->name,
            'surname' => $this->surname,
            'username' => $this->username
        ];
    }
}

```

Add `JsonSerializable` implementation to the class, by providing the `jsonSerialize()` method.

```
public function jsonSerialize()

```

Now in your application controller or script, when passing the object User to `json_encode()` you will get the return json encoded array of the `jsonSerialize()` method instead of the entire object.

```
json_encode($User);

```

Will return:

```
{"name":"John", "surname":"Doe", "username" : "TestJson"}

```

### properties values example.

This will both reduce the amount of data returned from a RESTful endpoint, and allow to exclude object properties from a json representation.

### **Using Private and Protected Properties with `json_encode()`**

To avoid using JsonSerializable, it is also possible to use private or protected properties to hide class information from `json_encode()` output. The Class then does not need to implement \JsonSerializable.

> 
<p>The json_encode() function will only encode public properties of a
class into JSON.</p>


```
<?php

class User {
    // private properties only within this class
    private $id;
    private $date_created;
    private $date_edit;

    // properties used in extended classes
    protected $password;
    protected $email;
    protected $role;
    protected $status;

    // share these properties with the end user        
    public $name;
    public $surname;
    public $username;

    // jsonSerialize() not needed here
}        

$theUser = new User();

var_dump(json_encode($theUser));

```

### **Output:**

```
string(44) "{"name":null,"surname":null,"username":null}"

```



## Header json and the returned response


By adding a header with content type as JSON:

```
<?php
 $result = array('menu1' => 'home', 'menu2' => 'code php', 'menu3' => 'about');

//return the json response :
header('Content-Type: application/json');  // <-- header declaration
echo json_encode($result, true);    // <--- encode
exit();

```

The header is there so your app can detect what data was returned and how it should handle it. <br>
**Note that :** the content header is just information about type of returned data.

If you are using UTF-8, you can use :

```
header("Content-Type: application/json;charset=utf-8");

```

Example jQuery :

```
$.ajax({
        url:'url_your_page_php_that_return_json'        
    }).done(function(data){
        console.table('json ',data);
        console.log('Menu1 : ', data.menu1);
    });

```



#### Syntax


- string json_encode ( mixed $value [, int $options = 0 [, int $depth = 512 ]] )
- mixed json_decode ( string $json [, bool $assoc = false [, int $depth = 512 [, int $options = 0 ]]] )



#### Parameters


|Parameter|Details
|------
|**json_encode**|-
|value|The value being encoded. Can be any type except a resource. All string data must be UTF-8 encoded.
|options|Bitmask consisting of JSON_HEX_QUOT, JSON_HEX_TAG, JSON_HEX_AMP, JSON_HEX_APOS, JSON_NUMERIC_CHECK, JSON_PRETTY_PRINT, JSON_UNESCAPED_SLASHES, JSON_FORCE_OBJECT, JSON_PRESERVE_ZERO_FRACTION, JSON_UNESCAPED_UNICODE, JSON_PARTIAL_OUTPUT_ON_ERROR. The behaviour of these constants is described on the [JSON constants](http://php.net/manual/en/json.constants.php) page.
|depth|Set the maximum depth. Must be greater than zero.
|**json_decode**|-
|json|The json string being decoded. This function only works with UTF-8 encoded strings.
|assoc|Should function return associative array instead of objects.
|options|Bitmask of JSON decode options. Currently only JSON_BIGINT_AS_STRING is supported (default is to cast large integers as floats)



#### Remarks


- **json_decode**  handling of invalid JSON is very flaky, and it is very hard to reliably determine if the decoding succeeded, json_decode returns null for invalid input, even though null is also a perfectly valid object for JSON to decode to. **To prevent such problems you should always call <strong>json_last_error** every time you use it.</strong>

