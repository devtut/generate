---
metaTitle: "Object Serialization"
description: "Serialize / Unserialize, The Serializable interface"
---

# Object Serialization



## Serialize / Unserialize


`serialize()` returns a string containing a byte-stream representation of any value that can be stored in PHP. `unserialize()` can use this string to recreate the original variable values.

**To serialize an object**

```
serialize($object);

```

**To Unserialize an object**

```
unserialize($object)

```

**Example**

```
$array = array();
$array["a"] = "Foo";
$array["b"] = "Bar";
$array["c"] = "Baz";
$array["d"] = "Wom";

$serializedArray = serialize($array);
echo $serializedArray; //output: a:4:{s:1:"a";s:3:"Foo";s:1:"b";s:3:"Bar";s:1:"c";s:3:"Baz";s:1:"d";s:3:"Wom";}

```



## The Serializable interface


**Introduction**

> 
Classes that implement this interface no longer support `__sleep()` and `__wakeup()`. The method serialize is called whenever an instance needs to be serialized. This does not invoke `__destruct()` or has any other side effect unless programmed inside the method. When the data is `unserialized` the class is known and the appropriate `unserialize()` method is called as a constructor instead of calling `__construct()`. If you need to execute the standard constructor you may do so in the method.


**Basic usage**

```
class obj implements Serializable {
    private $data;
    public function __construct() {
        $this->data = "My private data";
    }
    public function serialize() {
        return serialize($this->data);
    }
    public function unserialize($data) {
        $this->data = unserialize($data);
    }
    public function getData() {
        return $this->data;
    }
}

$obj = new obj;
$ser = serialize($obj);

var_dump($ser); // Output: string(38) "C:3:"obj":23:{s:15:"My private data";}"

$newobj = unserialize($ser);

var_dump($newobj->getData()); // Output: string(15) "My private data"

```



#### Syntax


- serialize($object)
- unserialize($object)



#### Remarks


All PHP types except for resources are serializable.  Resources are a unique variable type that reference "external" sources, such as database connections.

