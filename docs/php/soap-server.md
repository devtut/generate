---
metaTitle: "SOAP Server"
description: "Basic SOAP Server"
---

# SOAP Server



## Basic SOAP Server


```
function test($x)
{
    return $x;
}

$server = new SoapServer(null, array('uri' => "http://test-uri/"));
$server->addFunction("test");
$server->handle();

```



#### Syntax


- [addFunction()](http://php.net/manual/en/soapserver.addfunction.php) //Register one (or more) function into SOAP request handler
- [addSoapHeader()](http://php.net/manual/en/soapserver.addsoapheader.php) //Add a SOAP header to the response
- [fault()](http://php.net/manual/en/soapserver.fault.php) //Issue SoapServer fault indicating an error
- [getFunctions()](http://php.net/manual/en/soapserver.getfunctions.php) //Returns list of functions
- [handle()](http://php.net/manual/en/soapserver.handle.php) //Handles a SOAP request
- [setClass()](http://php.net/manual/en/soapserver.setclass.php) //Sets the class which handles SOAP requests
- [setObject()](http://php.net/manual/en/soapserver.setobject.php) //Sets the object which will be used to handle SOAP requests
- [setPersistence()](http://php.net/manual/en/soapserver.setpersistence.php) //Sets SoapServer persistence mode

