---
metaTitle: "PHP - SOAP Client"
description: "WSDL Mode, Non-WSDL Mode, Classmaps, Tracing SOAP request and response"
---

# SOAP Client




## WSDL Mode


First, create a new `SoapClient` object, passing the URL to the WSDL file and optionally, an array of options.

```php
// Create a new client object using a WSDL URL
$soap = new SoapClient('https://example.com/soap.wsdl', [
    # This array and its values are optional
    'soap_version' => SOAP_1_2,
    'compression' => SOAP_COMPRESSION_ACCEPT | SOAP_COMPRESSION_GZIP,
    'cache_wsdl' => WSDL_CACHE_BOTH,
    # Helps with debugging
    'trace' => TRUE,
    'exceptions' => TRUE
]);

```

Then use the `$soap` object to call your SOAP methods.

```php
$result = $soap->requestData(['a', 'b', 'c']);

```



## Non-WSDL Mode


This is similar to WSDL mode, except we pass `NULL` as the WSDL file and make sure to set the `location` and `uri` options.

```php
$soap = new SoapClient(NULL, [
    'location' => 'https://example.com/soap/endpoint',
    'uri' => 'namespace'
]);

```



## Classmaps


When creating a SOAP Client in PHP, you can also set a `classmap` key in the configuration array. This `classmap` defines which types defined in the WSDL should be mapped to actual classes, instead of the default `StdClass`. The reason you would want to do this is because you can get auto-completion of fields and method calls on these classes, instead of having to guess which fields are set on the regular `StdClass`.

```php
class MyAddress {
    public $country;
    public $city;
    public $full_name;
    public $postal_code; // or zip_code
    public $house_number;
}

class MyBook {
    public $name;
    public $author;

    // The classmap also allows us to add useful functions to the objects
    // that are returned from the SOAP operations.
    public function getShortDescription() {
        return "{$this->name}, written by {$this->author}";
    }
}

$soap_client = new SoapClient($link_to_wsdl, [
    // Other parameters
    "classmap" => [
        "Address" => MyAddress::class, // ::class simple returns class as string
        "Book" => MyBook::class,
    ]
]);

```

After configuring the classmap, whenever you perform a certain operation that returns a type `Address` or `Book`, the SoapClient will instantiate that class, fill the fields with the data and return it from the operation call.

```php
// Lets assume 'getAddress(1234)' returns an Address by ID in the database
$address = $soap_client->getAddress(1234);

// $address is now of type MyAddress due to the classmap
echo $address->country;

// Lets assume the same for 'getBook(1234)'
$book = $soap_client->getBook(124);

// We can not use other functions defined on the MyBook class
echo $book->getShortDescription();

// Any type defined in the WSDL that is not defined in the classmap
// will become a regular StdClass object
$author = $soap_client->getAuthor(1234);

// No classmap for Author type, $author is regular StdClass.
// We can still access fields, but no auto-completion and no custom functions
// to define for the objects.
echo $author->name;

```



## Tracing SOAP request and response


Sometimes we want to look at what is sent and received in the SOAP request. The following methods will return the XML in the request and response:

```php
SoapClient::__getLastRequest()
SoapClient::__getLastRequestHeaders()
SoapClient::__getLastResponse()
SoapClient::__getLastResponseHeaders()

```

For example, suppose we have an `ENVIRONMENT` constant and when this constant's value is set to `DEVELOPMENT` we want to echo all information when the call to `getAddress` throws an error. One solution could be:

```php
try {
    $address = $soap_client->getAddress(1234);
} catch (SoapFault $e) {
    if (ENVIRONMENT === 'DEVELOPMENT') {
        var_dump(
            $soap_client->__getLastRequestHeaders()
            $soap_client->__getLastRequest(),
            $soap_client->__getLastResponseHeaders(),
            $soap_client->__getLastResponse()
        );
    }
    ...
}

```



#### Syntax


- [__getFunctions()](http://php.net/manual/en/soapclient.getfunctions.php) // Returns array of functions for service (WSDL mode only)
- [__getTypes()](http://php.net/manual/en/soapclient.gettypes.php) // Returns array of types for service (WSDL mode only)
- [__getLastRequest()](http://php.net/manual/en/soapclient.getlastrequest.php) // Returns XML from last request (Requires `trace` option)
- [__getLastRequestHeaders()](http://php.net/manual/en/soapclient.getlastrequestheaders.php) // Returns headers from last request (Requires `trace` option)
- [__getLastResponse()](http://php.net/manual/en/soapclient.getlastresponse.php) // Returns XML from last response (Requires `trace` option)
- [__getLastResponseHeaders()](http://php.net/manual/en/soapclient.getlastresponseheaders.php) // Returns headers from last response (Requires `trace` option)



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|$wsdl|URI of WSDL or `NULL` if using non-WSDL mode
|$options|Array of options for SoapClient.  Non-WSDL mode requires `location` and `uri` to set, all other options are optional.  See table below for possible values.



#### Remarks


The `SoapClient` class is equipped with a `__call` method.  This is **not** to be called directly.  Instead this allows you to do:

```php
$soap->requestInfo(['a', 'b', 'c']);

```

This will call the `requestInfo` SOAP method.

Table of possible `$options` values (**Array of key/value pairs**):

|Option|Details
|---|---|---|---|---|---|---|---|---|---
|location|URL of SOAP server.  **Required** in non-WSDL mode.  Can be used in WSDL mode to override the URL.
|uri|Target namespace of SOAP service.  **Required** in non-WSDL mode.
|style|Possible values are `SOAP_RPC` or `SOAP_DOCUMENT`.  Only valid in non-WSDL mode.
|use|Possible values are `SOAP_ENCODED` or `SOAP_LITERAL`.  Only valid in non-WSDL mode.
|soap_version|Possible values are `SOAP_1_1` (**default**) or `SOAP_1_2`.
|authentication|Enable HTTP authentication.  Possible values are `SOAP_AUTHENTICATION_BASIC` (**default**) or `SOAP_AUTHENTICATION_DIGEST`.
|login|Username for HTTP authentication
|password|Password for HTTP authentication
|proxy_host|URL of proxy server
|proxy_port|Proxy server port
|proxy_login|Username for proxy
|proxy_password|Password for proxy
|local_cert|Path to HTTPS client cert (for authentication)
|passphrase|Passphrase for HTTPS client cert
|compression|Compress request / response.  Value is a bitmask of `SOAP_COMPRESSION_ACCEPT` with either `SOAP_COMPRESSION_GZIP` or `SOAP_COMPRESSION_DEFLATE`.  For example: `SOAP_COMPRESSION_ACCEPT \| SOAP_COMPRESSION_GZIP`.
|encoding|Internal character encoding (TODO: possible values)
|trace|**Boolean**, defaults to `FALSE`. Enables tracing of requests so faults can be backtraced. Enables use of `__getLastRequest()`, `__getLastRequestHeaders()`, `__getLastResponse()` and `__getLastResponseHeaders()`.
|classmap|Map WSDL types to PHP classes.  Value should be an array with WSDL types as keys and PHP class names as values.
|exceptions|**Boolean** value.  Should SOAP errors exceptions (of type `SoapFault).
|connection_timeout|Timeout (in seconds) for the connection to the SOAP service.
|typemap|Array of type mappings.  Array should be key/value pairs with the following keys: `type_name`, `type_ns` (namespace URI), `from_xml` (callback accepting one string parameter) and `to_xml` (callback accepting one object parameter).
|cache_wsdl|How (if at all) should the WSDL file be cached.  Possible values are `WSDL_CACHE_NONE`, `WSDL_CACHE_DISK`, `WSDL_CACHE_MEMORY` or `WSDL_CACHE_BOTH`.
|user_agent|String to use in the `User-Agent` header.
|stream_context|A resource for a context.
|features|Bitmask of `SOAP_SINGLE_ELEMENT_ARRAYS`, `SOAP_USE_XSI_ARRAY_TYPE`, `SOAP_WAIT_ONE_WAY_CALLS`.
|keep_alive|(**PHP version >= 5.4 only**) **Boolean** value.  Send either `Connection: Keep-Alive` header (`TRUE`) or `Connection: Close` header (`FALSE`).
|ssl_method|(**PHP version >= 5.5 only**) Which SSL/TLS version to use.  Possible values are `SOAP_SSL_METHOD_TLS`, `SOAP_SSL_METHOD_SSLv2`, `SOAP_SSL_METHOD_SSLv3` or `SOAP_SSL_METHOD_SSLv23`.

> 
**[Issue with 32 bit PHP](http://stackoverflow.com/questions/19228213/php-soapclient-soap-request-with-long-integer):** In 32 bit PHP, numeric strings greater than 32 bits which are automatically cast to integer by `xs:long` will result in it hitting the 32 bit limit, casting it to `2147483647`. To work around this, cast the strings to float before passing it in to `__soapCall()`.


