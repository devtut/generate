---
metaTitle: "Spring MVC - File Upload"
description: "Uploading a single file, Marshaling a part into an object, Uploading multiple files, Uploading multiple parts with different names"
---

# File Upload



## Uploading a single file


To receive a file uploaded via an HTTP Post, you need to do the following:

```java
@RequestMapping(
    value = "...",
    method = RequestMethod.POST,
    consumes = MediaType.MULTIPART_FORM_DATA_VALUE
)
public Object uploadFile(
    @RequestPart MultipartFile file
) {
    String fileName = file.getOriginalFilename();
    InputStream inputStream = file.getInputStream();
    String contentType = file.getContentType();
    .
    .
    .
}

```

Note that the name of the `@RequestPart` parameter needs to match up with the name of the part in the request.

As HTML:

```

<form action="/..." enctype="multipart/form-data" method="post">
        <input type="file" name="file">
    </form>

```

As HTML ([Spring TagLibs](http://docs.spring.io/spring/docs/current/spring-framework-reference/html/view.html#view-jsp-formtaglib)):

```java
<form action="/..." enctype="multipart/form-data" method="post">
    <form:input type="file" path="file">
</form>

```

As a raw HTTP request:

```java
POST /... HTTP/1.1
Host: ...
Content-Type: multipart/form-data; boundary=----------287032381131322

------------287032381131322
Content-Disposition: form-data; name="file"; filename="r.gif"
Content-Type: image/gif

GIF87a.............,...........D..;
------------287032381131322--

```

That request would mean the following:

```java
fileName == "r.gif"
contentType == "image/gif"

```

In Spring MVC

Need to add the mentioned bean for accessing multipart functionality


 

```

   <!-- max size of file in memory (in bytes) -->
    <property name="maxInMemorySize" value="1048576" /> <!-- 1MB -->

</bean>

```



## Marshaling a part into an object


If you want to convert the content of a part into a domain object (e.g. a `User` or `Account` or `Address`), then the process is very simple:

It is possible to upload multiple parts, each with a different name. For each part name, you will need one parameter annotated with `@RequestPart`, whose name matches the part name.

To receive a file uploaded via an HTTP Post, you need to do the following:

```java
@RequestMapping(
    value = "...",
    method = RequestMethod.POST,
    consumes = MediaType.MULTIPART_FORM_DATA_VALUE
)
public Object uploadFile(
    @RequestPart Address address,
) {
    .
    .
    .
}

```

As a raw HTTP request:

```java
POST /... HTTP/1.1
Host: ...
Content-Type: multipart/form-data; boundary=----------287032381131322

------------287032381131322
Content-Disposition: form-data; name="address"; filename="address.json"
Content-Type: application/json

{"houseNumber": "10/A", "streetName": "Dumbldore Road", "town": "Hogsmede"}
------------287032381131322--

```

The most important things are:

- The name of the part must match the name of the variable.
- The `Content-Type` of the part must be one that Spring would be able to handle if you had sent it as a regular request. That is, if you could perform a `POST` to an endpoint with a `Content-Type` of `foo/bar`, and Spring is able to turn that into an object, then it will also be able to marshal a part into an object.
- You **must** be able to set the `Content-Type` of the part. If you cannot, this approach will not work - Spring will **not** attempt to guess the `Content-Type` of the part.



## Uploading multiple files


To receive multiple files uploaded via a single HTTP Post, you need to do the following:

```java
@RequestMapping(
    value = "...",
    method = RequestMethod.POST,
    consumes = MediaType.MULTIPART_FORM_DATA_VALUE
)
public Object uploadFile(
    @RequestPart MultipartFile[] files
) {
    for (file : files) {
        String fileName = file.getOriginalFilename();
        InputStream inputStream = file.getInputStream();
        String contentType = file.getContentType();
        .
        .
        .
    }
}

```

Note that the name of the `@RequestPart` parameter needs to match up with the name of the part in the request.

As HTML:

```java
<form action="/..." enctype="multipart/form-data" method="post">
    <input type="file" name="files">
    <input type="file" name="files">
</form>

```

As a raw HTTP request:

```java
POST /... HTTP/1.1
Host: ...
Content-Type: multipart/form-data; boundary=----------287032381131322

------------287032381131322
Content-Disposition: form-data; name="files"; filename="r.gif"
Content-Type: image/gif

GIF87a.............,...........D..;
------------287032381131322
Content-Disposition: form-data; name="files"; filename="banana.jpeg"
Content-Type: image/jpeg

GIF87a.............,...........D..;
------------287032381131322--

```

That request would mean the following:

```java
files[0].getOriginalFilename() == "r.gif"
files[0].getContentType() == "image/gif"
files[1].getOriginalFilename() == "r.jpeg"
files[1].getContentType() == "image/jpeg"

```



## Uploading multiple parts with different names


It is possible to upload multiple parts, each with a different name. For each part name, you will need one parameter annotated with `@RequestPart`, whose name matches the part name.

To receive a file uploaded via an HTTP Post, you need to do the following:

```java
@RequestMapping(
    value = "...",
    method = RequestMethod.POST,
    consumes = MediaType.MULTIPART_FORM_DATA_VALUE
)
public Object uploadFile(
    @RequestPart MultipartFile profilePicture,
    @RequestPart MultipartFile companyLogo,
) {
    .
    .
    .
}

```

As HTML:

```java
<form action="/..." enctype="multipart/form-data" method="post">
    <input type="file" name="profilePicture">
    <input type="file" name="companyLogo">
</form>

```

As a raw HTTP request:

```java
POST /... HTTP/1.1
Host: ...
Content-Type: multipart/form-data; boundary=----------287032381131322

------------287032381131322
Content-Disposition: form-data; name="profilePicture"; filename="r.gif"
Content-Type: image/gif

GIF87a.............,...........D..;
------------287032381131322
Content-Disposition: form-data; name="companyLogo"; filename="banana.jpeg"
Content-Type: image/jpeg

GIF87a.............,...........D..;
------------287032381131322--

```



#### Syntax


- [`@RequestPart(String, String, boolean)`](http://docs.spring.io/spring/docs/current/javadoc-api/org/springframework/web/bind/annotation/RequestPart.html)



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|[`@RequestPart`](http://docs.spring.io/spring/docs/current/javadoc-api/org/springframework/web/bind/annotation/RequestPart.html)|This annotation specifies that a parameter should be mapped to a given request part. The part name must match the name of the method parameter, **unless** you choose to provide it as an argument to `@RequestPart`. If the part name is not expressible as a Java name (e.g. `123`), then you can use the `value` attribute of the `@RequestPart` to specify the actual name. e.g. `@RequestPart("123") String _123`.



#### Remarks


If you are running on an older version of Java (pre 1.7), or are compiling **without** debug information, then Java will replace the name of the parameter with `arg0`, `arg1`, etc, which will prevent Spring from being able to match them up with the part names. If that is the case, then you will need to set the name of the part in the `@RequestPart` annotation, as documented in Parameters.

