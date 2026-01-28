---
metaTitle: "Spring - RestTemplate"
description: "Downloading a Large File, Using Preemptive Basic Authentication with RestTemplate and HttpClient, Using Basic Authentication with HttpComponent's HttpClient, Setting headers on Spring RestTemplate request, Generics results from Spring RestTemplate"
---

# RestTemplate



## Downloading a Large File


The `getForObject` and `getForEntity` methods of `RestTemplate` load the entire response in memory. This is not suitable for downloading large files since it can cause out of memory exceptions. This example shows how to stream the response of a GET request.

```java
RestTemplate restTemplate // = ...;

// Optional Accept header
RequestCallback requestCallback = request -> request.getHeaders()
        .setAccept(Arrays.asList(MediaType.APPLICATION_OCTET_STREAM, MediaType.ALL));

// Streams the response instead of loading it all in memory
ResponseExtractor<Void> responseExtractor = response -> {
    // Here I write the response to a file but do what you like
    Path path = Paths.get("some/path");
    Files.copy(response.getBody(), path);
    return null;
};
restTemplate.execute(URI.create("www.something.com"), HttpMethod.GET, requestCallback, responseExtractor);

```

Note that you cannot simply return the `InputStream` from the extractor, because by the time the execute method returns, the underlying connection and stream are already closed.



## Using Preemptive Basic Authentication with RestTemplate and HttpClient


Preemptive basic authentication is the practice of sending http basic authentication credentials (username and password) **before** a server replies with a `401` response asking for them. This can save a request round trip when consuming REST apis which are known to require basic authentication.

As described in the [Spring documentation](http://docs.spring.io/spring/docs/current/spring-framework-reference/htmlsingle/#rest-resttemplate), [Apache HttpClient](https://hc.apache.org/httpcomponents-client-ga/) may be used as the underlying implementation to create HTTP requests by using the `HttpComponentsClientHttpRequestFactory`. HttpClient can be configured to do [preemptive basic authentication](https://hc.apache.org/httpcomponents-client-ga/tutorial/html/authentication.html#d5e717).

The following class extends `HttpComponentsClientHttpRequestFactory` to provide preemptive basic authentication.

```java
/**
 * {@link HttpComponentsClientHttpRequestFactory} with preemptive basic
 * authentication to avoid the unnecessary first 401 response asking for
 * credentials.
 * <p>
 * Only preemptively sends the given credentials to the given host and
 * optionally to its subdomains. Matching subdomains can be useful for APIs
 * using multiple subdomains which are not always known in advance.
 * <p>
 * Other configurations of the {@link HttpClient} are not modified (e.g. the
 * default credentials provider).
 */
public class PreAuthHttpComponentsClientHttpRequestFactory extends HttpComponentsClientHttpRequestFactory {

    private String hostName;
    private boolean matchSubDomains;
    private Credentials credentials;

    /**
     * @param httpClient
     *            client
     * @param hostName
     *            host name
     * @param matchSubDomains
     *            whether to match the host's subdomains
     * @param userName
     *            basic authentication user name
     * @param password
     *            basic authentication password
     */
    public PreAuthHttpComponentsClientHttpRequestFactory(HttpClient httpClient, String hostName,
            boolean matchSubDomains, String userName, String password) {
        super(httpClient);
        this.hostName = hostName;
        this.matchSubDomains = matchSubDomains;
        credentials = new UsernamePasswordCredentials(userName, password);
    }

    @Override
    protected HttpContext createHttpContext(HttpMethod httpMethod, URI uri) {
        // Add AuthCache to the execution context
        HttpClientContext context = HttpClientContext.create();
        context.setCredentialsProvider(new PreAuthCredentialsProvider());
        context.setAuthCache(new PreAuthAuthCache());
        return context;
    }

    /**
     * @param host
     *            host name
     * @return whether the configured credentials should be used for the given
     *         host
     */
    protected boolean hostNameMatches(String host) {
        return host.equals(hostName) || (matchSubDomains && host.endsWith("." + hostName));
    }

    private class PreAuthCredentialsProvider extends BasicCredentialsProvider {
        @Override
        public Credentials getCredentials(AuthScope authscope) {
            if (hostNameMatches(authscope.getHost())) {
                // Simulate a basic authenticationcredentials entry in the
                // credentials provider.
                return credentials;
            }
            return super.getCredentials(authscope);
        }
    }

    private class PreAuthAuthCache extends BasicAuthCache {
        @Override
        public AuthScheme get(HttpHost host) {
            if (hostNameMatches(host.getHostName())) {
                // Simulate a cache entry for this host. This instructs
                // HttpClient to use basic authentication for this host.
                return new BasicScheme();
            }
            return super.get(host);
        }
    }
}

```

This can be used as follows:

```java
HttpClientBuilder builder = HttpClientBuilder.create();
ClientHttpRequestFactory requestFactory =
    new PreAuthHttpComponentsClientHttpRequestFactory(builder.build(),
        "api.some-host.com", true, "api", "my-key");
RestTemplate restTemplate = new RestTemplate(requestFactory);

```



## Using Basic Authentication with HttpComponent's HttpClient


Using `HttpClient` as `RestTemplate`'s underlying implementation to create HTTP requests allows for automatic handling of basic authentication requests (an http 401 response) when interacting with APIs. This example shows how to configure a `RestTemplate` to achieve this.

```java
// The credentials are stored here
CredentialsProvider credsProvider = new BasicCredentialsProvider();
credsProvider.setCredentials(
        // AuthScope can be configured more extensively to restrict
        // for which host/port/scheme/etc the credentials will be used.
        new AuthScope("somehost", AuthScope.ANY_PORT), 
        new UsernamePasswordCredentials("username", "password"));

// Use the credentials provider
HttpClientBuilder builder = HttpClientBuilder.create();
builder.setDefaultCredentialsProvider(credsProvider);

// Configure the RestTemplate to use HttpComponent's HttpClient
ClientHttpRequestFactory requestFactory =
        new HttpComponentsClientHttpRequestFactory(builder.build());
RestTemplate restTemplate = new RestTemplate(requestFactory);

```



## Setting headers on Spring RestTemplate request


The `exchange` methods of `RestTemplate` allows you specify a `HttpEntity` that will be written to the request when execute the method. You can add headers (such user agent, referrer...) to this entity:

```java
public void testHeader(final RestTemplate restTemplate){
        //Set the headers you need send
        final HttpHeaders headers = new HttpHeaders();
        headers.set("User-Agent", "eltabo");

        //Create a new HttpEntity
        final HttpEntity<String> entity = new HttpEntity<String>(headers);
        
        //Execute the method writing your HttpEntity to the request
        ResponseEntity<Map> response = restTemplate.exchange("https://httpbin.org/user-agent", HttpMethod.GET, entity, Map.class);        
        System.out.println(response.getBody());
    }

```

Also you can add an interceptor to your `RestTemplate` if you need to add the same headers to multiple requests:

```java
public void testHeader2(final RestTemplate restTemplate){
    //Add a ClientHttpRequestInterceptor to the RestTemplate
    restTemplate.getInterceptors().add(new ClientHttpRequestInterceptor(){
        @Override
        public ClientHttpResponse intercept(HttpRequest request, byte[] body, ClientHttpRequestExecution execution) throws IOException {
            request.getHeaders().set("User-Agent", "eltabo");//Set the header for each request
            return execution.execute(request, body);
        }
    }); 
    
    ResponseEntity<Map> response = restTemplate.getForEntity("https://httpbin.org/user-agent", Map.class);        
    System.out.println(response.getBody());
    
    ResponseEntity<Map> response2 = restTemplate.getForEntity("https://httpbin.org/headers", Map.class);        
    System.out.println(response2.getBody());
}

```



## Generics results from Spring RestTemplate


To let `RestTemplate` understand generic of returned content we need to define result type reference.

`org.springframework.core.ParameterizedTypeReference` has been introduced since 3.2

```java
Wrapper<Model> response = restClient.exchange(url, 
                          HttpMethod.GET, 
                          null, 
                          new ParameterizedTypeReference<Wrapper<Model>>() {}).getBody();

```

Could be useful to get e.g. `List<User>` from a controller.

