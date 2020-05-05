---
metaTitle: "PHP - How to Detect Client IP Address"
description: "Proper use of HTTP_X_FORWARDED_FOR"
---

# How to Detect Client IP Address



## Proper use of HTTP_X_FORWARDED_FOR


In the light of the latest [httpoxy](https://httpoxy.org/) vulnerabilities, there is another variable, that is widely misused.

`HTTP_X_FORWARDED_FOR` is often used to detect the client IP address, but without any additional checks, this can lead to security issues, especially when this IP is later used for authentication or in SQL queries without sanitization.

Most of the code samples available ignore the fact that `HTTP_X_FORWARDED_FOR` can actually be considered as information provided by the client itself and therefore **is not** a reliable source to detect clients IP address. Some of the samples do add a warning about the possible misuse, but still lack any additional check in the code itself.

So here is an example of function written in PHP, how to detect a client IP address, if you know that client may be behind a proxy and you know this proxy can be trusted. If you don't known any trusted proxies, you can just use `REMOTE_ADDR`

```php
function get_client_ip()
{
    // Nothing to do without any reliable information
    if (!isset($_SERVER['REMOTE_ADDR'])) {
        return NULL;
    }
    
    // Header that is used by the trusted proxy to refer to
    // the original IP
    $proxy_header = "HTTP_X_FORWARDED_FOR";

    // List of all the proxies that are known to handle 'proxy_header'
    // in known, safe manner
    $trusted_proxies = array("2001:db8::1", "192.168.50.1");

    if (in_array($_SERVER['REMOTE_ADDR'], $trusted_proxies)) {
        
        // Get IP of the client behind trusted proxy
        if (array_key_exists($proxy_header, $_SERVER)) {

            // Header can contain multiple IP-s of proxies that are passed through.
            // Only the IP added by the last proxy (last IP in the list) can be trusted.
            $client_ip = trim(end(explode(",", $_SERVER[$proxy_header])));

            // Validate just in case
            if (filter_var($client_ip, FILTER_VALIDATE_IP)) {
                return $client_ip;
            } else {
                // Validation failed - beat the guy who configured the proxy or
                // the guy who created the trusted proxy list?
                // TODO: some error handling to notify about the need of punishment
            }
        }
    }

    // In all other cases, REMOTE_ADDR is the ONLY IP we can trust.
    return $_SERVER['REMOTE_ADDR'];
}

print get_client_ip();

```

