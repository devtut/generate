---
metaTitle: "Ruby - Pure RSpec JSON API testing"
description: "Testing Serializer object and introducing it to Controller"
---

# Pure RSpec JSON API testing



## Testing Serializer object and introducing it to Controller


Let say you want to build your API to comply  <a href="http://jsonapi.org/" rel="nofollow noreferrer">jsonapi.org
specification</a> and the result should look like:

Test for Serializer object may look like this:

Serializer object may look like this:

When we run our "serializers" specs everything passes.

That's pretty boring. Let's introduce a
typo to our Article Serializer: Instead of `type: "articles"` let's return `type: "events"` and rerun our tests.

Once you've run the test it's pretty easy to spot the error.

Once you fix the error  (correct the type to be `article`) you can introduce it to Controller like this:

This example is based on article: [http://www.eq8.eu/blogs/30-pure-rspec-json-api-testing](http://www.eq8.eu/blogs/30-pure-rspec-json-api-testing)

