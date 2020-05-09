---
metaTitle: "iOS - StoreKit"
description: "Get localized product information from the App Store"
---

# StoreKit



## Get localized product information from the App Store


Get localized product information from a set of product identifier strings using `SKProductsRequest`:

```swift
import StoreKit

let productIdentifierSet = Set(["yellowSubmarine", "pennyLane"])
let productsRequest = SKProductsRequest(productIdentifiers: productIdentifierSet)

```

In order to process the products from the `productsRequest`, we need to assign a delegate to the request that handles the response. The delegate needs to conform to the `SKProductsRequestDelegate` protocol, which means that it must inherit from `NSObject` (i.e. any `Foundation` object) and implement the `productsRequest` method:

```swift
class PaymentManager: NSObject, SKProductsRequestDelegate {

    var products: [SKProduct] = []

    func productsRequest(request: SKProductsRequest,
                         didReceiveResponse response: SKProductsResponse) {

        products = response.products

    }

}

```

To initiate the `productsRequest` we assign `PaymentManager` as the products-request's delegate, and call the `start()` method on the request:

```swift
let paymentManager = PaymentManager()
productsRequest.delegate = paymentManager
productsRequest.start()

```

If the requests succeeds the products will be in `paymentManager.products`.

