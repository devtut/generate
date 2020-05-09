---
metaTitle: "iOS - Codable"
description: "Use of Codable with JSONEncoder and JSONDecoder in Swift 4"
---

# Codable


[Codable](https://developer.apple.com/documentation/swift/codable) is added with Xcode 9, iOS 11 and Swift 4. Codable is used to make your data types encodable and decodable for compatibility with external representations such as JSON.

Codable use to support both encoding and decoding, declare conformance to Codable, which combines the Encodable and Decodable protocols. This process is known as making your types codable.



## Use of Codable with JSONEncoder and JSONDecoder in Swift 4


Let’s Take an Example with Structure of Movie, here we have defined the structure as Codable. So, We can encode and decode it easily.

```swift
struct Movie: Codable {
    enum MovieGenere: String, Codable {
        case horror, skifi, comedy, adventure, animation
    }
    
    var name : String
    var moviesGenere : [MovieGenere]
    var rating : Int
}

```

**We can create a object from movie like as:**

```swift
let upMovie = Movie(name: "Up", moviesGenere: [.comedy , .adventure, .animation], rating : 4)

```

The upMovie contains the name “Up” and it’s movieGenere is comedy, adventure and animation witch contains 4 rating out of 5.

**Encode**

JSONEncoder is an object that encodes instances of a data type as JSON objects. JSONEncoder supports the Codable object.

```swift
// Encode data
let jsonEncoder = JSONEncoder()
do {
    let jsonData = try jsonEncoder.encode(upMovie)
    let jsonString = String(data: jsonData, encoding: .utf8)
    print("JSON String : " + jsonString!)
}
catch {
}

```

JSONEncoder will give us the JSON data which is used to retrieve JSON string.

**Output string will be like :**

```swift
{
  "name": "Up",
  "moviesGenere": [
    "comedy",
    "adventure",
    "animation"
  ],
  "rating": 4
}

```

**Decode**

JSONDecoder is an object that decodes instances of a data type from JSON objects. We can get the object back from the JSON string.

```swift
do {
    // Decode data to object
    
    let jsonDecoder = JSONDecoder()
    let upMovie = try jsonDecoder.decode(Movie.self, from: jsonData)
    print("Rating : \(upMovie.name)")
    print("Rating : \(upMovie.rating)")
}
catch {
}

```

By decoding the JSONData we will receive the Movie object back. So we can get all the values which is saved in that object.

**Output will be like:**

```swift
Name : Up
Rating : 4

```

