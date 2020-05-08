---
metaTitle: "Android - Jackson"
description: "Full Data Binding Example"
---

# Jackson


**Jackson** is a multi-purpose Java library for processing JSON. Jackson aims to be the best possible combination of fast, correct, lightweight, and ergonomic for developers.

**Jackson features：**

Multi processing mode, and very good collaboration

Not only annotations, but also mixed annotations

Fully support generic types

Support polymorphic types



## Full Data Binding Example


**JSON data**

```java
{
  "name" : { "first" : "Joe", "last" : "Sixpack" },
  "gender" : "MALE",
  "verified" : false,
  "userImage" : "keliuyue"
}

```

**It takes two lines of Java to turn it into a User instance:**

```java
ObjectMapper mapper = new ObjectMapper(); // can reuse, share globally
User user = mapper.readValue(new File("user.json"), User.class);

```

**User.class**

```java
public class User {

    public enum Gender {MALE, FEMALE};

    public static class Name {
        private String _first, _last;

        public String getFirst() {
            return _first;
        }

        public String getLast() {
            return _last;
        }

        public void setFirst(String s) {
            _first = s;
        }

        public void setLast(String s) {
            _last = s;
        }
    }

    private Gender _gender;
    private Name _name;
    private boolean _isVerified;
    private byte[] _userImage;

    public Name getName() {
        return _name;
    }

    public boolean isVerified() {
        return _isVerified;
    }

    public Gender getGender() {
        return _gender;
    }

    public byte[] getUserImage() {
        return _userImage;
    }

    public void setName(Name n) {
        _name = n;
    }

    public void setVerified(boolean b) {
        _isVerified = b;
    }

    public void setGender(Gender g) {
        _gender = g;
    }

    public void setUserImage(byte[] b) {
        _userImage = b;
    }
}

```

**Marshalling back to JSON is similarly straightforward:**

```java
mapper.writeValue(new File("user-modified.json"), user);

```

