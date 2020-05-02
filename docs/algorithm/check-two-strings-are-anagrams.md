---
metaTitle: "Check two strings are anagrams"
description: "Sample input and output, Generic Code for Anagrams"
---

# Check two strings are anagrams


Two string with same set of character is called anagram. I have used javascript here.

We will create an hash of str1 and increase count +1.
We will loop on 2nd string and check all characters are there in hash and decrease value of hash key.
Check all value of hash key are zero will be anagram.



## Sample input and output


Ex1:-

```cpp
let str1 = 'stackoverflow';
let str2 = 'flowerovstack';

```

These strings are anagrams.

// Create Hash from str1 and increase one count.

```cpp
hashMap = {
    s : 1,
    t : 1,
    a : 1,
    c : 1,
    k : 1,
    o : 2,
    v : 1,
    e : 1,
    r : 1,
    f : 1,
    l : 1,
    w : 1
}

```

You can see hashKey 'o' is containing value 2 because o is 2 times in string.

Now loop over str2 and check for each character are present in hashMap, if yes, decrease value of hashMap Key, else return false (which indicate it's not anagram).

```cpp
hashMap = {
    s : 0,
    t : 0,
    a : 0,
    c : 0,
    k : 0,
    o : 0,
    v : 0,
    e : 0,
    r : 0,
    f : 0,
    l : 0,
    w : 0
}

```

Now, loop over hashMap object and check all values are zero in the key of hashMap.

In our case all values are zero so its a anagram.



## Generic Code for Anagrams


```cpp
(function(){

    var hashMap = {};
    
    function isAnagram (str1, str2) {
    
        if(str1.length !== str2.length){
            return false;
        }
        
        // Create hash map of str1 character and increase value one (+1).
        createStr1HashMap(str1);

        // Check str2 character are key in hash map and decrease value by one(-1);
        var valueExist = createStr2HashMap(str2);

        // Check all value of hashMap keys are zero, so it will be anagram.
        return isStringsAnagram(valueExist);
    }
    
    function createStr1HashMap (str1) {
        [].map.call(str1, function(value, index, array){
            hashMap[value] = value in hashMap ?  (hashMap[value] + 1) : 1;
            return value;
        });
    }
    
    function createStr2HashMap (str2) {
        var valueExist = [].every.call(str2, function(value, index, array){
            if(value in hashMap) {
                hashMap[value] = hashMap[value] - 1;
            }
            return value in hashMap;
        });
        return valueExist;
    }
    
    function isStringsAnagram (valueExist) {
        if(!valueExist) {
            return valueExist;
        } else {
            var isAnagram;
            for(var i in hashMap) {
                if(hashMap[i] !== 0) {
                    isAnagram = false;
                    break;
                } else {
                    isAnagram = true;
                }
            }
    
            return isAnagram;
        }
    }
    
    isAnagram('stackoverflow', 'flowerovstack'); // true
    isAnagram('stackoverflow', 'flowervvstack'); // false
    
})();

```

Time complexity :- 3n i.e O(n).

