---
metaTitle: "Regular expressions"
description: "Basic regex_match and regex_search Examples, regex_iterator Example, Anchors, regex_replace Example, regex_token_iterator Example, Splitting a string, Quantifiers"
---

# Regular expressions


[Regular Expressions](https://en.wikipedia.org/wiki/Regular_expression) (sometimes called regexs or regexps) are a textual syntax which represents the patterns which can be matched in the strings operated upon.

Regular Expressions, introduced in [c++11](https://stackoverflow.com/questions/tagged/c%2b%2b11), may optionally support a return array of matched strings or another textual syntax defining how to replace matched patterns in strings operated upon.



## Basic regex_match and regex_search Examples


```cpp
const auto input = "Some people, when confronted with a problem, think \"I know, I'll use regular expressions.\""s;
smatch sm;

cout << input << endl;

// If input ends in a quotation that contains a word that begins with "reg" and another word begining with "ex" then capture the preceeding portion of input
if (regex_match(input, sm, regex("(.*)\".*\\breg.*\\bex.*\"\\s*$"))) {
    const auto capture = sm[1].str();
    
    cout << '\t' << capture << endl; // Outputs: "\tSome people, when confronted with a problem, think\n"
    
    // Search our capture for "a problem" or "# problems"
    if(regex_search(capture, sm, regex("(a|d+)\\s+problems?"))) {
        const auto count = sm[1] == "a"s ? 1 : stoi(sm[1]);
        
        cout << '\t' << count << (count > 1 ? " problems\n" : " problem\n"); // Outputs: "\t1 problem\n"
        cout << "Now they have " << count + 1 << " problems.\n"; // Ouputs: "Now they have 2 problems\n"
    }
}

```

[Live Example](http://ideone.com/nSRXEa)



## regex_iterator Example


When processing of captures has to be done iteratively a `regex_iterator` is a good choice. Dereferencing a `regex_iterator` returns a `match_result`. This is great for conditional captures or captures which have interdependence. Let's say that we want to tokenize some C++ code. Given:

```cpp
enum TOKENS {
    NUMBER,
    ADDITION,
    SUBTRACTION,
    MULTIPLICATION,
    DIVISION,
    EQUALITY,
    OPEN_PARENTHESIS,
    CLOSE_PARENTHESIS
};

```

We can tokenize this string: `const auto input = "42/2 + -8\t=\n(2 + 2) * 2 * 2 -3"s` with a `regex_iterator` like this:

```cpp
vector<TOKENS> tokens;
const regex re{ "\\s*(\\(?)\\s*(-?\\s*\\d+)\\s*(\\)?)\\s*(?:(\\+)|(-)|(\\*)|(/)|(=))" };

for_each(sregex_iterator(cbegin(input), cend(input), re), sregex_iterator(), [&](const auto& i) {
    if(i[1].length() > 0) {
        tokens.push_back(OPEN_PARENTHESIS);
    }
    
    tokens.push_back(i[2].str().front() == '-' ? NEGATIVE_NUMBER : NON_NEGATIVE_NUMBER);
    
    if(i[3].length() > 0) {
        tokens.push_back(CLOSE_PARENTHESIS);
    }        
    
    auto it = next(cbegin(i), 4);
    
    for(int result = ADDITION; it != cend(i); ++result, ++it) {
        if (it->length() > 0U) {
            tokens.push_back(static_cast<TOKENS>(result));
            break;
        }
    }
});

match_results<string::const_reverse_iterator> sm;

if(regex_search(crbegin(input), crend(input), sm, regex{ tokens.back() == SUBTRACTION ? "^\\s*\\d+\\s*-\\s*(-?)" : "^\\s*\\d+\\s*(-?)" })) {
    tokens.push_back(sm[1].length() == 0 ? NON_NEGATIVE_NUMBER : NEGATIVE_NUMBER);
}

```

[Live Example](http://ideone.com/Rv5WNI)

A notable gotcha with regex iterators is that the `regex` argument must be an L-value, an R-value will not work: [Visual Studio regex_iterator Bug?](http://stackoverflow.com/q/29895747/2642059)



## Anchors


C++ provides only 4 anchors:

- `^` which asserts the start of the string
- `$` which asserts the end of the string
- `\b` which asserts a `\W` character or the beginning or end of the string
- `\B` which asserts a `\w` character

Let's say for example we want to capture a number **with** it's sign:

```cpp
auto input = "+1--12*123/+1234"s;
smatch sm;

if(regex_search(input, sm, regex{ "(?:^|\\b\\W)([+-]?\\d+)" })) {

    do {
        cout << sm[1] << endl;
        input = sm.suffix().str();
    } while(regex_search(input, sm, regex{ "(?:^\\W|\\b\\W)([+-]?\\d+)" }));
}

```

[Live Example](http://ideone.com/uE4dGr)

An important note here is that the anchor does not consume any characters.



## regex_replace Example


This code takes in various brace styles and converts them to [One True Brace Style](https://en.wikipedia.org/wiki/Indent_style#Variant:_1TBS):

```cpp
const auto input = "if (KnR)\n\tfoo();\nif (spaces) {\n    foo();\n}\nif (allman)\n{\n\tfoo();\n}\nif (horstmann)\n{\tfoo();\n}\nif (pico)\n{\tfoo(); }\nif (whitesmiths)\n\t{\n\tfoo();\n\t}\n"s;

cout << input << regex_replace(input, regex("(.+?)\\s*\\{?\\s*(.+?;)\\s*\\}?\\s*"), "$1 {\n\t$2\n}\n") << endl;

```

[Live Example](http://ideone.com/ICR5wM)



## regex_token_iterator Example


A [`std::regex_token_iterator`](http://en.cppreference.com/w/cpp/regex/regex_token_iterator) provides a tremendous tool for [extracting elements of a Comma Separated Value file](http://stackoverflow.com/a/28880605/2642059). Aside from the advantages of iteration, this iterator is also able to capture escaped commas where other methods struggle:

```cpp
const auto input = "please split,this,csv, ,line,\\,\n"s;
const regex re{ "((?:[^\\\\,]|\\\\.)+)(?:,|$)" };
const vector<string> m_vecFields{ sregex_token_iterator(cbegin(input), cend(input), re, 1), sregex_token_iterator() };

cout << input << endl;

copy(cbegin(m_vecFields), cend(m_vecFields), ostream_iterator<string>(cout, "\n"));

```

[Live Example](http://ideone.com/lySlTJ)

A notable gotcha with regex iterators is, that the `regex` argument must be an L-value. [An R-value will not work](http://stackoverflow.com/q/29895747/2642059).



## Splitting a string


```cpp
std::vector<std::string> split(const std::string &str, std::string regex)
{
    std::regex r{ regex };
    std::sregex_token_iterator start{ str.begin(), str.end(), r, -1 }, end;
    return std::vector<std::string>(start, end);
}

```

```cpp
split("Some  string\t with whitespace ", "\\s+"); // "Some", "string", "with", "whitespace"

```



## Quantifiers


Let's say that we're given `const string input` as a phone number to be validated. We could start by requiring a numeric input with a **zero or more quantifier**: `regex_match(input, regex("\\d*"))` or a **one or more quantifier**: `regex_match(input, regex("\\d+"))` But both of those really fall short if `input` contains an invalid numeric string like: "123" Let's use a **n or more quantifier** to ensure that we're getting at least 7 digits:

```cpp
regex_match(input, regex("\\d{7,}"))

```

This will guarantee that we will get at least a phone number of digits, but `input` could also contain a numeric string that's too long like: "123456789012". So lets go with a **between n and m quantifier** so the `input` is at least 7 digits but not more than 11:

```cpp
regex_match(input, regex("\\d{7,11}"));

```

This gets us closer, but illegal numeric strings that are in the range of [7, 11] are still accepted, like: "123456789" So let's make the country code optional with a **lazy quantifier**:

```cpp
regex_match(input, regex("\\d?\\d{7,10}"))

```

It's important to note that the **lazy quantifier** matches **as few characters as possible**, so the only way this character will be matched is if there are already 10 characters that have been matched by `\d{7,10}`. (To match the first character greedily we would have had to do: `\d{0,1}`.) The **lazy quantifier** can be appended to any other quantifier.

Now, how would we make the area code optional **and** only accept a country code if the area code was present?

```cpp
regex_match(input, regex("(?:\\d{3,4})?\\d{7}"))

```

In this final regex, the `\d{7}` **requires** 7 digits. These 7 digits are optionally preceded by either 3 or 4 digits.

Note that we did not append the **lazy quantifier**: <strike>`\d{3,4}?\d{7}`</strike>, the `\d{3,4}?` would have matched either 3 or 4 characters, preferring 3. Instead we're making the non-capturing group match at most once, preferring not to match. Causing a mismatch if `input` didn't include the area code like: "1234567".

In conclusion of the quantifier topic, I'd like to mention the other appending quantifier that you can use, the **possessive quantifier**. **Either** the **lazy quantifier** or the **possessive quantifier** can be appended to any quantifier. The **possessive quantifier**'s only function is to assist the regex engine by telling it, greedily take these characters **and don't ever give them up even if it causes the regex to fail**. This for example doesn't make much sense: `regex_match(input, regex("\\d{3,4}+\\d{7}))` Because an `input` like: "1234567890" wouldn't be matched as `\d{3,4}+` will always match 4 characters even if matching 3 would have allowed the regex to succeed.<br>
The **possessive quantifier** is best used **when the quantified token limits the number of matchable characters**. For example:

```cpp
regex_match(input, regex("(?:.*\\d{3,4}+){3}"))

```

Can be used to match if `input` contained any of the following:

> 
<p>123 456 7890<br>
123-456-7890<br>
(123)456-7890<br>
(123) 456 - 7890</p>


But when this regex really shines is when `input` contains an **illegal** input:

> 
12345 - 67890


Without the **possessive quantifier** the regex engine has to go back and test **every combination of `.*` and either 3 or 4 characters** to see if it can find a matchable combination. With the **possessive quantifier** the regex starts where the 2<sup>nd</sup> **possessive quantifier** left off, the '0' character, and the regex engine tries to adjust the `.*` to allow `\d{3,4}` to match; when it can't the regex just fails, no back tracking is done to see if earlier `.*` adjustment could have allowed a match.



#### Syntax


- regex_match // Returns whether the entire character sequence was matched by the regex, optionally capturing into a match object
- regex_search // Returns whether a portion of the character sequence was matched by the regex, optionally capturing into a match object
- regex_replace // Returns the input character sequence as modified by a regex via a replacement format string
- regex_token_iterator // Initialized with a character sequence defined by iterators, a list of capture indexes to iterate over, and a regex. Dereferencing returns the currently indexed match of the regex. Incrementing moves to the next capture index or if currently at the last index, resets the index and hinds the next occurrence of a regex match in the character sequence
- regex_iterator // Initialized with a character sequence defined by iterators and a regex. Dereferencing returns the portion of the character sequence the entire regex currently matches. Incrementing finds the next occurrence of a regex match in the character sequence



#### Parameters


|Signature|Description
|---|---|---|---|---|---|---|---|---|---
|`bool regex_match(BidirectionalIterator first, BidirectionalIterator last, smatch& sm, const regex& re, regex_constraints::match_flag_type flags)`|**`BidirectionalIterator`** is any character iterator that provides increment and decrement operators **`smatch`** may be `cmatch` or any other other variant of `match_results` that accepts the type of `BidirectionalIterator` the `smatch` argument may be ommitted if the results of the regex are not needed **Returns** whether `re` matches the entire character sequence defined by `first` and `last`
|`bool regex_match(const string& str, smatch& sm, const regex re&, regex_constraints::match_flag_type flags)`|**`string`** may be either a `const char*` or an L-Value `string`, **the functions accepting an R-Value `string` are explicitly deleted** **`smatch`** may be `cmatch` or any other other variant of `match_results` that accepts the type of `str` the `smatch` argument may be ommitted if the results of the regex are not needed **Returns** whether `re` matches the entire character sequence defined by `str`

