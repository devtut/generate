---
metaTitle: "Digit separators"
description: "Digit Separator"
---

# Digit separators




## Digit Separator


Numeric literals of more than a few digits are hard to read.

- Pronounce 7237498123.
- Compare 237498123 with 237499123 for equality.
- Decide whether 237499123 or 20249472 is larger.

`C++14` define Simple Quotation Mark `'` as a digit separator, in numbers and user-defined literals. This can make it easier for human readers to parse large numbers.

```cpp
long long decn = 1'000'000'000ll;
long long hexn = 0xFFFF'FFFFll; 
long long octn = 00'23'00ll;
long long binn = 0b1010'0011ll;

```

Single quotes mark are ignored when determining its value.

**Example:**

<li>The literals `1048576`,
`1'048'576`, `0X100000`, `0x10'0000`, and `0'004'000'000` all have the same value.</li>
- The literals `1.602'176'565e-19` and `1.602176565e-19` have the same value.

The position of the single quotes is irrelevant. All the following are equivalent:

```cpp
long long a1 = 123456789ll;
long long a2 = 123'456'789ll; 
long long a3 = 12'34'56'78'9ll;
long long a4 = 12345'6789ll;

```

It is also allowed in `user-defined` literals:

```cpp
std::chrono::seconds tiempo = 1'674'456s + 5'300h;

```

