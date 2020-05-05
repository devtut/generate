---
metaTitle: "Java - NumberFormat"
description: "NumberFormat"
---

# NumberFormat




## NumberFormat


Different countries have different number formats and considering this we can have different formats using Locale of java. Using locale can help in formatting

```java
Locale locale = new Locale("en", "IN");
NumberFormat numberFormat = NumberFormat.getInstance(locale);

```

using above format you can perform various tasks

<li>
Format Number
`numberFormat.format(10000000.99);`
</li>

<li>
Format Currency
`NumberFormat currencyFormat = NumberFormat.getCurrencyInstance(locale); currencyFormat.format(10340.999);`
</li>

<li>
Format Percentage
`NumberFormat percentageFormat = NumberFormat.getPercentInstance(locale); percentageFormat.format(10929.999);`
</li>
<li>
Control Number of Digits
</li>

```java
numberFormat.setMinimumIntegerDigits(int digits)
numberFormat.setMaximumIntegerDigits(int digits)
numberFormat.setMinimumFractionDigits(int digits)
numberFormat.setMaximumFractionDigits(int digits)

```

