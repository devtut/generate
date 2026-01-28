---
metaTitle: "JavaScript - Localization"
description: "Number formatting, Currency formatting, Date and time formatting"
---

# Localization



## Number formatting


Number formatting, grouping digits according to the localization.

```js
const usNumberFormat = new Intl.NumberFormat('en-US');
const esNumberFormat = new Intl.NumberFormat('es-ES');

const usNumber = usNumberFormat.format(99999999.99); // "99,999,999.99"
const esNumber = esNumberFormat.format(99999999.99); // "99.999.999,99"

```



## Currency formatting


Currency formatting, grouping digits and placing the currency symbol according to the localization.

```js
const usCurrencyFormat = new Intl.NumberFormat('en-US', {style: 'currency', currency: 'USD'})
const esCurrencyFormat = new Intl.NumberFormat('es-ES', {style: 'currency', currency: 'EUR'})

const usCurrency = usCurrencyFormat.format(100.10); // "$100.10"
const esCurrency = esCurrencyFormat.format(100.10); // "100.10 €"

```



## Date and time formatting


Date time formatting, according to the localization.

```js
const usDateTimeFormatting = new Intl.DateTimeFormat('en-US');
const esDateTimeFormatting = new Intl.DateTimeFormat('es-ES');

const usDate = usDateTimeFormatting.format(new Date('2016-07-21')); // "7/21/2016"
const esDate = esDateTimeFormatting.format(new Date('2016-07-21')); // "21/7/2016"

```



#### Syntax


- new Intl.NumberFormat()
- new Intl.NumberFormat('en-US')
- new Intl.NumberFormat('en-GB',{timeZone: 'UTC'})



#### Parameters


|Paramater|Details
|---|---|---|---|---|---|---|---|---|---
|weekday|"narrow", "short", "long"
|era|"narrow", "short", "long"
|year|"numeric", "2-digit"
|month|"numeric", "2-digit", "narrow", "short", "long"
|day|"numeric", "2-digit"
|hour|"numeric", "2-digit"
|minute|"numeric", "2-digit"
|second|"numeric", "2-digit"
|timeZoneName|"short", "long"

