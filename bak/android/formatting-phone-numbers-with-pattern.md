---
metaTitle: "Android - Formatting phone numbers with pattern."
description: "Patterns + 1 (786) 1234 5678"
---

# Formatting phone numbers with pattern.


This example show you how to format phone numbers with a patter

You will need the following library in your gradle.

compile 'com.googlecode.libphonenumber:libphonenumber:7.2.2'



## Patterns + 1 (786) 1234 5678


Given a normalized phone number like +178612345678 we will get a formatted number with the provided pattern.

```java
private String getFormattedNumber(String phoneNumber) {
    
    PhoneNumberUtil phoneNumberUtil = PhoneNumberUtil.getInstance();
    
    Phonemetadata.NumberFormat numberFormat = new Phonemetadata.NumberFormat();
    
    numberFormat.pattern = "(\\d{3})(\\d{3})(\\d{4})";
    
    numberFormat.format = "($1) $2-$3";
    
    List<Phonemetadata.NumberFormat> newNumberFormats = new ArrayList<>();
    
    newNumberFormats.add(numberFormat);
    
    Phonenumber.PhoneNumber phoneNumberPN = null;
    
    try {
        phoneNumberPN = phoneNumberUtil.parse(phoneNumber, Locale.US.getCountry());
        phoneNumber = phoneNumberUtil.formatByPattern(phoneNumberPN, PhoneNumberUtil.PhoneNumberFormat.INTERNATIONAL, newNumberFormats);
        
    } catch (NumberParseException e) {
        e.printStackTrace();
    }
    
    return phoneNumber;
}

```

