---
metaTitle: "Dynamic data masking"
description: "Mask email address using Dynamic data masking, Add partial mask on column, Showing random value from the range using random() mask, Adding default mask on the column, Controlling who can see unmasked data"
---

# Dynamic data masking



## Mask email address using Dynamic data masking


If you have email column you can mask it with email() mask:

```sql
ALTER TABLE  Company
ALTER COLUMN Email ADD MASKED WITH (FUNCTION = 'email()')

```

When user tries to select emails from Company table, he will get something like the following values:

mXXX@XXXX.com

zXXX@XXXX.com

rXXX@XXXX.com



## Add partial mask on column


You can add partial mask on the column that will show few characters from te beginning and the end of the string and show mask instead of the characters in the middle:

```sql
ALTER TABLE  Company
ALTER COLUMN Phone ADD MASKED WITH (FUNCTION = 'partial(5,"XXXXXXX",2)')

```

In the parameters of the partial function you can specify how many values from the beginning will be shown, how many values from the end will be shown, and what woudl be the pattern that is shown in the middle.

When user tries to select emails from Company table, he will get something like the following values:

(381)XXXXXXX39

(360)XXXXXXX01

(415)XXXXXXX05



## Showing random value from the range using random() mask


Random mask will show a rundom number from the specified range instead of the actual value:

```sql
ALTER TABLE  Product
ALTER COLUMN Price ADD MASKED WITH (FUNCTION = 'random(100,200)')

```

Note that is some cases displayed value might match actual value in column (if randomly selected number matches value in the cell).



## Adding default mask on the column


If you add default mask on the column, instead of actual value in SELECT statement will be shown mask:

```sql
ALTER TABLE  Company
ALTER COLUMN Postcode ADD MASKED WITH (FUNCTION = 'default()')

```



## Controlling who can see unmasked data


You can grant in-privileged users right to see unmasked values using the following statement:

```sql
GRANT UNMASK TO MyUser

```

If some user already has unmask permission, you can revoke this permission:

```sql
REVOKE UNMASK TO MyUser

```

