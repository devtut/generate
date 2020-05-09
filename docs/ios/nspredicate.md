---
metaTitle: "iOS - NSPredicate"
description: "Form validation using NSPredicate, Creating an NSPredicate Using predicateWithBlock, Creating an NSPredicate Using predicateWithFormat, Creating an NSPredicate with Substitution Variables, Using NSPredicate to Filter an Array, NSPredicate with `AND`, `OR` and `NOT` condition"
---

# NSPredicate



## Form validation using NSPredicate


```swift
NSString *emailRegex = @"[A-Z0-9a-z]([A-Z0-9a-z._-]{0,64})+[A-Z0-9a-z]+@[A-Z0-9a-z]+([A-Za-z0-9.-]{0,64})+([A-Z0-9a-z])+\\.[A-Za-z]{2,4}";    NSString *firstNameRegex = @"[0-9A-Za-z\"'-]{2,32}$";
NSString *firstNameRegex = @"[ 0-9A-Za-z]{2,32}$";
NSString *lastNameRegex = @"[0-9A-Za-z\"'-]{2,32}$";
NSString *mobileNumberRegEx = @"^[0-9]{10}$";
NSString *zipcodeRegEx = @"^[0-9]{5}$";
NSString *SSNRegEx = @"^\\d{3}-?\\d{2}-?\\d{4}$";
NSString *addressRegEx = @"^[ A-Za-z0-9]{2,32}$";
NSString *cityRegEx = @"^[ A-Za-z0-9]{2,25}$";
NSString *PINRegEx = @"^[0-9]{4}$";
NSString *driversLiscRegEx = @"^[0-9a-zA-Z]{5,20}$";

-(BOOL)validateEmail {
    //Email address field should give an error when the email address begins with ".","-","_" .
    NSPredicate *emailPredicate = [NSPredicate predicateWithFormat:@"SELF MATCHES %@", emailRegex];   
    return ([emailPredicate evaluateWithObject:self.text] && self.text.length <= 64 && ([self.text rangeOfString:@".."].location == NSNotFound));
}

- (BOOL)validateFirstName {
    NSPredicate *firstNamePredicate = [NSPredicate predicateWithFormat:@"SELF MATCHES %@", firstNameRegex];
    return [firstNamePredicate evaluateWithObject:self.text];
}

- (BOOL)validateLastName {
    NSPredicate *lastNamePredicate = [NSPredicate predicateWithFormat:@"SELF MATCHES %@", lastNameRegex];
    return [lastNamePredicate evaluateWithObject:self.text];
}

- (BOOL)validateAlphaNumericMin2Max32 {
    NSPredicate *firstNamePredicate = [NSPredicate predicateWithFormat:@"SELF MATCHES %@", firstNameRegex];
    return [firstNamePredicate evaluateWithObject:self.text];
}

- (BOOL)validateMobileNumber {
    NSString *strippedMobileNumber =  [[[[self.text stringByReplacingOccurrencesOfString:@"(" withString:@""]
                                        stringByReplacingOccurrencesOfString:@")" withString:@""]
                                        stringByReplacingOccurrencesOfString:@"-" withString:@""]
                                        stringByReplacingOccurrencesOfString:@" " withString:@""];
    
    NSPredicate *mobileNumberPredicate = [NSPredicate predicateWithFormat:@"SELF MATCHES %@", mobileNumberRegEx];
    
    return [mobileNumberPredicate evaluateWithObject:strippedMobileNumber];
}

- (BOOL)validateZipcode {
    NSPredicate *zipcodePredicate = [NSPredicate predicateWithFormat:@"SELF MATCHES %@", zipcodeRegEx];
    
    return [zipcodePredicate evaluateWithObject:self.text];
}

- (BOOL)validateSSN {
NSPredicate *predicate = [NSPredicate predicateWithFormat:@"SELF MATCHES %@", SSNRegEx];

return [predicate evaluateWithObject:self.text];
}

- (BOOL)validateAddress {
    NSPredicate *predicate = [NSPredicate predicateWithFormat:@"SELF MATCHES %@", addressRegEx];
    
    return [predicate evaluateWithObject:self.text];
}

- (BOOL)validateCity {
    NSPredicate *predicate = [NSPredicate predicateWithFormat:@"SELF MATCHES %@", cityRegEx];
    return [predicate evaluateWithObject:self.text];
}

- (BOOL)validatePIN {
    NSPredicate *predicate = [NSPredicate predicateWithFormat:@"SELF MATCHES %@", PINRegEx];    
    return [predicate evaluateWithObject:self.text];
}
   - (BOOL)validateDriversLiscNumber {
    if([self.text length] > 20) {
        return NO;
    }
    NSPredicate *driversLiscPredicate = [NSPredicate predicateWithFormat:@"SELF MATCHES %@", driversLiscRegEx];
    
    return [driversLiscPredicate evaluateWithObject:self.text];
}

```



## Creating an NSPredicate Using predicateWithBlock


### Objective-C

```swift
NSPredicate *predicate = [NSPredicate predicateWithBlock:^BOOL(id item, 
                                                               NSDictionary *bindings) {
    return [item isKindOfClass:[UILabel class]];
}];

```

### Swift

```swift
let predicate = NSPredicate { (item, bindings) -> Bool in
    return item.isKindOfClass(UILabel.self)
}

```

In this example, the predicate will match items that are of the class `UILabel`.



## Creating an NSPredicate Using predicateWithFormat


### Objective-C

```swift
NSPredicate *predicate = [NSPredicate predicateWithFormat: @"self[SIZE] = %d", 5)];

```

### Swift

```swift
let predicate = NSPredicate(format: "self[SIZE] >= %d", 5)

```

In this example, the predicate will match items that are arrays with length of at least 5.



## Creating an NSPredicate with Substitution Variables


An `NSPredicate` can use substitution variables to allow values to be bound on the fly.

### Objective-C

```swift
NSPredicate *template = [NSPredicate predicateWithFormat: @"self BEGINSWITH $letter"];
NSDictionary *variables = @{ @"letter": @"r" };
NSPredicate *beginsWithR = [template predicateWithSubstitutionVariables: variables];

```

### Swift

```swift
let template = NSPredicate(format: "self BEGINSWITH $letter")
let variables = ["letter": "r"]
let beginsWithR = template.predicateWithSubstitutionVariables(variables)

```

The template predicate is not modified by `predicateWithSubstitutionVariables`. Instead, a copy is created, and that copy receives the substitution variables.



## Using NSPredicate to Filter an Array


### Objective-C

```swift
NSArray *heroes = @[@"tracer", @"bastion", @"reaper", @"junkrat", @"roadhog"];

NSPredicate *template = [NSPredicate predicateWithFormat:@"self BEGINSWITH $letter"];

NSDictionary *beginsWithRVariables = @{ @"letter": @"r"};
NSPredicate *beginsWithR = [template predicateWithSubstitutionVariables: beginsWithRVariables];

NSArray *beginsWithRHeroes = [heroes filteredArrayUsingPredicate: beginsWithR];
// ["reaper", "roadhog"]

NSDictionary *beginsWithTVariables = @{ @"letter": @"t"};
NSPredicate *beginsWithT = [template predicateWithSubstitutionVarables: beginsWithTVariables];

NSArray *beginsWithTHeroes = [heroes filteredArrayUsingPredicate: beginsWithT];
// ["tracer"]

```

### Swift

```swift
let heroes = ["tracer", "bastion", "reaper", "junkrat", "roadhog"]

let template = NSPredicate(format: "self BEGINSWITH $letter")

let beginsWithRVariables = ["letter": "r"]
let beginsWithR = template.predicateWithSubstitutionVariables(beginsWithRVariables)

let beginsWithRHeroes = heroes.filter { beginsWithR.evaluateWithObject($0) }
// ["reaper", "roadhog"]

let beginsWithTVariables = ["letter": "t"]
let beginsWithT = template.predicateWithSubstitutionVariables(beginsWithTVariables)

let beginsWithTHeroes = heroes.filter { beginsWithT.evaluateWithObject($0) }
// ["tracer"]

```



## NSPredicate with `AND`, `OR` and `NOT` condition


Conditional predicate will be cleaner and safer by using the `NSCompoundPredicate` class which provides basic boolean operators for the given predicates.

### Objective-c

### AND - Condition

```

 NSPredicate *predicate = [NSPredicate predicateWithFormat:@"samplePredicate"];
  NSPredicate *anotherPredicate = [NSPredicate predicateWithFormat:@"anotherPredicate"];
  NSPredicate *combinedPredicate = [NSCompoundPredicate andPredicateWithSubpredicates: @[predicate,anotherPredicate]];

```

### OR - Condition

```

NSPredicate *predicate = [NSPredicate predicateWithFormat:@"samplePredicate"];
 NSPredicate *anotherPredicate = [NSPredicate predicateWithFormat:@"anotherPredicate"];
 NSPredicate *combinedPredicate = [NSCompoundPredicate orPredicateWithSubpredicates: @[predicate,anotherPredicate]];

```

### NOT - Condition

```

NSPredicate *predicate = [NSPredicate predicateWithFormat:@"samplePredicate"];
 NSPredicate *anotherPredicate = [NSPredicate predicateWithFormat:@"anotherPredicate"];
 NSPredicate *combinedPredicate = [NSCompoundPredicate notPredicateWithSubpredicate: @[predicate,anotherPredicate]];

```



#### Syntax


<li>Predicate Format String Substitions
<ul>
- C format string specifiers: %d, %s, %f, etc
- Object substitution: %@
- Keypath substitution: %K

- =, ==: Left-hand expression equals right-hand expression
- >=, =>: Left-hand expression is greater than or equal to right-hand expression
- <=, =<: Left-hand expression is less than or equal to right-hand expression
- >: Left-hand expression is greater than right-hand expression
- <: Left-hand expression is less than right-hand expression
- !=, <>: Left-hand expression is not equal to right-hand expression
- BETWEEN: Left-hand expression is between or equal to either of the values in the right-hand expression, which specifies lower and upper bounds - ex: BETWEEN { 0, 5 }

- AND, &&: Logical AND
- OR, ||: Logical OR
- NOT, !: Logical NOT

- BEGINSWITH: Left-hand expression begins with right-hand expression
- ENDSWITH: Left-hand expression ends with right-hand expression
- CONTAINS: Left-hand expression contains right-hand expression
<li>LIKE: Left-hand expression equals right-hand expression, with wildcard substitution
<ul>
- *: Match zero or more characters
- ?: Match one character

