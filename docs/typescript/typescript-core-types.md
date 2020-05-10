---
metaTitle: "TypeScript - TypeScript Core Types"
description: "String Literal Types, Tuple, Boolean, Intersection Types, Number, String, Array, Enum, Any, Void, Types in function arguments and return value. Number, Types in function arguments and return value. String, const Enum"
---

# TypeScript Core Types



## String Literal Types


String literal types allow you to specify the exact value a string can have.

```ts
let myFavoritePet: "dog";
myFavoritePet = "dog";

```

Any other string will give a error.

```ts
// Error: Type '"rock"' is not assignable to type '"dog"'.
// myFavoritePet = "rock";

```

Together with Type Aliases and Union Types you get a enum-like behavior.

```ts
type Species = "cat" | "dog" | "bird";

function buyPet(pet: Species, name: string) : Pet { /*...*/ }

buyPet(myFavoritePet /* "dog" as defined above */, "Rocky");

// Error: Argument of type '"rock"' is not assignable to parameter of type "'cat' | "dog" | "bird". Type '"rock"' is not assignable to type '"bird"'.
// buyPet("rock", "Rocky");

```

String Literal Types can be used to distinguish overloads.

```ts
function buyPet(pet: Species, name: string) : Pet;
function buyPet(pet: "cat", name: string): Cat;
function buyPet(pet: "dog", name: string): Dog;
function buyPet(pet: "bird", name: string): Bird;
function buyPet(pet: Species, name: string) : Pet { /*...*/ }

let dog = buyPet(myFavoritePet /* "dog" as defined above */, "Rocky");
// dog is from type Dog (dog: Dog)

```

They works well for User-Defined Type Guards.

```ts
interface Pet {
    species: Species;
    eat();
    sleep();
}

interface Cat extends Pet {
    species: "cat";
}

interface Bird extends Pet {
    species: "bird";
    sing();
}

function petIsCat(pet: Pet): pet is Cat {
    return pet.species === "cat";
}

function petIsBird(pet: Pet): pet is Bird {
    return pet.species === "bird";
}

function playWithPet(pet: Pet){
    if(petIsCat(pet)) {
        // pet is now from type Cat (pet: Cat)
        pet.eat();
        pet.sleep();
    } else if(petIsBird(pet)) {
        // pet is now from type Bird (pet: Bird)
        pet.eat();
        pet.sing();
        pet.sleep();
    }
}

```

Full example code

```ts
let myFavoritePet: "dog";
myFavoritePet = "dog";

// Error: Type '"rock"' is not assignable to type '"dog"'.
// myFavoritePet = "rock";

type Species = "cat" | "dog" | "bird";

interface Pet {
    species: Species;
    name: string;
    eat();
    walk();
    sleep();
}

interface Cat extends Pet {
    species: "cat";
}

interface Dog extends Pet {
    species: "dog";
}

interface Bird extends Pet {
    species: "bird";
    sing();
}

// Error: Interface 'Rock' incorrectly extends interface 'Pet'. Types of property 'species' are incompatible. Type '"rock"' is not assignable to type '"cat" | "dog" | "bird"'. Type '"rock"' is not assignable to type '"bird"'.
// interface Rock extends Pet { 
//      type: "rock"; 
// }

function buyPet(pet: Species, name: string) : Pet;
function buyPet(pet: "cat", name: string): Cat;
function buyPet(pet: "dog", name: string): Dog;
function buyPet(pet: "bird", name: string): Bird;
function buyPet(pet: Species, name: string) : Pet {
    if(pet === "cat") { 
        return {
            species: "cat",
            name: name,
            eat: function () {
                console.log(`${this.name} eats.`);
            }, walk: function () {
                console.log(`${this.name} walks.`);
            }, sleep: function () {
                console.log(`${this.name} sleeps.`);
            }
        } as Cat;
    } else if(pet === "dog") { 
        return {
            species: "dog",
            name: name,
            eat: function () {
                console.log(`${this.name} eats.`);
            }, walk: function () {
                console.log(`${this.name} walks.`);
            }, sleep: function () {
                console.log(`${this.name} sleeps.`);
            }
        } as Dog;
    } else if(pet === "bird") { 
        return {
            species: "bird",
            name: name,
            eat: function () {
                console.log(`${this.name} eats.`);
            }, walk: function () {
                console.log(`${this.name} walks.`);
            }, sleep: function () {
                console.log(`${this.name} sleeps.`);
            }, sing: function () {
                console.log(`${this.name} sings.`);
            }
        } as Bird;
    } else {
        throw `Sorry we don't have a ${pet}. Would you like to buy a dog?`;
    }
}

function petIsCat(pet: Pet): pet is Cat {
    return pet.species === "cat";
}

function petIsDog(pet: Pet): pet is Dog {
    return pet.species === "dog";
}

function petIsBird(pet: Pet): pet is Bird {
    return pet.species === "bird";
}

function playWithPet(pet: Pet) {
    console.log(`Hey ${pet.name}, let's play.`);
    
    if(petIsCat(pet)) {
        // pet is now from type Cat (pet: Cat)
        
        pet.eat();
        pet.sleep();
        
        // Error: Type '"bird"' is not assignable to type '"cat"'.
        // pet.type = "bird";
        
        // Error: Property 'sing' does not exist on type 'Cat'.
        // pet.sing();
        
    } else if(petIsDog(pet)) {
        // pet is now from type Dog (pet: Dog)
        
        pet.eat();
        pet.walk();
        pet.sleep();
        
    } else if(petIsBird(pet)) {
        // pet is now from type Bird (pet: Bird)
    
        pet.eat();
        pet.sing();
        pet.sleep();
    } else {
        throw "An unknown pet. Did you buy a rock?";
    }
}

let dog = buyPet(myFavoritePet /* "dog" as defined above */, "Rocky");
// dog is from type Dog (dog: Dog)

// Error: Argument of type '"rock"' is not assignable to parameter of type "'cat' | "dog" | "bird". Type '"rock"' is not assignable to type '"bird"'.
// buyPet("rock", "Rocky");

playWithPet(dog);
// Output: Hey Rocky, let's play.
//         Rocky eats.
//         Rocky walks.
//         Rocky sleeps.

```



## Tuple


Array type with known and possibly different types:

```ts
let day: [number, string];
day = [0, 'Monday'];      // valid
day = ['zero', 'Monday']; // invalid: 'zero' is not numeric
console.log(day[0]); // 0
console.log(day[1]); // Monday

day[2] = 'Saturday'; // valid: [0, 'Saturday']
day[3] = false;      // invalid: must be union type of 'number | string'

```



## Boolean


A boolean represents the most basic datatype in TypeScript, with the purpose of assigning true/false values.

```ts
// set with initial value (either true or false)
let isTrue: boolean = true;        

// defaults to 'undefined', when not explicitely set
let unsetBool: boolean;             

// can also be set to 'null' as well
let nullableBool: boolean = null;

```



## Intersection Types


A Intersection Type combines the member of two or more types.

```ts
interface Knife {
    cut();
}

interface BottleOpener{
    openBottle();
}

interface Screwdriver{
    turnScrew();
}

type SwissArmyKnife = Knife & BottleOpener & Screwdriver;

function use(tool: SwissArmyKnife){
    console.log("I can do anything!");
    
    tool.cut();
    tool.openBottle();
    tool.turnScrew();
}

```



## Number


Like JavaScript, numbers are floating point values.

```ts
let pi: number = 3.14;           // base 10 decimal by default
let hexadecimal: number = 0xFF;  // 255 in decimal

```

ECMAScript 2015 allows binary and octal.

```ts
let binary: number = 0b10;   // 2 in decimal
let octal: number = 0o755;   // 493 in decimal

```



## String


Textual data type:

```ts
let singleQuotes: string = 'single';
let doubleQuotes: string = "double";
let templateString: string = `I am ${ singleQuotes }`; // I am single

```



## Array


An array of values:

```ts
let threePigs: number[] = [1, 2, 3];
let genericStringArray: Array<string> = ['first', '2nd', '3rd'];

```



## Enum


A type to name a set of numeric values:

Number values default to 0:

```ts
enum Day { Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday };
let bestDay: Day = Day.Saturday;

```

Set a default starting number:

```ts
enum TenPlus { Ten = 10, Eleven, Twelve }

```

or assign values:

```ts
enum MyOddSet { Three = 3, Five = 5, Seven = 7, Nine = 9 } 

```



## Any


When unsure of a type, `any` is available:

```ts
let anything: any = 'I am a string';
anything = 5; // but now I am the number 5

```



## Void


If you have no type at all, commonly used for functions that do not return anything:

```ts
function log(): void {
    console.log('I return nothing');
}

```

`void` types Can only be assigned `null` or `undefined`.



## Types in function arguments and return value. Number


When you create a function in TypeScript you can specify the data type of the function's arguments and the data type for the return value

Example:

```ts
function sum(x: number, y: number): number {
    return x + y;
}

```

Here the syntax `x: number, y: number` means that the function can accept two arguments`x` and `y` and they can only be numbers and `(...): number {` means that the return value can only be a number

Usage:

```ts
sum(84 + 76) // will be return 160

```

Note:

You can not do so

```ts
function sum(x: string, y: string): number {
    return x + y;
}

```

or

```ts
function sum(x: number, y: number): string {
    return x + y;
}

```

it will receive the following errors:

`error TS2322: Type 'string' is not assignable to type 'number'` and `error TS2322: Type 'number' is not assignable to type 'string'` respectively



## Types in function arguments and return value. String


Example:

```ts
function hello(name: string): string {
    return `Hello ${name}!`;
}

```

Here the syntax `name: string` means that the function can accept one `name` argument and this argument can only be string and `(...): string {` means that the return value can only be a string

Usage:

```ts
hello('StackOverflow Documentation') // will be return Hello StackOverflow Documentation!

```



## const Enum


A const Enum is the same as a normal Enum. Except that no Object is generated at compile time. Instead, the literal values are substituted where the const Enum is used.

```ts
// Typescript: A const Enum can be defined like a normal Enum (with start value, specifig values, etc.)
const enum NinjaActivity {
    Espionage, 
    Sabotage, 
    Assassination
}

// Javascript: But nothing is generated    

// Typescript: Except if you use it
let myFavoriteNinjaActivity = NinjaActivity.Espionage;
console.log(myFavoritePirateActivity); // 0

// Javascript: Then only the number of the value is compiled into the code
// var myFavoriteNinjaActivity = 0 /* Espionage */;
// console.log(myFavoritePirateActivity); // 0

// Typescript: The same for the other constant example
console.log(NinjaActivity["Sabotage"]); // 1   

// Javascript: Just the number and in a comment the name of the value
// console.log(1 /* "Sabotage" */); // 1

// Typescript: But without the object none runtime access is possible
// Error: A const enum member can only be accessed using a string literal.
// console.log(NinjaActivity[myFavoriteNinjaActivity]);

```

For comparison, a normal Enum

```ts
// Typescript: A normal Enum
enum PirateActivity {
    Boarding,
    Drinking, 
    Fencing 
} 

// Javascript: The Enum after the compiling
// var PirateActivity;
// (function (PirateActivity) {
//     PirateActivity[PirateActivity["Boarding"] = 0] = "Boarding";
//     PirateActivity[PirateActivity["Drinking"] = 1] = "Drinking";
//     PirateActivity[PirateActivity["Fencing"] = 2] = "Fencing";
// })(PirateActivity || (PirateActivity = {}));

// Typescript: A normale use of this Enum
let myFavoritePirateActivity = PirateActivity.Boarding;
console.log(myFavoritePirateActivity); // 0

// Javascript: Looks quite similar in Javascript
// var myFavoritePirateActivity = PirateActivity.Boarding;
// console.log(myFavoritePirateActivity); // 0

// Typescript: And some other normale use
console.log(PirateActivity["Drinking"]); // 1

// Javascript: Looks quite similar in Javascript
// console.log(PirateActivity["Drinking"]); // 1

// Typescript: At runtime, you can access an normal enum
console.log(PirateActivity[myFavoritePirateActivity]); // "Boarding"

// Javascript: And it will be resolved at runtime
// console.log(PirateActivity[myFavoritePirateActivity]); // "Boarding"

```



#### Syntax


- let variableName: VariableType;
- function functionName(parameterName: VariableType, parameterWithDefault: VariableType = ParameterDefault, optionalParameter?: VariableType, ...variardicParameter: VariableType[]): ReturnType { /*...*/};

