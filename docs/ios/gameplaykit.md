---
metaTitle: "iOS - GameplayKit"
description: "Generating random numbers, GKEntity and GKComponent"
---

# GameplayKit



## Generating random numbers


Although `GameplayKit` (which is introduced with iOS 9 SDK) is about implementing game logic, it could also be used to generate random numbers, which is very useful in apps and games.

Beside the `GKRandomSource.sharedRandom` which is used in the following chapters there are three additional types of `GKRandomSource`'s out of the box.

- **GKARC4RandomSource** Which uses the the ARC4 algorithm
- **GKLinearCongruentialRandomSource** Which is a fast but not so random `GKRandomSource`
- **GKMersenneTwisterRandomSource** Which implements a MersenneTwister algorithm. It is slower but more random.

In the following chapter we only use the `nextInt()` method of a `GKRandomSource`.
In addition to this there is the `nextBool() -> Bool` and the `nextUniform() -> Float`

### Generation

First, import `GameplayKit`:

### Swift

```swift
import GameplayKit

```

### Objective-C

```swift
#import <GameplayKit/GameplayKit.h>

```

Then, to generate a random number, use this code:

### Swift

```swift
let randomNumber = GKRandomSource.sharedRandom().nextInt()

```

### Objective-C

```swift
int randomNumber = [[GKRandomSource sharedRandom] nextInt];

```

> 
<h3>Note</h3>
The nextInt() function, when used without parameters, will return a random number between -2,147,483,648 and 2,147,483,647, including themselves, so we are not sure that it is always a positive or non-zero number.


### Generating a number from 0 to n

To achieve this, you should give n to `nextIntWithUpperBound()` method:

### Swift

```swift
let randomNumber = GKRandomSource.sharedRandom().nextInt(upperBound: 10)

```

### Objective-C

```swift
int randomNumber = [[GKRandomSource sharedRandom] nextIntWithUpperBound: 10];

```

This code will give us a number between 0 and 10, including themselves.

### Generating a number from m to n

To do this you create a `GKRandomDistribution` object with a `GKRandomSource` and pass in the bounds. A `GKRandomDistribution` can be used to change the distribution behaviour like `GKGaussianDistribution` or `GKShuffledDistribution`.

After that the object can be used like every regular `GKRandomSource` since it does implement the `GKRandom` protocol too.

### Swift

```swift
let randomizer = GKRandomDistribution(randomSource: GKRandomSource(), lowestValue: 0, highestValue: 6)
let randomNumberInBounds = randomizer.nextInt()

```

### Objective-C **outdated**

```swift
int randomNumber = [[GKRandomSource sharedRandom] nextIntWithUpperBound: n - m] + m;

```

For example, to generate a random number between 3 and 10, you use this code:

### Swift

```swift
let randomNumber = GKRandomSource.sharedRandom().nextInt(upperBound: 7) + 3

```

### Objective-C **outdated**

```swift
int randomNumber = [[GKRandomSource sharedRandom] nextIntWithUpperBound: 7] + 3;

```



## GKEntity and GKComponent


An entity represents an object of a game like a player figure or an enemy figure. Since this object does not do much without arms and legs we can add the components to this. To create this system apple has the `GKEntity` and `GKComponent` classes.

Lets assume we have the following classe for the following chapters:

```swift
class Player: GKEntity{}
class PlayerSpriteComponent: GKComponent {}

```

### GKEntity

An entity is a collection of components and offers several functions to add, remove and interact with components of it.

While we could just use the GKEntity it is common to Subclass it for a specific type of game entity.

It is important that it is only possible to add a component of a class once. In case you add a second component of the same class it will override the first exsisting component inside of the `GKEntity`

```swift
let otherComponent = PlayerSpriteComponent()
var player = Player()
player.addComponent(PlayerSpriteComponent())
player.addComponent(otherComponent)
print(player.components.count) //will print 1
print(player.components[0] === otherComponent) // will print true

```

You may ask why. The reason for this is the methods called `component(for: T.Type)` which returns the component of a specific type of the entity.

```swift
let component = player.component(ofType: PlayerSpriteComponent.self) 

```

In addition to the components-methods it has an `update` method which is used to delegate the delta time or current time of the game logic to it's components.

```swift
var player = Player()
player.addComponent(PlayerSpriteComponent())
player.update(deltaTime: 1.0) // will call the update method of the PlayerSpriteComponent added to it

```

### GKComponent

A component represents something of an entity for example the visual component or the logic component.

If an the update method of an entity is called it will delegate this to all of it's components. Overriding this method is used to manipulate an Entity.

```swift
class PlayerSpriteComponent: GKComponent {
    override func update(deltaTime seconds: TimeInterval) {
        //move the sprite depending on the update time
    }
}

```

In addition to this it is possible to override the method `didAddToEntity` and `willRemoveFromEntity` to inform other components about it's removal or add.

To manipulate a other component inside of a component it is possible to get the GKEntity which the component is added to.

```swift
override func update(deltaTime seconds: TimeInterval) {
    let controller = self.entity?.component(ofType: PlayerControlComponent.self)
    //call methods on the controller
}

```

While this is possible it is **not** a common pattern since it wires the two components together.

### GKComponentSystem

While we just talked about using the update delegate mechanism of the `GKEntity` to update the `GKComponents` there is a different way to update `GKComponents` which is called `GKComponentSystem`.

It is used in case it is needed that all components of a specific type need to be updated in one go.

A `GKComponentSystem` is created for a specific type of component.

```swift
let system = GKComponentSystem(componentClass: PlayerSpriteComponent.self)

```

To add a component you can use the add method:

```swift
system.addComponent(PlayerSpriteComponent())

```

But a more common way is to pass the created entity with it's components to the `GKComponentSystem` and it will find a matching component inside of the entity.

```swift
system.addComponent(foundIn: player)

```

To update all components of a specific type call the update:

```swift
system.update(deltaTime: delta)

```

In case you want to use the `GKComponentSystem` instead of a entity based update mechanism you have to have a `GKComponentSystem` for every component and call the update on all of the systems.

