---
metaTitle: "Angular 2 - Barrel"
description: "Using Barrel"
---

# Barrel


A barrel is a way to rollup exports from several ES2015 modules into a single convenience ES2015 module. The barrel itself is an ES2015 module file that re-exports selected exports of other ES2015 modules.



## Using Barrel


For example without a barrel, a consumer would need three import statements:

```js
import { HeroComponent } from '../heroes/hero.component.ts';                                
import { Hero }          from '../heroes/hero.model.ts';                                      
import { HeroService }   from '../heroes/hero.service.ts';

```

We can add a barrel by creating a file in the same component folder. In this case the folder is called 'heroes' named index.ts (using the conventions) that exports all of these items:

```js
export * from './hero.model.ts';   // re-export all of its exports
export * from './hero.service.ts'; // re-export all of its exports                           
export { HeroComponent } from './hero.component.ts'; // re-export the named thing

```

Now a consumer can import what it needs from the barrel.<br>
`import { Hero, HeroService } from '../heroes/index';`

Still, this can become a very long line; which could be reduced further.

```js
import * as h from '../heroes/index';

```

That's pretty reduced! The `* as h` imports all of the modules and aliases as h

