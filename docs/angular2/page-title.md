---
metaTitle: "Angular 2 - Page title"
description: "changing the page title"
---

# Page title


How can you change the title of the page



## changing the page title


1. First we need to provide Title service.
1. Using setTitle

```js
import {Title} from "@angular/platform-browser"; 
@Component({
  selector: 'app',
  templateUrl: './app.component.html',
  providers : [Title]
})

export class AppComponent implements {
   constructor( private title: Title) { 
     this.title.setTitle('page title changed');
   }
}

```



#### Syntax


- `setTitle(newTitle: string): void;`
- `getTitle(): string;`

