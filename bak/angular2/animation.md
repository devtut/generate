---
metaTitle: "Angular 2 - Animation"
description: "Transition between null states, Animating between multiple states"
---

# Animation




## Transition between null states


```

   @Component({
        ...
        animations: [
            trigger('appear', [
                transition(':enter', [
                    style({
                      //style applied at the start of animation
                    }),
                animate('300ms ease-in', style({
                    //style applied at the end of animation
                }))
                ])
            ])
        ]
    })
    class AnimComponent {

    }
]

```



## Animating between multiple states


The `<div>` in this template grows to `50px` and then `100px` and then shrinks back to `20px` when you click the button.

Each `state` has an associated style described in the `@Component` metadata.

The logic for whichever `state` is active can be managed in the component logic. In this case, the component variable `size` holds the string value "small", "medium" or "large".

The `<div>` element respond to that value through the `trigger` specified in the `@Component` metadata: `[@size]="size"`.

```js
@Component({
  template: '<div [@size]="size">Some Text</div><button (click)="toggleSize()">TOGGLE</button>',
  animations: [
    trigger('size', [
      state('small', style({
        height: '20px'
      })),
      state('medium', style({
        height: '50px'
      })),
      state('large', style({
        height: '100px'
      })),
      transition('small => medium', animate('100ms')),
      transition('medium => large', animate('200ms')),
      transition('large => small', animate('300ms'))
    ])
  ]
})
export class TestComponent {

    size: string;

    constructor(){
        this.size = 'small';
    }
    toggleSize(){
        switch(this.size) {
            case 'small':
                this.size = 'medium';
                break;
            case 'medium':
                this.size = 'large';
                break;
            case 'large':
                this.size = 'small';
        }
    }
}

```

