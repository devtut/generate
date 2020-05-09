---
metaTitle: "React Native - Render Best Practises"
description: "Functions in JSX"
---

# Render Best Practises


Topic for important notes about specific Component.render method behavoir.



## Functions in JSX


For better performance it's important to avoid using of array (lambda) function in JSX.

As explained at [https://github.com/yannickcr/eslint-plugin-react/blob/master/docs/rules/jsx-no-bind.md](https://github.com/yannickcr/eslint-plugin-react/blob/master/docs/rules/jsx-no-bind.md) :

> 
A bind call or arrow function in a JSX prop will create a brand new function on every single render. This is bad for performance, as it will result in the garbage collector being invoked way more than is necessary. It may also cause unnecessary re-renders if a brand new function is passed as a prop to a component that uses reference equality check on the prop to determine if it should update.


So if have jsx code block like this:

```

                   <TextInput
                      onChangeValue={  value => this.handleValueChanging(value) }
                    />

```

or

```

                   <button onClick={ this.handleClick.bind(this) }></button>

```

you can make it better:

```

                   <TextInput
                      onChangeValue={  this.handleValueChanging }
                    />

```

and

```

                   <button onClick={ this.handleClick }></button>

```

For correct context within handleValueChanging function you can apply it in constructor of component:

```

         constructor(){
            this.handleValueChanging = this.handleValueChanging.bind(this)
          }

```

more in [binding a function passed to a component](https://stackoverflow.com/questions/35446486/binding-a-function-passed-to-a-component)

Or you can use solutions like this: [https://github.com/andreypopp/autobind-decorator](https://github.com/andreypopp/autobind-decorator)
and simply add @autobind decorator to each methos that you want bind to:

```

       @autobind
        handleValueChanging(newValue)
        {
            //processing event
        }

```

