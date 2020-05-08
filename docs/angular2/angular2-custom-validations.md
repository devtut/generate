---
metaTitle: "Angular 2 - Angular2 Custom Validations"
description: "get/set formBuilder controls parameters, Custom validator examples:, Using validators in the Formbuilder"
---

# Angular2 Custom Validations



## get/set formBuilder controls parameters


There are 2 ways to set formBuilder controls parameters.

1. On initialize:

```js
exampleForm : FormGroup;
constructor(fb: FormBuilder){
  this.exampleForm = fb.group({
      name : new FormControl({value: 'default name'}, Validators.compose([Validators.required, Validators.maxLength(15)]))
   });
}

```

2.After initialize:

```js
this.exampleForm.controls['name'].setValue('default name');

```

Get formBuilder control value:

```js
let name = this.exampleForm.controls['name'].value();

```



## Custom validator examples:


Angular 2 has two kinds of custom validators. Synchronous validators as in the first example that will run directly on the client and asynchronous validators (the second example) that you can use to call a remote service to do the validation for you. In this example the validator should call the server to see if a value is unique.

```js
export class CustomValidators {

static cannotContainSpace(control: Control) {
    if (control.value.indexOf(' ') >= 0)
        return { cannotContainSpace: true };

    return null;
}

static shouldBeUnique(control: Control) {
    return new Promise((resolve, reject) => {
        // Fake a remote validator.
        setTimeout(function () {
            if (control.value == "exisitingUser")
                resolve({ shouldBeUnique: true });
            else
                resolve(null);
        }, 1000);
    });
}}

```

If your control value is valid you simply return null to the caller. Otherwise you can return an object which describes the error.



## Using validators in the Formbuilder


```

  constructor(fb: FormBuilder) {
    this.form = fb.group({
        firstInput: ['', Validators.compose([Validators.required, CustomValidators.cannotContainSpace]), CustomValidators.shouldBeUnique],
        secondInput: ['', Validators.required]
    });
}

```

Here we use the FormBuilder to create a very basic form with two input boxes. The FromBuilder takes an array for three arguments for each input control.

1. The default value of the control.
<li>The validators that will run on the client. You can use
Validators.compose([arrayOfValidators]) to apply multiple validators
on your control.</li>
1. One or more async validators in a similar fashion as the second argument.



#### Parameters


|parameter|description
|---|---|---|---|---|---|---|---|---|---
|control|This is the control that is being validated. Typically you will want to see if control.value meets some criteria.

