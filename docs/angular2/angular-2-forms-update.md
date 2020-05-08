---
metaTitle: "Angular 2 - Angular 2 Forms Update"
description: "Angular 2 : Template Driven Forms, Angular 2 Form - Custom Email/Password Validation, Simple Password Change Form with Multi Control Validation, Angular 2 Forms ( Reactive Forms ) with registration form and confirm password validation, Angular 2: Reactive Forms (a.k.a Model-driven Forms), Angular2 - Form Builder"
---

# Angular 2 Forms Update



## Angular 2 : Template Driven Forms


```js
import { Component } from '@angular/core';
import { Router , ROUTER_DIRECTIVES} from '@angular/router';
import { NgForm }    from '@angular/forms';

@Component({
    selector: 'login',
    template: `
<h2>Login</h2>
<form #f="ngForm" (ngSubmit)="login(f.value,f.valid)" novalidate>
    <div>
        <label>Username</label>
        <input type="text" [(ngModel)]="username" placeholder="enter username" required>
    </div>
    <div>
        <label>Password</label>
        <input type="password" name="password" [(ngModel)]="password" placeholder="enter password" required>
    </div>
    <input class="btn-primary" type="submit" value="Login">
</form>`
   //For long form we can use **templateUrl** instead of template
})

export class LoginComponent{

    constructor(private router : Router){ }

    login (formValue: any, valid: boolean){
        console.log(formValue);
        
        if(valid){
            console.log(valid);
        }
    }    
}

```



## Angular 2 Form - Custom Email/Password Validation


For live demo [click..](http://embed.plnkr.co/AtW3FrYU3qyNsWtLfUUF/)

**App index ts**

```js
import {bootstrap} from '@angular/platform-browser-dynamic';
import {MyForm} from './my-form.component.ts';

bootstrap(MyForm);

```

**Custom validator**

```js
import {Control} from @'angular/common';

export class CustomValidators {
  static emailFormat(control: Control): [[key: string]: boolean] {
    let pattern:RegExp = /\S+@\S+\.\S+/;
    return pattern.test(control.value) ? null : {"emailFormat": true};
  }
}

```

**Form Components ts**

```js
import {Component} from '@angular/core';
import {FORM_DIRECTIVES, NgForm, FormBuilder, Control, ControlGroup, Validators} from '@angular/common';
import {CustomValidators} from './custom-validators';

@Component({
  selector: 'my-form',
  templateUrl: 'app/my-form.component.html',
  directives: [FORM_DIRECTIVES],
  styleUrls: ['styles.css']
})
export class MyForm {
  email: Control;
  password: Control;
  group: ControlGroup;
  
  constructor(builder: FormBuilder) {
    this.email = new Control('', 
      Validators.compose([Validators.required, CustomValidators.emailFormat])
    );
    
    this.password = new Control('',
      Validators.compose([Validators.required, Validators.minLength(4)])
    );
    
    this.group = builder.group({
      email: this.email,
      password: this.password
    });
  }
  
  onSubmit() {
    console.log(this.group.value);
  }
}

```

Form Components HTML

```js
<form [ngFormModel]="group" (ngSubmit)="onSubmit()" novalidate>

  <div>
    <label for="email">Email:</label>
    <input type="email" id="email" [ngFormControl]="email">
    
    <ul *ngIf="email.dirty && !email.valid">
      <li *ngIf="email.hasError('required')">An email is required</li>
    </ul>
  </div>

  <div>
    <label for="password">Password:</label>
    <input type="password" id="password" [ngFormControl]="password">
    
    <ul *ngIf="password.dirty && !password.valid">
      <li *ngIf="password.hasError('required')">A password is required</li>
      <li *ngIf="password.hasError('minlength')">A password needs to have at least 4 characterss</li>
    </ul>
  </div>

  <button type="submit">Register</button>

</form>

```



## Simple Password Change Form with Multi Control Validation


The below examples use the new form API introduced in RC3.

### pw-change.template.html

```js
<form class="container" [formGroup]="pwChangeForm">
    <label for="current">Current Password</label>
    <input id="current" formControlName="current" type="password" required><br />

    <label for="newPW">New Password</label>
    <input id="newPW" formControlName="newPW" type="password" required><br/>
    <div *ngIf="newPW.touched && newPW.newIsNotOld">
        New password can't be the same as current password.
    </div>

    <label for="confirm">Confirm new password</label>
    <input id="confirm" formControlName="confirm" type="password" required><br />
    <div *ngIf="confirm.touched && confirm.errors.newMatchesConfirm">
        The confirmation does not match.
    </div>

    <button type="submit">Submit</button>
</form>

```

### pw-change.component.ts

```js
import {Component} from '@angular/core'
import {REACTIVE_FORM_DIRECTIVES, FormBuilder, AbstractControl, FormGroup, 
    Validators} from '@angular/forms'
import {PWChangeValidators} from './pw-validators'

@Component({
    moduleId: module.id
    selector: 'pw-change-form',
    templateUrl: `./pw-change.template.html`,
    directives: [REACTIVE_FORM_DIRECTIVES]
})

export class PWChangeFormComponent {
    pwChangeForm: FormGroup;

    // Properties that store paths to FormControls makes our template less verbose
    current: AbstractControl;
    newPW: AbstractControl;
    confirm: AbstractControl;

    constructor(private fb: FormBuilder) { }
    ngOnInit() {
        this.pwChangeForm = this.fb.group({
            current: ['', Validators.required],
            newPW: ['', Validators.required],
            confirm: ['', Validators.required]
        }, {
            // Here we create validators to be used for the group as a whole
            validator: Validators.compose([
                PWChangeValidators.newIsNotOld, 
                PWChangeValidators.newMatchesConfirm
            ])
        );
        this.current = this.pwChangeForm.controls['current'];
        this.newPW = this.pwChangeForm.controls['newPW'];
        this.confirm = this.pwChangeForm.controls['confirm'];
    }
}

```

### pw-validators.ts

```js
import {FormControl, FormGroup} from '@angular/forms'
export class PWChangeValidators {

    static OldPasswordMustBeCorrect(control: FormControl) {
        var invalid = false;
        if (control.value != PWChangeValidators.oldPW)
            return { oldPasswordMustBeCorrect: true }
        return null;
    }
   
    // Our cross control validators are below
    // NOTE: They take in type FormGroup rather than FormControl 
    static newIsNotOld(group: FormGroup){
        var newPW = group.controls['newPW'];
        if(group.controls['current'].value == newPW.value)
            newPW.setErrors({ newIsNotOld: true });
        return null;
    }

    static newMatchesConfirm(group: FormGroup){
        var confirm = group.controls['confirm'];
        if(group.controls['newPW'].value !== confirm.value)
            confirm.setErrors({ newMatchesConfirm: true });
        return null;
    }
}

```

A gist including some bootstrap classes can be found [here](https://gist.github.com/christopher-taylor/17044e105263ec424517d8fefd51791e).



## Angular 2 Forms ( Reactive Forms ) with registration form and confirm password validation


### app.module.ts

Add these into your app.module.ts file to use reactive forms

```js
import { NgModule } from '@angular/core';
    import { BrowserModule } from '@angular/platform-browser';
    import { FormsModule, ReactiveFormsModule } from '@angular/forms';
    import { AppComponent } from './app.component';
    @NgModule({
      imports: [
        BrowserModule,
        FormsModule,
        ReactiveFormsModule,
     ],
    declarations: [
    AppComponent
     ]
  providers: [],
  bootstrap: [
    AppComponent
   ]
  })
export class AppModule {}

```

### app.component.ts

```js
import { Component,OnInit } from '@angular/core';
import template from './add.component.html';
import { FormGroup,FormBuilder,Validators } from '@angular/forms';
import { matchingPasswords } from './validators';
@Component({
    selector: 'app',
    template
})
export class AppComponent implements OnInit {
    addForm: FormGroup;
    constructor(private formBuilder: FormBuilder) {
    }
    ngOnInit() {
  
    this.addForm = this.formBuilder.group({
            username: ['', Validators.required],
            email: ['', Validators.required],
            role: ['', Validators.required],
            password: ['', Validators.required],
            password2: ['', Validators.required] }, 
          { validator: matchingPasswords('password', 'password2')
        })
    };

addUser() {
        if (this.addForm.valid) {
            var adduser = {
                username: this.addForm.controls['username'].value,
                email: this.addForm.controls['email'].value,
                password: this.addForm.controls['password'].value,
                profile: {
                    role: this.addForm.controls['role'].value,
                    name: this.addForm.controls['username'].value,
                    email: this.addForm.controls['email'].value
                }
            };
          console.log(adduser);// adduser var contains all our form values. store it where you want 
            this.addForm.reset();// this will reset our form values to null 
        }
    }  
}

```

### app.component.html

```js
<div>
  <form [formGroup]="addForm">
   <input type="text" placeholder="Enter username" formControlName="username" />
   <input type="text" placeholder="Enter Email Address" formControlName="email"/>
   <input type="password" placeholder="Enter Password" formControlName="password" />
   <input type="password" placeholder="Confirm Password" name="password2" formControlName="password2"/>
   <div class='error' *ngIf="addForm.controls.password2.touched">
    <div class="alert-danger errormessageadduser" *ngIf="addForm.hasError('mismatchedPasswords')">                                  Passwords do not match
  </div>
</div>
<select name="Role" formControlName="role">
    <option value="admin" >Admin</option>
    <option value="Accounts">Accounts</option>
    <option value="guest">Guest</option>
</select>
<br/>
<br/>
<button type="submit" (click)="addUser()"><span><i class="fa fa-user-plus" aria-hidden="true"></i></span> Add User </button>
</form>
</div>

```

### validators.ts

```js
export function matchingPasswords(passwordKey: string, confirmPasswordKey: string) {
    return (group: ControlGroup): {
        [key: string]: any
    } => {
        let password = group.controls[passwordKey];
        let confirmPassword = group.controls[confirmPasswordKey];

        if (password.value !== confirmPassword.value) {
            return {
                mismatchedPasswords: true
            };
        }
    }
}

```



## Angular 2: Reactive Forms (a.k.a Model-driven Forms)


This example uses Angular 2.0.0 Final Release

### registration-form.component.ts

```js
import { FormGroup,
    FormControl,
    FormBuilder,
    Validators }              from '@angular/forms';

@Component({
    templateUrl: "./registration-form.html"
})
export class ExampleComponent {
    constructor(private _fb: FormBuilder) { }

exampleForm = this._fb.group({
    name: ['DefaultValue', [<any>Validators.required, <any>Validators.minLength(2)]],
    email: ['default@defa.ult', [<any>Validators.required, <any>Validators.minLength(2)]]
})

```

### registration-form.html

```js
<form [formGroup]="exampleForm" novalidate (ngSubmit)="submit(exampleForm)">
    <label>Name: </label>
    <input type="text" formControlName="name"/>
    <label>Email: </label>
    <input type="email" formControlName="email"/>
    <button type="submit">Submit</button>
</form>

```



## Angular2 - Form Builder


FormComponent.ts

```js
import {Component} from "@angular/core";
import {FormBuilder} from "@angular/forms";

@Component({
 selector: 'app-form',
 templateUrl: './form.component.html',
 styleUrls: ['./form.component.scss'],
 providers : [FormBuilder]
})

export class FormComponent{
  form : FormGroup;
  emailRegex = /^\w+([\.-]?\w+)*@\w+([\.-]?\w+)*(\.\w{2,3})+$/;

  constructor(fb: FormBuilder) {

    this.form = fb.group({
      FirstName : new FormControl({value: null}, Validators.compose([Validators.required, Validators.maxLength(15)])),
      LastName : new FormControl({value: null}, Validators.compose([Validators.required, Validators.maxLength(15)])),
      Email : new FormControl({value: null}, Validators.compose([
        Validators.required,
        Validators.maxLength(15),
        Validators.pattern(this.emailRegex)]))
    });
  }
}

```

form.component.html

```js
<form class="form-details" role="form" [formGroup]="form">
    <div class="row input-label">
      <label class="form-label" for="FirstName">First name</label>
      <input
        [formControl]="form.controls['FirstName']"
        type="text"
        class="form-control"
        id="FirstName"
        name="FirstName">
    </div>
    <div class="row input-label">
      <label class="form-label" for="LastName">Last name</label>
      <input
        [formControl]="form.controls['LastName']"
        type="text"
        class="form-control"
        id="LastName"
        name="LastName">
    </div>
    <div class="row">
      <label class="form-label" for="Email">Email</label>
      <input
        [formControl]="form.controls['Email']"
        type="email"
        class="form-control"
        id="Email"
        name="Email">
    </div>
    <div class="row">
      <button
        (click)="submit()"
        role="button"
        class="btn btn-primary submit-btn"
        type="button"
        [disabled]="!form.valid">Submit</button>
    </div>
  </div>
</form>

```



#### Remarks


Angular 2 allow to access the ngForm instance by creating a local template variable. Angular 2 exposes directive instances like ngForm by specifying exportAs property of the directive metadata. Now, the advantage here is without much coding you can access the ngForm instance and use it to access submitted values or to check if all the fields are valid using properties (valid, submitted, value etc).

```js
#f = ngForm (creates local template instance "f")

```

ngForm emits the event "ngSubmit" when it's submitted (Check @Output documentation for more details of event emitter)

```js
(ngSubmit)= "login(f.value,f.submitted)"

```

"ngModel" creates a Form Control in combination with input "name" attribute.

```js
<input type="text" [(ngModel)]="username" placeholder="enter username" required>

```

When form is submitted, f.value has the JSON object representing the submitted values.

{
username: 'Sachin',
password: 'Welcome1'
}

