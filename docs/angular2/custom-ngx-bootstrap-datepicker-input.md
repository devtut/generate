---
metaTitle: "Angular 2 - custom ngx-bootstrap datepicker + input"
description: "custom ngx-bootstrap datepicker"
---

# custom ngx-bootstrap datepicker + input




## custom ngx-bootstrap datepicker


> 
datepicker.component.html


```js
<div (clickOutside)="onClickedOutside($event)" (blur)="onClickedOutside($event)">
      <div class="input-group date" [ngClass]="{'disabled-icon': disabledDatePicker == false }">
        <input (change)="changedDate()" type="text" [ngModel]="value" class="form-control" id="{{id}}" (focus)="openCloseDatepicker()" disabled="{{disabledInput}}" />
        <span id="openCloseDatePicker" class="input-group-addon" (click)="openCloseDatepicker()">
                <span class="glyphicon-calendar glyphicon"></span>
            </span>
      </div>
    
      <div class="dp-popup" *ngIf="showDatepicker">
        <datepicker [startingDay]="1" [startingDay]="dt" [minDate]="min"  [(ngModel)]="dt"  (selectionDone)="onSelectionDone($event)"></datepicker>
      </div>
    </div>

```

> 
datepicker.component.ts


```js
import {Component, Input, EventEmitter, Output, OnChanges, SimpleChanges, ElementRef, OnInit} from "@angular/core";
import {DatePipe} from "@angular/common";
import {NgModel} from "@angular/forms";
import * as moment from 'moment';

@Component({
  selector: 'custom-datepicker',
  templateUrl: 'datepicker.component.html',
  providers: [DatePipe, NgModel],
  host: {
    '(document:mousedown)': 'onClick($event)',
  }
})

export class DatepickerComponent implements OnChanges , OnInit{
  ngOnInit(): void {
    this.dt = null;
  }

  inputElement : ElementRef;
  dt: Date = null;
  showDatepicker: boolean = false;

  @Input() disabledInput : boolean = false;
  @Input() disabledDatePicker: boolean = false;
  @Input() value: string = null;
  @Input() id: string;
  @Input() min: Date = null;
  @Input() max: Date = null;

  @Output() dateModelChange = new EventEmitter();
  constructor(el: ElementRef) {
    this.inputElement = el;
  }

  changedDate(){
    if(this.value === ''){
      this.dateModelChange.emit(null);
    }else if(this.value.split('/').length === 3){
      this.dateModelChange.emit(DatepickerComponent.convertToDate(this.value));
    }
  }

  clickOutSide(event : Event){
    if(this.inputElement.nativeElement !== event.target) {
      console.log('click outside', event);
    }
  }

  onClick(event) {
    if (!this.inputElement.nativeElement.contains(event.target)) {
      this.close();
    }
  }
  ngOnChanges(changes: SimpleChanges): void {
    if (this.value !== null && this.value !== undefined && this.value.length > 0) {
      this.value = null;
      this.dt  = null;
    }else {
      if(this.value !== null){
        this.dt = new Date(this.value);
        this.value = moment(this.value).format('MM/DD/YYYY');
      }
    }
  }

  private static transformDate(date: Date): string {
    return new DatePipe('pt-PT').transform(date, 'MM/dd/yyyy');
  }

  openCloseDatepicker(): void {
    if (!this.disabledDatePicker) {
      this.showDatepicker = !this.showDatepicker;
    }
  }

  open(): void {
    this.showDatepicker = true;
  }

  close(): void {
    this.showDatepicker = false;
  }

  private apply(): void {
    this.value = DatepickerComponent.transformDate(this.dt);
    this.dateModelChange.emit(this.dt);
  }

  onSelectionDone(event: Date): void {
    this.dt = event;
    this.apply();
    this.close();
  }

  onClickedOutside(event: Date): void {
    if (this.showDatepicker) {
      this.close();
    }
  }

  static convertToDate(val : string): Date {
    return new Date(val.replace('/','-'));
  }

}

```

