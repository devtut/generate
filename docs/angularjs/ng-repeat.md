---
metaTitle: "AngularJS - ng-repeat"
description: "ng-repeat-start + ng-repeat-end, Iterating over object properties, Tracking and Duplicates"
---

# ng-repeat




## ng-repeat-start + ng-repeat-end


AngularJS 1.2 `ng-repeat` handle multiple elements with `ng-repeat-start` and `ng-repeat-end`:

```js
// table items
$scope.tableItems = [
    {
        row1: 'Item 1: Row 1',
        row2: 'Item 1: Row 2'
    },
    {
        row1: 'Item 2: Row 1',
        row2: 'Item 2: Row 2'
    }
];

// template
<table>
    <th>
        <td>Items</td>
    </th>
    <tr ng-repeat-start="item in tableItems">
        <td ng-bind="item.row1"></td>
    </tr>
    <tr ng-repeat-end>
        <td ng-bind="item.row2"></td>
    </tr>
</table>

```

Output:

|Items
|---|---|---|---|---|---|---|---|---|---
|Item 1: Row 1
|Item 1: Row 2
|Item 2: Row 1
|Item 2: Row 2



## Iterating over object properties


```js
<div ng-repeat="(key, value) in myObj"> ... </div>

```

**For example**

```js
<div ng-repeat="n in [42, 42, 43, 43]">
  { {n} }
</div>

```



## Tracking and Duplicates


`ngRepeat` uses [$watchCollection](https://docs.angularjs.org/api/ng/type/$rootScope.Scope#$watchCollection) to detect changes in the collection. When a change happens, `ngRepeat` then makes the corresponding changes to the DOM:

<li>When an item is added, a new instance of the template is added to the
DOM.</li>
<li>When an item is removed, its template instance is removed from the
DOM.</li>
<li>When items are reordered, their respective templates are reordered in
the DOM.</li>

**Duplicates**

- `track by` for any list that may include duplicate values.
- `track by` also speeds up list changes significantly.
- If you don't use `track by` in this case, you get the error: `[ngRepeat:dupes]`

```js
$scope.numbers = ['1','1','2','3','4'];

<ul>
  <li ng-repeat="n in numbers track by $index">
    { {n} }
  </li>
</ul>

```



#### Syntax


- `<element ng-repeat="expression"></element>`
- `<div ng-repeat="(key, value) in myObj">...</div>`
- `<div ng-repeat="variable in expression">...</div>`



#### Parameters


|Variable|Details
|---|---|---|---|---|---|---|---|---|---
|`$index`|**number** iterator offset of the repeated element (0..length-1)
|`$first`|**boolean** true if the repeated element is first in the iterator.
|`$middle`|**boolean** true if the repeated element is between the first and last in the iterator.
|`$last`|**boolean** true if the repeated element is last in the iterator.
|`$even`|**boolean** true if the iterator position `$index` is even (otherwise false).
|`$odd`|**boolean** true if the iterator position `$index` is odd (otherwise false).



#### Remarks


AngularJS provides these parameters as special variables that are available in the
ng-repeat expression and anywhere inside of the HTML tag on which the
ng-repeat lives.

