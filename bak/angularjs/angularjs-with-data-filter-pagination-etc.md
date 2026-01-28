---
metaTitle: "AngularJS - angularjs with data filter, pagination etc"
description: "Angularjs display data with filter, pagination"
---

# angularjs with data filter, pagination etc


Provider example and query about display data with filter, pagination etc in Angularjs.



## Angularjs display data with filter, pagination


```js
<div ng-app="MainApp" ng-controller="SampleController">
    <input ng-model="dishName" id="search" class="form-control" placeholder="Filter text">
    <ul>
        <li dir-paginate="dish in dishes |  filter : dishName | itemsPerPage: pageSize" pagination-id="flights">{ {dish} }</li>
    </ul>
    <dir-pagination-controls boundary-links="true" on-page-change="changeHandler(newPageNumber)" pagination-id="flights"></dir-pagination-controls>
</div>
<script type="text/javascript" src="angular.min.js"></script>
<script type="text/javascript" src="pagination.js"></script>
<script type="text/javascript">

var MainApp = angular.module('MainApp', ['angularUtils.directives.dirPagination'])
MainApp.controller('SampleController', ['$scope', '$filter', function ($scope, $filter) {

    $scope.pageSize = 5;
    
    $scope.dishes = [
    'noodles',
    'sausage',
    'beans on toast',
    'cheeseburger',
    'battered mars bar',
    'crisp butty',
    'yorkshire pudding',
    'wiener schnitzel',
    'sauerkraut mit ei',
    'salad',
    'onion soup',
    'bak choi',
    'avacado maki'
    ];
    
    $scope.changeHandler = function (newPage) { };
}]);
</script>

```

