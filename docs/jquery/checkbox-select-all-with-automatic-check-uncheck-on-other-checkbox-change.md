---
metaTitle: "jQuery - Checkbox Select all with automatic check/uncheck on other checkbox change"
description: "2 select all checkboxes with corresponding group checkboxes"
---

# Checkbox Select all with automatic check/uncheck on other checkbox change


I've used various Stackoverflow examples and answers to come to this really simple example on how to manage "select all" checkbox coupled with an automatic check/uncheck if any of the group checkbox status changes.
Constraint: The "select all" id must match the input names to create the select all group.
In the example, the input select all ID is cbGroup1. The input names are also cbGroup1

Code is very short, not plenty of if statement (time and resource consuming).



## 2 select all checkboxes with corresponding group checkboxes


```js
<script src="https://ajax.googleapis.com/ajax/libs/jquery/3.2.1/jquery.min.js"></script>

<p>
<input id="cbGroup1" type="checkbox">Select all
<input name="cbGroup1" type="checkbox" value="value1_1">Group1 value 1
<input name="cbGroup1" type="checkbox" value="value1_2">Group1 value 2
<input name="cbGroup1" type="checkbox" value="value1_3">Group1 value 3
</p>
<p>
<input id="cbGroup2" type="checkbox">Select all
<input name="cbGroup2" type="checkbox" value="value2_1">Group2 value 1
<input name="cbGroup2" type="checkbox" value="value2_2">Group2 value 2
<input name="cbGroup2" type="checkbox" value="value2_3">Group2 value 3
</p>

<script type="text/javascript" language="javascript">
    $("input").change(function() {
      $('input[name=\''+this.id+'\']').not(this).prop('checked', this.checked);
      $('#'+this.name).prop('checked', $('input[name=\''+this.name+'\']').length === $('input[name=\''+this.name+'\']').filter(':checked').length);      
    });
</script>

```

