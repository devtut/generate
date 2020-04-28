---
metaTitle: "php mysqli affected rows returns 0 when it should return a positive integer"
description: "PHP's $stmt->affected_rows intermittently returning 0 when it should return a positive integer"
---

# php mysqli affected rows returns 0 when it should return a positive integer


This script is designed to handle reporting devices (IoT), when a device is not previously authorized (in the devices table in the database), I add the new device to a new_devices table. I run an update query, and if affected_rows returns < 1, I insert.

When I have a new device report, the first time $stmt->affected_rows runs it returns 0, subsequent communication returns 1, then 1, 0, 2, 2, 2, 0, 3, 3, 3, 3, 3, 3, 0, 4, 0, 0, 6, 6, 6, etc

It's as if the update statement is failing. Why?



## PHP's $stmt->affected_rows intermittently returning 0 when it should return a positive integer


```php
<?php
    // if device exists, update timestamp
    $stmt = $mysqli->prepare("UPDATE new_devices SET nd_timestamp=? WHERE nd_deviceid=?");
    $stmt->bind_param('ss', $now, $device);
    $stmt->execute();
    //echo "Affected Rows: ".$stmt->affected_rows; // This line is where I am checking the status of the update query.

    if ($stmt->affected_rows < 1){ // Because affected_rows sometimes returns 0, the insert code runs instead of being skipped. Now I have many duplicate entries.
        
        $ins = $mysqli->prepare("INSERT INTO new_devices (nd_id,nd_deviceid,nd_timestamp) VALUES (nd_id,?,?)");
        $ins -> bind_param("ss",$device,$now);
        $ins -> execute();
        $ins -> store_result();
        $ins -> free_result();
    }
?>

```

