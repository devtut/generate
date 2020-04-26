# ArcPy




## Printing one field's value for all rows of feature class in file geodatabase using Search Cursor


To print a test field (TestField) from a test feature class (TestFC) in a test file geodatabase (Test.gdb) located in a temporary folder (C:\Temp):

```
with arcpy.da.SearchCursor(r&quot;C:\Temp\Test.gdb\TestFC&quot;,[&quot;TestField&quot;]) as cursor:
    for row in cursor:
        print row[0]

```



## createDissolvedGDB to create a file gdb on the workspace


```
def createDissolvedGDB(workspace, gdbName):
    gdb_name = workspace + &quot;/&quot; + gdbName + &quot;.gdb&quot;

    if(arcpy.Exists(gdb_name):
        arcpy.Delete_management(gdb_name)
        arcpy.CreateFileGDB_management(workspace, gdbName, &quot;&quot;)
    else:
        arcpy.CreateFileGDB_management(workspace, gdbName, &quot;&quot;)

    return gdb_name

```



#### Remarks


This example uses a Search Cursor from the Data Access (da) module of ArcPy.

Do not confuse arcpy.da.SearchCursor syntax with the earlier and slower arcpy.SearchCursor().

The Data Access module (arcpy.da) has only been available since ArcGIS 10.1 for Desktop.

