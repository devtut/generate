---
metaTitle: "PowerShell - MongoDB"
description: "MongoDB with C# driver 1.7 using PowerShell, I have 3 sets of array in Powershell"
---

# MongoDB



## MongoDB with C# driver 1.7 using PowerShell


I need to query all the details from virtual machine and update into the MongoDB.

```powershell
Which require the output look like this. 
{
    "_id" : ObjectId("5800509f23888a12bccf2347"),
    "ResourceGrp" : "XYZZ-MachineGrp",
    "ProcessTime" : ISODate("2016-10-14T03:27:16.586Z"),
    "SubscriptionName" : "GSS",
    "OS" : "Windows",
    "HostName" : "VM1",
    "IPAddress" : "192.168.22.11",
    "Tags" : {
        "costCenter" : "803344",
        "BusinessUNIT" : "WinEng",
        "MachineRole" : "App",
        "OwnerEmail" : "zteffer@somewhere.com",
        "appSupporter" : "Steve",
        "environment" : "Prod",
        "implementationOwner" : "xyzr@somewhere.com",
        "appSoftware" : "WebServer",
        "Code" : "Gx",
        "WholeOwner" : "zzzgg@somewhere.com"
    },
    "SubscriptionID" : "",
    "Status" : "running fine",
    "ResourceGroupName" : "XYZZ-MachineGrp",
    "LocalTime" : "14-10-2016-11:27"
}

```



## I have 3 sets of array in Powershell


```powershell
        $MachinesList  # Array 
        $ResourceList # Array
        $MachineTags  # Array
    
    pseudo code 

        $mongoDriverPath = 'C:\Program Files (x86)\MongoDB\CSharpDriver 1.7';
        Add-Type -Path "$($mongoDriverPath)\MongoDB.Bson.dll";
        Add-Type -Path "$($mongoDriverPath)\MongoDB.Driver.dll";

      $db = [MongoDB.Driver.MongoDatabase]::Create('mongodb://127.0.0.1:2701/RGrpMachines');
      [System.Collections.ArrayList]$TagList = $vm.tags 
      $A1 = $Taglist.key
      $A2 = $Taglist.value 
    foreach ($Machine in $MachinesList) 
    {
        foreach($Resource in $ResourceList) 
        {
                    $doc2 = $null
                   [MongoDB.Bson.BsonDocument] $doc2 = @{}; #Create a Document here 
                    for($i = 0; $i -lt $TagList.count; $i++)
                           {
                                $A1Key = $A1[$i].ToString()
                                $A2Value = $A2[$i].toString()
                                $doc2.add("$A1Key", "$A2Value")
                           }
                    
                     [MongoDB.Bson.BsonDocument] $doc = @{
                        "_id"= [MongoDB.Bson.ObjectId]::GenerateNewId();
                        "ProcessTime"= [MongoDB.Bson.BsonDateTime] $ProcessTime;
                        "LocalTime" = "$LocalTime";
                        "Tags" = [MongoDB.Bson.BsonDocument] $doc2; 
                        "ResourceGrp" = "$RGName"; 
                        "HostName"= "$VMName";
                        "Status"= "$VMStatus";
                        "IPAddress"= "$IPAddress";
                        "ResourceGroupName"= "$RGName";
                        "SubscriptionName"= "$CurSubName";
                        "SubscriptionID"= "$subid";
                        "OS"= "$OSType";
                    }; #doc loop close
 
                    $collection.Insert($doc);
            }
    }

```



#### Remarks


The most hard part is to attach a **subdocument** into the document which hasn't created yet if we need the subdocument need to be in the expected looking we will need to iterate with a for loop the array into a variable and using `$doc2.add("Key", "Value")` instead using the `foreach` current array with index. This will make the subdocument in two lines as you can see in the `"Tags" = [MongoDB.Bson.BsonDocument] $doc2`.

