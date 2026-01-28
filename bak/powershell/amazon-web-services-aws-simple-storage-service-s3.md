---
metaTitle: "PowerShell - Amazon Web Services (AWS) Simple Storage Service (S3)"
description: "Create a new S3 Bucket, Upload a Local File Into an S3 Bucket, Delete a S3 Bucket"
---

# Amazon Web Services (AWS) Simple Storage Service (S3)


This documentation section focuses on developing against the Amazon Web Services (AWS) Simple Storage Service (S3). S3 is truly a simple service to interact with. You create S3 "buckets" which can contain zero or more "objects." Once you create a bucket, you can upload files or arbitrary data into the S3 bucket as an "object." You reference S3 objects, inside of a bucket, by the object's "key" (name).



## Create a new S3 Bucket


```powershell
New-S3Bucket -BucketName trevor

```

The Simple Storage Service (S3) bucket name must be globally unique. This means that if someone else has already used the bucket name that you want to use, then you must decide on a new name.



## Upload a Local File Into an S3 Bucket


```powershell
Set-Content -Path myfile.txt -Value 'PowerShell Rocks'
Write-S3Object -BucketName powershell -File myfile.txt

```

Uploading files from your local filesystem into AWS S3 is easy, using the `Write-S3Object` command. In its most basic form, you only need to specify the `-BucketName` parameter, to indicate which S3 bucket you want to upload a file into, and the `-File` parameter, which indicates the relative or absolute path to the local file that you want to upload into the S3 bucket.



## Delete a S3 Bucket


```powershell
Get-S3Object -BucketName powershell | Remove-S3Object -Force
Remove-S3Bucket -BucketName powershell -Force

```

In order to remove a S3 bucket, you must first remove all of the S3 objects that are stored inside of the bucket, provided you have permission to do so. In the above example, we are retrieving a list of all the objects inside a bucket, and then piping them into the `Remove-S3Object` command to delete them. Once all of the objects have been removed, we can use the `Remove-S3Bucket` command to delete the bucket.



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|BucketName|The name of the AWS S3 bucket that you are operating on.
|CannedACLName|The name of the built-in (pre-defined) Access Control List (ACL) that will be associated with the S3 bucket.
|File|The name of a file on the local filesystem that will be uploaded to an AWS S3 Bucket.

