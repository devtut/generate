---
metaTitle: "PowerShell - SharePoint Module"
description: "Loading SharePoint Snap-In, Iterating over all lists of a site collection, Get all installed features on a site collection"
---

# SharePoint Module



## Loading SharePoint Snap-In


Loading the SharePoint Snapin can be done using the following:

`Add-PSSnapin "Microsoft.SharePoint.PowerShell"`

**This only works in the 64bit version of PowerShell.** If the window says "Windows PowerShell (x86)" in the title you are using the incorrect version.

If the Snap-In is already loaded, the code above will cause an error. Using the following will load only if necessary, which can be used in Cmdlets/functions:

```powershell
if ((Get-PSSnapin "Microsoft.SharePoint.PowerShell" -ErrorAction SilentlyContinue) -eq $null)
{
    Add-PSSnapin "Microsoft.SharePoint.PowerShell"
}

```

Alternatively, if you start the SharePoint Management Shell, it will automatically include the Snap-In.

To get a list of all the available SharePoint Cmdlets, run the following:

`Get-Command -Module Microsoft.SharePoint.PowerShell`



## Iterating over all lists of a site collection


Print out all list names and the item count.

```powershell
$site = Get-SPSite -Identity https://mysharepointsite/sites/test
foreach ($web in $site.AllWebs)
{
    foreach ($list in $web.Lists)
    {
        # Prints list title and item count
        Write-Output "$($list.Title), Items: $($list.ItemCount)"
    }
}
$site.Dispose()

```



## Get all installed features on a site collection


`Get-SPFeature -Site https://mysharepointsite/sites/test`

Get-SPFeature can also be run on web scope (`-Web <WebUrl>`), farm scope (`-Farm`) and web application scope (`-WebApplication <WebAppUrl>`).

**Get all orphaned features on a site collection**

Another usage of Get-SPFeature can be to find all features that have no scope:

`Get-SPFeature -Site https://mysharepointsite/sites/test |? { $_.Scope -eq $null )`

