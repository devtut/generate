---
metaTitle: "Entity Framework - Entity-Framework  with Postgresql"
description: "Pre-Steps needed in order to use Entity Framework 6.1.3 with PostgresSql using Npgsqlddexprovider"
---

# Entity-Framework  with Postgresql



## Pre-Steps needed in order to use Entity Framework 6.1.3 with PostgresSql using Npgsqlddexprovider


1)Took backup of Machine.config from locations C:\Windows\Microsoft.NET\Framework\v4.0.30319\Config and C:\Windows\Microsoft.NET\Framework64\v4.0.30319\Config

2)Copy them to different location and edit them as

> 
a)locate and add under `<system.data> <DbProviderFactories>`


```

           <add name="Npgsql Data Provider" invariant="Npgsql" support="FF"
            description=".Net Framework Data Provider for Postgresql Server"
            type="Npgsql.NpgsqlFactory, Npgsql, Version=2.2.5.0, Culture=neutral, PublicKeyToken=5d8b90d52f46fda7" />

```

> 
b)if already exist above entry, check verison and update it.


1. Replace original files with changed ones.
1. run Developer Command Prompt for VS2013 as Administrator.
1. if Npgsql already installed use command " gacutil -u Npgsql " to uninstall then install new version of Npgsql 2.5.0 by command " gacutil -i [path of dll] "
1. Do above for Mono.Security 4.0.0.0
1. Download NpgsqlDdexProvider-2.2.0-VS2013.zip and run NpgsqlDdexProvider.vsix from    it(Do close all instances of visual studio)
1. Found EFTools6.1.3-beta1ForVS2013.msi and run it.
1. After crating new project, Install version of EntityFramework(6.1.3), NpgSql(2.5.0) and NpgSql.EntityFramework(2.5.0) from Manage Nuget Packages.10)Its Done go ahead...Add new Entity Data Model in your MVc project

