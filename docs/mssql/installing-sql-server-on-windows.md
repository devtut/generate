---
metaTitle: "Microsoft SQL Server - Installing SQL Server on Windows"
description: "Introduction"
---

# Installing SQL Server on Windows



## Introduction


These are the available editions of SQL Server, as told by the [Editions Matrix](https://www.microsoft.com/en/server-cloud/products/sql-server-editions/overview.aspx):

- Express: Entry-level free database. Includes core-RDBMS functionality. Limited to 10G of disk size. Ideal for development and testing.
- Standard Edition: Standard Licensed edition. Includes core functionality and Business Intelligence capabilities.
- Enterprise Edition: Full-featured SQL Server edition. Includes advanced security and data warehousing capabilities.
- Developer Edition: Includes all of the features from Enterprise Edition and no limitations, and it is [free to download and use](https://www.microsoft.com/en-us/cloud-platform/sql-server-editions-developers) for development purposes only.

After downloading/acquiring SQL Server, the installation gets executed with SQLSetup.exe, which is available as a GUI or a command-line program.

Installing via either of these will require you to specify a product key and run some initial configuration that includes enabling features, separate services and setting the initial parameters for each of them. Additional services and features can be enabled at any time by running the SQLSetup.exe program in either the command-line or the GUI version.

