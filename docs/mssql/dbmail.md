---
metaTitle: "Microsoft SQL Server - DBMAIL"
description: "Send simple email, Send results of a query, Send HTML email"
---

# DBMAIL



## Send simple email


This code sends a simple text-only email to recipient@someaddress.com

```sql
EXEC msdb.dbo.sp_send_dbmail  
    @profile_name = 'The Profile Name',  
    @recipients = 'recipient@someaddress.com',  
    @body = 'This is a simple email sent from SQL Server.',  
    @subject = 'Simple email' 

```



## Send results of a query


This attaches the results of the query `SELECT * FROM Users` and sends it to `recipient@someaddress.com`

```sql
EXEC msdb.dbo.sp_send_dbmail  
    @profile_name = 'The Profile Name',  
    @recipients = 'recipient@someaddress.com',  
    @query = 'SELECT * FROM Users',  
    @subject = 'List of users',  
    @attach_query_result_as_file = 1; 

```



## Send HTML email


HTML content must be passed to `sp_send_dbmail`

```sql
DECLARE @html VARCHAR(MAX);
SET @html = CONCAT
(
    '<html><body>',
    '<h1>Some Header Text</h1>',
    '<p>Some paragraph text</p>',
    '</body></html>'
)

```

```sql
DECLARE @html VARCHAR(MAX);
SET @html =
    '<html><body>' +
    '<h1>Some Header Text</h1>' +
    '<p>Some paragraph text</p>' +
    '</body></html>';

```

Then use the `@html` variable with the `@body argument`. The HTML string can also be passed directly to `@body`, although it may make the code harder to read.

```sql
EXEC msdb.dbo.sp_send_dbmail 
    @recipients='recipient@someaddress.com',  
    @subject = 'Some HTML content',  
    @body = @html,  
    @body_format = 'HTML';  

```



#### Syntax


<li>sp_send_dbmail [ [ @profile_name = ] 'profile_name' ]
[ , [ @recipients = ] 'recipients [ ; ...n ]' ]
[ , [ @copy_recipients = ] 'copy_recipient [ ; ...n ]' ]
[ , [ @blind_copy_recipients = ] 'blind_copy_recipient [ ; ...n ]' ]
[ , [ @from_address = ] 'from_address' ]
[ , [ @reply_to = ] 'reply_to' ]
[ , [ @subject = ] 'subject' ]
[ , [ @body = ] 'body' ]
[ , [ @body_format = ] 'body_format' ]
[ , [ @importance = ] 'importance' ]
[ , [ @sensitivity = ] 'sensitivity' ]
[ , [ @file_attachments = ] 'attachment [ ; ...n ]' ]
[ , [ @query = ] 'query' ]
[ , [ @execute_query_database = ] 'execute_query_database' ]
[ , [ @attach_query_result_as_file = ] attach_query_result_as_file ]
[ , [ @query_attachment_filename = ] query_attachment_filename ]
[ , [ @query_result_header = ] query_result_header ]
[ , [ @query_result_width = ] query_result_width ]
[ , [ @query_result_separator = ] 'query_result_separator' ]
[ , [ @exclude_query_output = ] exclude_query_output ]
[ , [ @append_query_error = ] append_query_error ]
[ , [ @query_no_truncate = ] query_no_truncate ]
[ , [@query_result_no_padding = ] @query_result_no_padding ]
[ , [ @mailitem_id = ] mailitem_id ] [ OUTPUT ]</li>

