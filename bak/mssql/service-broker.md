---
metaTitle: "Microsoft SQL Server - Service broker"
description: "1. Basics, 2. Enable service broker on database, 3. Create basic service broker construction on database (single database communication), 4. How to send basic communication through service broker, 5. How to receive conversation from TargetQueue automatically"
---

# Service broker



## 1. Basics


Service broker is technology based on asyncronous communication between two(or more) entities.
Service broker consists of:
message types,
contracts,
queues,
services,
routes,
and at least instance endpoints

More: [https://msdn.microsoft.com/en-us/library/bb522893.aspx](https://msdn.microsoft.com/en-us/library/bb522893.aspx)



## 2. Enable service broker on database


```sql
ALTER DATABASE [MyDatabase] SET ENABLE_BROKER WITH ROLLBACK IMMEDIATE;

```



## 3. Create basic service broker construction on database (single database communication)


```sql
USE [MyDatabase]  

CREATE MESSAGE TYPE [//initiator] VALIDATION = WELL_FORMED_XML;
    GO

CREATE CONTRACT [//call/contract]
(
    [//initiator] SENT BY INITIATOR 
)
GO

CREATE QUEUE  InitiatorQueue;
GO

CREATE QUEUE  TargetQueue;
GO

CREATE SERVICE InitiatorService
      ON QUEUE InitiatorQueue
(
     [//call/contract]    
      
)

CREATE SERVICE TargetService
ON QUEUE TargetQueue 
(
     [//call/contract]    
      
)

GRANT SEND ON SERVICE::[InitiatorService] TO PUBLIC
GO

GRANT SEND ON SERVICE::[TargetService] TO PUBLIC
GO

```

We dont need route for one database communication.



## 4. How to send basic communication through service broker


For this demonstration we will use service broker construction created in another part of this documentation. Mentioned part is named **3. Create basic service broker construction on database (single database communication)**.

```sql
USE [MyDatabase]

DECLARE @ch uniqueidentifier = NEWID()
DECLARE @msg XML 

BEGIN DIALOG CONVERSATION @ch
    FROM SERVICE [InitiatorService]
    TO SERVICE 'TargetService'
    ON CONTRACT [//call/contract]
    WITH ENCRYPTION = OFF; -- more possible options

        SET @msg = (
                    SELECT 'HelloThere' "elementNum1"
                    FOR XML PATH(''), ROOT('ExampleRoot'), ELEMENTS XSINIL, TYPE
                    );         
      
SEND ON CONVERSATION @ch MESSAGE TYPE [//initiator] (@msg);
END CONVERSATION @ch;

```

After this conversation will be your msg in TargetQueue



## 5. How to receive conversation from TargetQueue automatically


For this demonstration we will use service broker construction created in another part of this documentation. Mentioned part is called **3. Create basic service broker construction on database (single database communication)**.

First we need to create a procedure that is able to read and process data from the Queue

```sql
USE [MyDatabase]
GO

SET ANSI_NULLS ON
GO
SET QUOTED_IDENTIFIER ON
GO

CREATE PROCEDURE [dbo].[p_RecieveMessageFromTargetQueue] 
    
    AS
    BEGIN
    
    declare 
    @message_body xml, 
    @message_type_name nvarchar(256), 
    @conversation_handle uniqueidentifier,
    @messagetypename nvarchar(256);

    
              
   WHILE 1=1
   BEGIN
    
    BEGIN TRANSACTION
        WAITFOR(
        RECEIVE TOP(1)
        @message_body = CAST(message_body as xml), 
        @message_type_name = message_type_name,
        @conversation_handle = conversation_handle,
        @messagetypename = message_type_name
        FROM DwhInsertSmsQueue
        ), TIMEOUT 1000;
    
         IF (@@ROWCOUNT = 0)
            BEGIN
                ROLLBACK TRANSACTION
                BREAK
            END

         IF (@messagetypename = '//initiator')
             BEGIN
                
                IF OBJECT_ID('MyDatabase..MyExampleTableHelloThere') IS NOT NULL 
                    DROP TABLE dbo.MyExampleTableHelloThere
                
                SELECT @message_body.value('(/ExampleRoot/"elementNum1")[1]', 'VARCHAR(50)') AS MyExampleMessage
                INTO dbo.MyExampleTableHelloThere
             
             END
    
           
 
         IF (@messagetypename = 'http://schemas.microsoft.com/SQL/ServiceBroker/EndDialog')
            BEGIN
                END CONVERSATION @conversation_handle;
            END
   
    COMMIT TRANSACTION
   END
   

END

```

Second step: Allow your TargetQueue to automatically run your procedure:

```sql
USE [MyDatabase]

ALTER QUEUE [dbo].[TargetQueue] WITH STATUS = ON , RETENTION = OFF , 
ACTIVATION
 (  STATUS = ON , --activation status
    PROCEDURE_NAME = dbo.p_RecieveMessageFromTargetQueue , --procedure name
    MAX_QUEUE_READERS = 1 , --number of readers
    EXECUTE AS SELF  )

```

