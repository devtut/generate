---
metaTitle: "Full-Text Indexing"
description: "A. Creating a unique index, a full-text catalog, and a full-text index, Creating a full-text index on several table columns, Creating a full-text index with a search property list without populating it, Full-Text Search"
---

# Full-Text Indexing



## A. Creating a unique index, a full-text catalog, and a full-text index


The following example creates a unique index on the JobCandidateID column of the HumanResources.JobCandidate table of the AdventureWorks2012 sample database. The example then creates a default full-text catalog, ft. Finally, the example creates a full-text index on the Resume column, using the ft catalog and the system stoplist.

```sql
USE AdventureWorks2012;  
GO  
CREATE UNIQUE INDEX ui_ukJobCand ON HumanResources.JobCandidate(JobCandidateID);  
CREATE FULLTEXT CATALOG ft AS DEFAULT;  
CREATE FULLTEXT INDEX ON HumanResources.JobCandidate(Resume)   
   KEY INDEX ui_ukJobCand   
   WITH STOPLIST = SYSTEM;  
GO 

```

[https://www.simple-talk.com/sql/learn-sql-server/understanding-full-text-indexing-in-sql-server/](https://www.simple-talk.com/sql/learn-sql-server/understanding-full-text-indexing-in-sql-server/)

[https://msdn.microsoft.com/en-us/library/cc879306.aspx](https://msdn.microsoft.com/en-us/library/cc879306.aspx)

[https://msdn.microsoft.com/en-us/library/ms142571.aspx](https://msdn.microsoft.com/en-us/library/ms142571.aspx)



## Creating a full-text index on several table columns


```sql
USE AdventureWorks2012;  
GO  
CREATE FULLTEXT CATALOG production_catalog;  
GO  
CREATE FULLTEXT INDEX ON Production.ProductReview  
 (   
  ReviewerName  
     Language 1033,  
  EmailAddress  
     Language 1033,  
  Comments   
     Language 1033       
 )   
  KEY INDEX PK_ProductReview_ProductReviewID   
      ON production_catalog;   
GO  

```



## Creating a full-text index with a search property list without populating it


```sql
USE AdventureWorks2012;  
GO  
CREATE FULLTEXT INDEX ON Production.Document  
  (   
  Title  
      Language 1033,   
  DocumentSummary  
      Language 1033,   
  Document   
      TYPE COLUMN FileExtension  
      Language 1033   
  )  
  KEY INDEX PK_Document_DocumentID  
          WITH STOPLIST = SYSTEM, SEARCH PROPERTY LIST = DocumentPropertyList, CHANGE_TRACKING OFF, NO POPULATION;  
   GO  

```

And populating it later with

```sql
ALTER FULLTEXT INDEX ON Production.Document SET CHANGE_TRACKING AUTO;  
GO  

```



## Full-Text Search


```sql
SELECT product_id   
FROM products   
WHERE CONTAINS(product_description, ”Snap Happy 100EZ” OR FORMSOF(THESAURUS,’Snap Happy’) OR ‘100EZ’)   
AND product_cost < 200 ;  


SELECT candidate_name,SSN   
FROM candidates   
WHERE CONTAINS(candidate_resume,”SQL Server”) AND candidate_division =DBA;  

```

For more and detailed info
[https://msdn.microsoft.com/en-us/library/ms142571.aspx](https://msdn.microsoft.com/en-us/library/ms142571.aspx)

