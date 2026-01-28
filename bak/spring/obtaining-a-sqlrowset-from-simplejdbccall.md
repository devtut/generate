---
metaTitle: "Spring - Obtaining a SqlRowSet from SimpleJdbcCall"
description: "SimpleJdbcCall creation, Oracle Databases"
---

# Obtaining a SqlRowSet from SimpleJdbcCall


This describes how to directly obtain a **SqlRowSet** using **SimpleJdbcCall** with a stored procedure in your database that has a **cursor output parameter**,

I am working with an Oracle database, I've attempted to create an example that should work for other databases, my Oracle example details issues with Oracle.



## SimpleJdbcCall creation


Typically, you will want to create your SimpleJdbcCalls in a Service.

This example assumes your procedure has a single output parameter that is a cursor; you will need to adjust your declareParameters to match your procedure.

```java
@Service
public class MyService() {

@Autowired
    private DataSource dataSource;
    
    // Autowire your configuration, for example
    @Value("${db.procedure.schema}")
    String schema;
    
    private SimpleJdbcCall myProcCall;
    
    // create SimpleJdbcCall after properties are configured
    @PostConstruct
    void initialize() {
        this.myProcCall = new SimpleJdbcCall(dataSource)
                        .withProcedureName("my_procedure_name")
                        .withCatalogName("my_package")
                        .withSchemaName(schema)
                        .declareParameters(new SqlOutParameter(
                            "out_param_name",
                            Types.REF_CURSOR, 
                            new SqlRowSetResultSetExtractor()));
    }

    public SqlRowSet myProc() {
        Map<String, Object> out = this.myProcCall.execute();
        return (SqlRowSet) out.get("out_param_name");
    }

}

```

There are many options you can use here:

- **withoutProcedureColumnMetaDataAccess()** needed if you have overloaded procedure names or just don't want SimpleJdbcCall to validate against the database.
- **withReturnValue()** if procedure has a return value. First value given to declareParameters defines the return value. Also, if your procedure is a function, use **withFunctionName** and **executeFunction** when executing.
- **withNamedBinding()** if you want to give arguments using names instead of position.
- **useInParameterNames()** defines the argument order. I think this may be required if you pass in your arguments as a list instead of a map of argument name to value. Though it may only be required if you use withoutProcedureColumnMetaDataAccess()



## Oracle Databases


There are a number of issues with Oracle. Here's how to resolve them.

Assuming your procedure output parameter is `ref cursor`, you will get this exception.

> 
java.sql.SQLException: Invalid column type: 2012


So change `Types.REF_CURSOR` to `OracleTypes.CURSOR` in **simpleJdbcCall.declareParameters()**

**Supporting OracleTypes**

**You may only need to do this if you have certain column types in your data.**

The next issue I encountered was that proprietary Types such as `oracle.sql.TIMESTAMPTZ` caused this error in SqlRowSetResultSetExtractor:

> 
<p>Invalid SQL type for column; nested exception is
java.sql.SQLException: Invalid SQL type for column</p>


So we need to create a **ResultSetExtractor** that supports Oracle types.<br />
**I will explain the reason for password after this code.**

```java
package com.boost.oracle;

import oracle.jdbc.rowset.OracleCachedRowSet;
import org.springframework.dao.DataAccessException;
import org.springframework.jdbc.core.ResultSetExtractor;
import org.springframework.jdbc.support.rowset.ResultSetWrappingSqlRowSet;
import org.springframework.jdbc.support.rowset.SqlRowSet;

import java.sql.ResultSet;
import java.sql.SQLException;

/**
 * OracleTypes can cause {@link org.springframework.jdbc.core.SqlRowSetResultSetExtractor}
 * to fail due to a Oracle SQL type that is not in the standard {@link java.sql.Types}.
 *
 * Also, types such as {@link oracle.sql.TIMESTAMPTZ} require a Connection when processing
 * the ResultSet; {@link OracleCachedRowSet#getConnectionInternal()} requires a JNDI
 * DataSource name or the username and password to be set.
 *
 * For now I decided to just set the password since changing SpringBoot to a JNDI DataSource
 * configuration is a bit complicated.
 *
 * Created by Arlo White on 2/23/17.
 */
public class OracleSqlRowSetResultSetExtractor implements ResultSetExtractor<SqlRowSet> {

    private String oraclePassword;

    public OracleSqlRowSetResultSetExtractor(String oraclePassword) {
        this.oraclePassword = oraclePassword;
    }

    @Override
    public SqlRowSet extractData(ResultSet rs) throws SQLException, DataAccessException {
        OracleCachedRowSet cachedRowSet = new OracleCachedRowSet();
        // allows getConnectionInternal to get a Connection for TIMESTAMPTZ
        cachedRowSet.setPassword(oraclePassword);
        cachedRowSet.populate(rs);
        return new ResultSetWrappingSqlRowSet(cachedRowSet);
    }

}

```

Certain Oracle types require a Connection to obtain the column value from a ResultSet.
TIMESTAMPTZ is one of these types. So when `rowSet.getTimestamp(colIndex)` is called, you will get this exception:

> 
<p>Caused by: java.sql.SQLException: One or more of the authenticating
RowSet properties not set
at oracle.jdbc.rowset.OracleCachedRowSet.getConnectionInternal(OracleCachedRowSet.java:560)
at oracle.jdbc.rowset.OracleCachedRowSet.getTimestamp(OracleCachedRowSet.java:3717)
at org.springframework.jdbc.support.rowset.ResultSetWrappingSqlRowSet.getTimestamp</p>


If you dig into this code, you will see that the OracleCachedRowSet needs the password or a JNDI DataSource name to get a Connection. If you prefer the JNDI lookup, just verify that OracleCachedRowSet has DataSourceName set.

So in my Service, I Autowire in the password and declare the output parameter like this:

```java
new SqlOutParameter("cursor_param_name", OracleTypes.CURSOR, new OracleSqlRowSetResultSetExtractor(oraclePassword))

```

