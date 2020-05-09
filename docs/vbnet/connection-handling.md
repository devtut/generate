---
metaTitle: "Visual Basic .NET - Connection Handling"
description: "Public connection property"
---

# Connection Handling



## Public connection property


```

   Imports System.Data.OleDb

    Private WithEvents _connection As OleDbConnection
    Private _connectionString As String = "myConnectionString"

    Public ReadOnly Property Connection As OleDbConnection
        Get
            If _connection Is Nothing Then
                _connection = New OleDbConnection(_connectionString)
                _connection.Open()
            Else
                If _connection.State <> ConnectionState.Open Then
                    _connection.Open()
                End If
            End If
            Return _connection
        End Get
    End Property

```

