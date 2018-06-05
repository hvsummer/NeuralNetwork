Imports System.IO
Imports System.Data.SQLite

Public Class LiteDb
    Private DbName$
    Public Path$ = getParentPath(CurProjectPath, 3)
    Public ConnString$

    Private Conn As SQLiteConnection

    Public status As Byte
    Public lastAffect&

    Public ReadOnly Property FullPath()
        Get
            Return IO.Path.Combine(Path, DbName)
        End Get
    End Property

    Public ReadOnly Property Tables
        Get
            If status < 2 Then Return New List(Of String)
            Dim Res = Conn.GetSchema(SQLiteMetaDataCollectionNames.Tables)
            Tables = New List(Of String)
            For ii% = 0 To Res.Rows.Count - 1
                If Res.Rows(ii)!table_type.ToString = "table" Then
                    Tables.Add(Res.Rows(ii)!table_name.ToString)
                End If
            Next
        End Get
    End Property
    Public ReadOnly Property DataBase_Name
        Get
            Return DbName
        End Get
    End Property

    Public Function Execute(strSQL$) As Integer
        If status < 2 Then Return -1
        Using cmd = New SQLiteCommand(strSQL, Conn)
            lastAffect = cmd.ExecuteNonQuery()
            Return lastAffect
        End Using
    End Function
    Public Overloads Function Query(strSQL$) As DataTable
        Try
            Using Tbl As New DataTable("Temp")
                Using adapter = New SQLiteDataAdapter(strSQL, Conn)
                    adapter.Fill(Tbl)
                    Return Tbl
                End Using
            End Using
        Catch e As Exception
            MsgBox(e.Message)
            Return Nothing
        End Try
    End Function
    Public Overloads Function Query(strsql$, Optional method% = 2) As List(Of String)
        Try
            '//another way
            Using Reader As SQLiteDataReader = (New SQLiteCommand(strsql, Conn)).ExecuteReader
                Dim Data As New List(Of String)
                While Reader.Read()
                    Dim row As New List(Of String)
                    With row
                        For ii& = 0 To Reader.FieldCount - 1
                            row.Add(Reader.GetValue(ii))
                        Next
                    End With
                    Data.Add(row.ToStr(vbTab))
                End While
                Return Data
            End Using
        Catch e As Exception
            MsgBox(e.Message)
            Return Nothing
        End Try
    End Function

    '// Custom function of this Wrapper Class
    Public Function LoadCSV(Path$, TblName$, Optional MaxColumn& = -1)
        'FileName=Path OR data=TEXT
        'schema=SCHEMA (Example: CREATE TABLE TblName (Field1, Field2) ) OR columns=N
        Dim strSQL$ = "CREATE VIRTUAL TABLE temp.{0} USING csv(filename={1})"
        strSQL = String.Format(strSQL, TblName, Path & IIf(MaxColumn > 0, ",Columns=" & MaxColumn, ""))
        lastAffect = Execute(strSQL)
        Return Query("SELECT * FROM " & TblName)
    End Function


    '// Custom function for SQlite

    'example of UDF in sqlite
    <SQLiteFunction(Name:="ToUpper", Arguments:=1, FuncType:=FunctionType.Scalar)>
    Public Class ToUpper
        Inherits SQLiteFunction

        Public Overrides Function Invoke(ByVal args As Object()) As Object
            Return args(0).ToString().ToUpper()
        End Function
    End Class


    '//Image to Blob and vise versa
    'Image BLOB Functions
    Private Function BlobToImage(ByVal blob)
        Using mStream As New System.IO.MemoryStream
            Dim pData() As Byte = DirectCast(blob, Byte())
            mStream.Write(pData, 0, Convert.ToInt32(pData.Length))
            Dim bm As Bitmap = New Bitmap(mStream, False)
            Return bm
        End Using
    End Function

    Public Overloads Function ImageToBlob(ByVal id As String, ByVal filePath As String)
        Dim fs As FileStream = New FileStream(filePath, FileMode.Open, FileAccess.Read)
        Dim br As BinaryReader = New BinaryReader(fs)
        Dim bm() As Byte = br.ReadBytes(fs.Length)
        br.Close()
        fs.Close()
        'Create Parm
        Dim photo() As Byte = bm
        Dim SQLparm As New SQLiteParameter(id, photo)
        SQLparm.DbType = DbType.Binary
        SQLparm.Value = photo
        Return SQLparm
    End Function
    Public Overloads Function ImageToBlob(ByVal id As String, ByVal image As Image)
        Dim ms As New MemoryStream()
        image.Save(ms, System.Drawing.Imaging.ImageFormat.Png)
        'Create Parm
        Dim photo() As Byte = ms.ToArray()
        Dim SQLparm As New SQLiteParameter(id, photo)
        SQLparm.DbType = DbType.Binary
        SQLparm.Value = photo
        Return SQLparm
    End Function


    '//prepare db if not exist
    Public Function CreateDB()
        If Not File.Exists(FullPath) Then
            Try
                SQLiteConnection.CreateFile(DbName)
                Return True
            Catch E As Exception
                Console.WriteLine("Failed to create SqLite, error on:" + E.Message)
                Return False
            End Try
        End If
        Return True
    End Function

    'Base sub of class
    Public Sub New(Optional Name$ = "")
        If Name.Length > 0 Then DbName = Name
        ConnString$ = String.Format("Data Source={0};", FullPath)
        Conn = New SQLiteConnection(ConnString)
        Conn.Open()
        status = 0
        If CreateDB() Then status = 2 Else status = 1
    End Sub
    Protected Overrides Sub Finalize()
        'Conn.Close()
        Conn = Nothing
        status = 0
        MyBase.Finalize()
    End Sub

End Class


