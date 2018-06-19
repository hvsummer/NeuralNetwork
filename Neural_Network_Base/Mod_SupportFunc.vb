Option Explicit On


Imports System.Runtime.CompilerServices


Public Module Support

    '//Extension method for enhance ablitity of default class
    <Extension()>
    Public Function ToStr(ByRef source As IEnumerable(Of String), Optional Delimiter$ = ",") As String
        If source.Count = 0 Then ToStr = "Null" : Exit Function
        Return Join(source.ToArray, Delimiter)
    End Function
    <Extension()>
    Public Function ToStr(ByRef source As List(Of String), Optional Delimiter$ = ",") As String
        If source.Count = 0 Then ToStr = "Null" : Exit Function
        Return Join(source.ToArray, Delimiter)
    End Function
    <Extension()>
    Public Function ToStr(ByRef source As VariantType(,), Optional Delimiter$ = ",") As String
        If source.Length = 0 Then ToStr = "Null" : Exit Function
        Dim sb As New Text.StringBuilder
        For Each g In source
            sb.Append(g).Append(Delimiter)
        Next
        Return sb.Remove(sb.Length - 1, 1).ToString
    End Function

    <Extension()>
    Public Function ToStr(ByRef source As IEnumerable(Of Double), Optional Delimiter$ = ",") As String
        If source.Count = 0 Then ToStr = "Null" : Exit Function
        Return source.ToArray.JoinDbl(Delimiter)
    End Function
    <Extension()>
    Public Function ToStr(ByRef source As List(Of Double), Optional Delimiter$ = ",") As String
        If source.Count = 0 Then ToStr = "Null" : Exit Function
        Return source.ToArray.JoinDbl(Delimiter)
    End Function

    <Extension()>
    Public Function JoinDbl(ByRef Source As IEnumerable(Of Double), Optional Delimiter$ = ",") As String
        Dim str As Text.StringBuilder = New Text.StringBuilder
        Dim c& = 0
        For Each n In Source
            If c <> 0 Then str.Append(Delimiter & CStr(n)) Else str.Append(CStr(n))
            c = c + 1
        Next
        Return str.ToString
    End Function

    <Extension()>
    Public Sub ImportFromArray(Of T)(ByRef source As List(Of T), Arr As Array)
        source.Clear()
        For Each n As T In Arr
            source.Add(n)
        Next
    End Sub

    <Extension()>
    Public Function toArray(ByRef Source As DataTable) As Object
        Return Source.AsEnumerable.Select(Function(d) DirectCast(d(0).ToString(), Object)).ToArray()
    End Function
    <Extension()>
    Public Function toDict(ByRef Source As DataTable, FieldKey$) As Dictionary(Of String, List(Of String))
        Dim temp As New Dictionary(Of String, List(Of String))

        For ii = 0 To Source.Rows.Count - 1
            Dim l As New List(Of String)
            For jj = 0 To Source.Columns.Count - 1
                If Source.Rows(ii).Item(jj).ToString <> FieldKey Then
                    l.Add(Source.Rows(ii).Item(jj).ToString)
                End If
            Next
            temp.Add(Source.Rows(ii).Item(FieldKey).ToString, l)
        Next
        Return temp
    End Function

    '//Frequently method
    Function Getfile(Multi As Boolean, Optional Title$ = "Select File to Import... ") As String
        With New OpenFileDialog() ' msoFileDialogFilePicker
            .InitialDirectory = "D:\"
            If Len(Title) > 0 Then .Title = Title
            .Multiselect = Multi
            .Filter = "All File|*.*|Excel|*.xls;*.xlsx;*.xlsb;*.xlsm"
            .RestoreDirectory = True
            If .ShowDialog() = DialogResult.OK Then
                Return .FileNames.ToStr("||")
            Else
                Return "0"
            End If
        End With
    End Function

    Private Function ShowThreads() As String
        Return String.Format("ThreadID:{0} -- ThreadState:{1}", Threading.Thread.CurrentThread.ManagedThreadId, Threading.Thread.CurrentThread.ThreadState)
    End Function

    Public Function Trans1D(Inp As Object) As Object
        Dim tmpArr() As VariantType = Nothing
        Dim X%, y%
        Dim Ux%, Uy%, Lx%, Ly%, I%
        Try
            Select Case CType(Inp, Array).Rank
                Case 2
                    Dim g(,) = CType(Inp, VariantType(,))
                    Ux = UBound(g, 1) : Lx = LBound(g, 1)
                    Uy = UBound(g, 2) : Ly = LBound(g, 2)
                    Select Case True
                        Case Ux = Lx
                            If Ly = 0 Then ReDim tmpArr(0 To Uy - 1) Else ReDim tmpArr(0 To Uy - 1)
                            For y = Uy To Ly Step -1
                                If Ly = 0 Then tmpArr(y) = g(Lx, y) Else tmpArr(y - 1) = g(Lx, y)
                            Next
                        Case Uy = Ly
                            If Lx = 0 Then ReDim tmpArr(0 To Ux - 1) Else ReDim tmpArr(0 To Ux - 1)
                            For X = Ux To Lx Step -1
                                If Lx = 0 Then tmpArr(X) = g(X, Ly) Else tmpArr(X - 1) = g(X, Ly)
                            Next
                        Case Ux > Uy
                            ReDim tmpArr(0 To Ux * Uy - 1)
                            For y = Ly To Uy
                                For X = Lx To Ux
                                    If Not g(X, y) = vbEmpty Then tmpArr(I) = g(X, y) : I = I + 1
                            Next X, y
                            ReDim Preserve tmpArr(0 To I - 1)
                        Case Uy > Ux
                            ReDim tmpArr(0 To Ux * Uy - 1)
                            For X = Lx To Ux
                                For y = Ly To Uy
                                    If Not g(X, y) = vbEmpty Then tmpArr(I) = g(X, y) : I = I + 1
                            Next y, X
                            ReDim Preserve tmpArr(0 To I - 1)
                    End Select
                    Return tmpArr
                Case 0
                    Return [Inp].ToString
                Case 1
                    Return Inp.ToString
                Case Else
                    Return False
                    Exit Function
            End Select
        Catch e As Exception
            MsgBox(e.Message,, "An Error has been occurs")
            Return False
        End Try
    End Function

    Public Function Trans(Inp As Object) As Object
        Dim X%, y%
        Dim Ux%, Uy%, Lx%, Ly%
        Try
            Select Case CType(Inp, Array).Rank
                Case 0
                    Return False
                    Exit Function
                Case 1
                    Dim g() = CType(Inp, VariantType())
                    Ux = UBound(g)
                    Lx = LBound(g)

                    Dim tmpArr(0 To Ux - Lx + 1, 0 To 0) As VariantType

                    For X = Ux To Lx Step -1
                        tmpArr(X, 1) = g(X)
                    Next
                    Return tmpArr
                Case 2
                    Dim g(,) = CType(Inp, VariantType(,))
                    Ux = UBound(g, 1) : Lx = LBound(g, 1)
                    Uy = UBound(g, 2) : Ly = LBound(g, 2)

                    Select Case True
                        Case Ux = Lx
                            Dim tmpArr(0 To Uy - Ly + 1, 0 To 0) As VariantType
                            For y = Uy To Ly Step -1
                                tmpArr(y, 1) = g(Ly, y)
                            Next
                            Return tmpArr
                        Case Uy = Ly
                            Dim tmpArr(0 To 0, 0 To Ux - Lx + 1) As VariantType
                            For X = Ux To Lx Step -1
                                tmpArr(1, X) = g(X, Lx)
                            Next
                            Return tmpArr
                        Case Else
                            Dim tmpArr(0 To Uy - Ly + 1, 0 To Ux - Lx + 1) As VariantType
                            For y = Ly To Uy
                                For X = Lx To Ux
                                    tmpArr(y, X) = g(X, y)
                                Next
                            Next
                            Return tmpArr
                    End Select
                Case Else
                    Return False
            End Select

        Catch e As Exception
            MsgBox(e.Message,, "An Error has been occurs")
            Return False
        End Try
    End Function

    Public Function C2List(ByVal Inp As String, Optional Deli$ = "|") As List(Of String)
        Dim ii%

        If InStr(1, Inp, "=") > 0 Then Inp = Split(Inp, "=")(1)
        Inp = Replace(Inp, "(", vbNullString)
        Inp = Replace(Inp, ")", vbNullString)

        C2List = New List(Of String)
        For ii = 0 To UBound(Split(Inp, Deli))
            C2List.Add(Split(Inp, Deli)(ii))
        Next
    End Function

    Public Function ReadFile(FName$, Optional Deli$ = ",") As String
        Dim tmp As List(Of String)
        tmp = New List(Of String)
        Try
            ' Create an instance of StreamReader to read from a file. 
            ' The using statement also closes the StreamReader. 
            Using sr As IO.StreamReader = New IO.StreamReader(FName)
                Dim line As String
                ' Read and display lines from the file until the end of  
                ' the file is reached. 
                Do
                    line = sr.ReadLine()
                    tmp.Add(line)
                Loop Until (line <> Nothing)
            End Using
            Return tmp.ToStr(Deli)
        Catch e As Exception
            ' Let the user know what went wrong.
            MsgBox("The file could not be read:" & vbCrLf & e.Message)
            Return "0"
        End Try
    End Function
    Public Function ReadFile(FName$) As List(Of String)
        Dim tmp As List(Of String)
        tmp = New List(Of String)
        Try
            ' Create an instance of StreamReader to read from a file. 
            ' The using statement also closes the StreamReader. 
            Using sr As IO.StreamReader = New IO.StreamReader(FName)
                Dim line As String
                ' Read and display lines from the file until the end of  
                ' the file is reached. 
                Do
                    line = sr.ReadLine()
                    tmp.Add(line)
                Loop Until (line <> Nothing)
            End Using
            Return tmp
        Catch e As Exception
            ' Let the user know what went wrong.
            MsgBox("The file could not be read:" & vbCrLf & e.Message)
            Return New List(Of String) From {"0"}
        End Try
    End Function
    Public Function WriteFile(ByRef Arr As List(Of String), fileName$, Optional DesPath$ = "") As Boolean ' HeaderArr,
        Dim s As String
        Try
            If DesPath.Length = 0 Then DesPath = DesktopPath()
            Using sw As IO.StreamWriter = New IO.StreamWriter(DesPath & fileName)
                For Each s In Arr
                    sw.WriteLine(s)
                Next s
            End Using
            Return True
        Catch e As Exception
            MsgBox("An error occured:" & vbCrLf & e.Message)
            Return False
        End Try
    End Function

    Public Function DesktopPath() As String
        DesktopPath = Environ("USERPROFILE") & "\Desktop\"
    End Function

    'Relative path of project
    Public Function CurProjectPath() As String
        '//2 other ways
        'Application.Info.DirectoryPath
        'Application.ExecutablePath
        Return Reflection.Assembly.GetEntryAssembly().Location
    End Function
    Public Function getParentPath(ByVal Path$, Num&) As String
        For ii& = 1 To Num
            Path = IO.Directory.GetParent(Path).FullName
        Next
        Return Path
    End Function
End Module

