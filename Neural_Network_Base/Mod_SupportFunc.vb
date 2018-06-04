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
    Public Function ToStr(ByRef source As IEnumerable(Of Double), Optional Delimiter$ = ",") As String
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
        For Each n In Arr
            source.Add(n)
        Next
    End Sub

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

    Public Function Trans1D(Inp As Array)
        Dim tmpArr As New Object
        Dim X&, y&
        Dim Ux&, Uy&, Lx&, Ly&, I&
        Try
            Select Case Inp.Rank
                Case 2
                    Ux = UBound(Inp, 1) : Lx = LBound(Inp, 1)
                    Uy = UBound(Inp, 2) : Ly = LBound(Inp, 2)
                    Select Case True
                        Case Ux = Lx
                            If Ly = 0 Then ReDim tmpArr(0 To Uy - 1) Else ReDim tmpArr(0 To Uy - 1)
                            For y = Uy To Ly Step -1
                                If Ly = 0 Then tmpArr(y) = Inp(Lx, y) Else tmpArr(y - 1) = Inp(Lx, y)
                            Next
                        Case Uy = Ly
                            If Lx = 0 Then ReDim tmpArr(0 To Ux - 1) Else ReDim tmpArr(0 To Ux - 1)
                            For X = Ux To Lx Step -1
                                If Lx = 0 Then tmpArr(X) = Inp(X, Ly) Else tmpArr(X - 1) = Inp(X, Ly)
                            Next
                        Case Ux > Uy
                            ReDim tmpArr(0 To Ux * Uy - 1)
                            For y = Ly To Uy
                                For X = Lx To Ux
                                    If Not Inp(X, y) = vbEmpty Then tmpArr(I) = Inp(X, y) : I = I + 1
                            Next X, y
                            ReDim Preserve tmpArr(0 To I - 1)
                        Case Uy > Ux
                            ReDim tmpArr(0 To Ux * Uy - 1)
                            For X = Lx To Ux
                                For y = Ly To Uy
                                    If Not Inp(X, y) = vbEmpty Then tmpArr(I) = Inp(X, y) : I = I + 1
                            Next y, X
                            ReDim Preserve tmpArr(0 To I - 1)
                    End Select
                Case 0
                    tmpArr = [Inp]
                Case 1
                    tmpArr = Inp
                Case Else
                    Return False
                    Exit Function
            End Select
            Return tmpArr
        Catch e As Exception
            MsgBox(e.Message,, "An Error has been occurs")
            Return False
        End Try
    End Function

    Public Function Trans(Inp As Array)
        Dim tmpArr As New Object
        Dim X&, y&
        Dim Ux&, Uy&, Lx&, Ly&
        Try
            Select Case Inp.Rank
                Case 0
                    Return False
                    Exit Function
                Case 1
                    Ux = UBound(Inp)
                    Lx = LBound(Inp)
                    ReDim tmpArr(0 To Ux - Lx + 1, 0 To 0)
                    For X = Ux To Lx Step -1
                        tmpArr(X, 1) = Inp(X)
                    Next
                Case 2
                    Ux = UBound(Inp, 1) : Lx = LBound(Inp, 1)
                    Uy = UBound(Inp, 2) : Ly = LBound(Inp, 2)
                    Select Case True
                        Case Ux = Lx
                            ReDim tmpArr(0 To Uy - Ly + 1, 0 To 0)
                            For y = Uy To Ly Step -1
                                tmpArr(y, 1) = Inp(Ly, y)
                            Next
                        Case Uy = Ly
                            ReDim tmpArr(0 To 0, 0 To Ux - Lx + 1)
                            For X = Ux To Lx Step -1
                                tmpArr(1, X) = Inp(X, Lx)
                            Next
                        Case Else
                            ReDim tmpArr(0 To Uy - Ly + 1, 0 To Ux - Lx + 1)
                            For y = Ly To Uy
                                For X = Lx To Ux
                                    tmpArr(y, X) = Inp(X, y)
                                Next
                            Next
                    End Select
            End Select
            Return tmpArr
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

    Public Function ReadFile(FName$, Optional Deli$ = ",")
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
    Public Function ReadFile(FName$)
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
            Return "0"
        End Try
    End Function
    Public Function WriteFile(ByRef Arr As List(Of String), fileName$, Optional DesPath$ = "") ' HeaderArr,
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

    Public Function CurProjectPath()
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

