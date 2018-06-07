


Public Class DArray(Of H)

    Private Val() As H
    Private iCol() As Long
    Public Rows&
    Public Columns&
    Public Sub New(x&, y&)
        ReDim Val(x * y - 1)
        ReDim iCol(x * y - 1)
        For i = 0 To y - 1
            iCol(i) = i * y
        Next
        Rows = x
        Columns = y
    End Sub
    Public Sub New(Arr As DArray(Of H))
        Erase Val
        Erase iCol
        Val = Arr.Val
        iCol = Arr.iCol
        Rows = Arr.Rows
        Columns = Arr.Columns
    End Sub

    Default Public Property Items(RowIndex&, ColIndex&)
        Get
            Return Val(iCol(ColIndex) + RowIndex)
        End Get
        Set(value)
            Val(iCol(ColIndex) + RowIndex) = value
        End Set
    End Property

    Default Public Property Items(RowIndex&)
        Get
            Return Val(RowIndex)
        End Get
        Set(value)
            Val(RowIndex) = value
        End Set
    End Property

    'Methods
    Public Function T() As DArray(Of H)
        Dim Result As New DArray(Of H)(Columns, Rows)
        Select Case True
            Case Columns = 1 Or Rows = 1
                For i = 0 To Val.Length - 1
                    Result(i) = Val(i)
                Next
            Case Else
                Dim NewRow& = Columns
                Dim NewCol& = Rows
                For j = 0 To NewCol
                    For i = 0 To NewRow
                        Result(j * NewRow + i) = Val(i * NewCol + j)
                    Next
                Next
        End Select
        Return Result
    End Function


    '//Operators
    Public Shared Operator *(obj1 As DArray(Of H), obj2 As DArray(Of H))
        If obj1 Is Nothing Or obj2 Is Nothing Then Throw New NullReferenceException("Dense Array is not valid, Null Ref Exception.")
        If obj1.Rows <> obj2.Rows Or obj1.Columns <> obj2.Columns Then Throw New Exception("Rows or column of Dense Array is not matched")
        Dim Result As New DArray(Of H)(obj1.Rows, obj1.Columns)
        For i = 0 To obj1.Val.Length - 1
            Result(i) = obj1(i) * obj2(i)
        Next
        Return Result
    End Operator

    Public Shared Operator +(obj1 As DArray(Of H), obj2 As DArray(Of H))
        If obj1 Is Nothing Or obj2 Is Nothing Then Throw New NullReferenceException("Dense Array is not valid, Null Ref Exception.")
        If obj1.Rows <> obj2.Rows Or obj1.Columns <> obj2.Columns Then Throw New Exception("Rows or column of Dense Array is not matched")
        Dim Result As New DArray(Of H)(obj1.Rows, obj1.Columns)
        For i = 0 To obj1.Val.Length - 1
            Result(i) = obj1(i) + obj2(i)
        Next
        Return Result
    End Operator

    Public Shared Operator -(obj1 As DArray(Of H), obj2 As DArray(Of H))
        If obj1 Is Nothing Or obj2 Is Nothing Then Throw New NullReferenceException("Dense Array is not valid, Null Ref Exception.")
        If obj1.Rows <> obj2.Rows Or obj1.Columns <> obj2.Columns Then Throw New Exception("Rows or column of Dense Array is not matched")
        Dim Result As New DArray(Of H)(obj1.Rows, obj1.Columns)
        For i = 0 To obj1.Val.Length - 1
            Result(i) = obj1(i) - obj2(i)
        Next
        Return Result
    End Operator

    Public Shared Operator /(obj1 As DArray(Of H), obj2 As DArray(Of H))
        If obj1 Is Nothing Or obj2 Is Nothing Then Throw New NullReferenceException("Dense Array is not valid, Null Ref Exception.")
        If obj1.Rows <> obj2.Rows Or obj1.Columns <> obj2.Columns Then Throw New Exception("Rows or column of Dense Array is not matched")
        Dim Result As New DArray(Of H)(obj1.Rows, obj1.Columns)
        For i = 0 To obj1.Val.Length - 1
            Result(i) = obj1(i) / obj2(i)
        Next
        Return Result
    End Operator
End Class
