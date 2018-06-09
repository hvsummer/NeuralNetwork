


Public Class DArray

    Private Val() As Double
    Private iCol() As Integer
    Public Rows&
    Public Columns&
    Public Sub New(x&, y&)
        Val = New Double(x * y - 1) {}
        iCol = New Integer(y - 1) {}
        For i = 0 To y - 1
            iCol(i) = i * y
        Next
        Rows = x
        Columns = y
    End Sub
    Public Sub New(Arr As DArray)
        Erase Val
        Erase iCol
        Val = Arr.Val
        iCol = Arr.iCol
        Rows = Arr.Rows
        Columns = Arr.Columns
    End Sub

    Default Public Property Items(RowIndex&, ColIndex&) As Double
        Get
            Return Val(iCol(ColIndex) + RowIndex)
        End Get
        Set(value As Double)
            Val(iCol(ColIndex) + RowIndex) = value
        End Set
    End Property

    Public Property index(RowIndex&) As Double
        Get
            Return Val(RowIndex)
        End Get
        Set(value As Double)
            Val(RowIndex) = value
        End Set
    End Property

    'Methods
    Public Function T() As DArray
        Dim Result As New DArray(Columns, Rows)
        Select Case True
            Case Columns = 1 Or Rows = 1
                For i = 0 To Val.Length - 1
                    Result.index(i) = Val(i)
                Next
            Case Else
                Dim NewRow& = Columns
                Dim NewCol& = Rows
                For j = 0 To NewCol
                    For i = 0 To NewRow
                        Result.index(j * NewRow + i) = Val(i * NewCol + j)
                    Next
                Next
        End Select
        Return Result
    End Function


    '//Operators
    Public Shared Operator *(obj1 As DArray, obj2 As DArray)
        If obj1 Is Nothing Or obj2 Is Nothing Then Throw New NullReferenceException("Dense Array is not valid, Null Ref Exception.")
        If obj1.Rows <> obj2.Rows Or obj1.Columns <> obj2.Columns Then Throw New Exception("Rows or column of Dense Array is not matched")
        Dim Result As New DArray(obj1.Rows, obj1.Columns)
        For i = 0 To obj1.Val.Length - 1
            Result.index(i) = obj1.index(i) * obj2.index(i)
        Next
        Return Result
    End Operator
    Public Shared Operator *(obj1 As DArray, obj2 As Double)
        If obj1 Is Nothing Then Throw New NullReferenceException("Dense Array is not valid, Null Ref Exception.")
        Dim Result As New DArray(obj1.Rows, obj1.Columns)
        For i = 0 To obj1.Val.Length - 1
            Result.index(i) = obj1.index(i) * obj2
        Next
        Return Result
    End Operator

    Public Shared Operator /(obj1 As DArray, obj2 As DArray)
        If obj1 Is Nothing Or obj2 Is Nothing Then Throw New NullReferenceException("Dense Array is not valid, Null Ref Exception.")
        If obj1.Rows <> obj2.Rows Or obj1.Columns <> obj2.Columns Then Throw New Exception("Rows or column of Dense Array is not matched")
        Dim Result As New DArray(obj1.Rows, obj1.Columns)
        For i = 0 To obj1.Val.Length - 1
            Result.index(i) = obj1.index(i) / obj2.index(i)
        Next
        Return Result
    End Operator
    Public Shared Operator /(obj1 As DArray, obj2 As Double)
        If obj1 Is Nothing Then Throw New NullReferenceException("Dense Array is not valid, Null Ref Exception.")
        Dim Result As New DArray(obj1.Rows, obj1.Columns)
        For i = 0 To obj1.Val.Length - 1
            Result.index(i) = obj1.index(i) / obj2
        Next
        Return Result
    End Operator

    Public Shared Operator +(obj1 As DArray, obj2 As DArray)
        If obj1 Is Nothing Or obj2 Is Nothing Then Throw New NullReferenceException("Dense Array is not valid, Null Ref Exception.")
        If obj1.Rows <> obj2.Rows Or obj1.Columns <> obj2.Columns Then Throw New Exception("Rows or column of Dense Array is not matched")
        Dim Result As New DArray(obj1.Rows, obj1.Columns)
        For i = 0 To obj1.Val.Length - 1
            Result.index(i) = obj1.index(i) + obj2.index(i)
        Next
        Return Result
    End Operator
    Public Shared Operator +(obj1 As DArray, obj2 As Double)
        If obj1 Is Nothing Then Throw New NullReferenceException("Dense Array is not valid, Null Ref Exception.")
        Dim Result As New DArray(obj1.Rows, obj1.Columns)
        For i = 0 To obj1.Val.Length - 1
            Result.index(i) = obj1.index(i) + obj2
        Next
        Return Result
    End Operator

    Public Shared Operator -(obj1 As DArray, obj2 As DArray)
        If obj1 Is Nothing Or obj2 Is Nothing Then Throw New NullReferenceException("Dense Array is not valid, Null Ref Exception.")
        If obj1.Rows <> obj2.Rows Or obj1.Columns <> obj2.Columns Then Throw New Exception("Rows or column of Dense Array is not matched")
        Dim Result As New DArray(obj1.Rows, obj1.Columns)
        For i = 0 To obj1.Val.Length - 1
            Result.index(i) = obj1.index(i) - obj2.index(i)
        Next
        Return Result
    End Operator
    Public Shared Operator -(obj1 As DArray, obj2 As Double)
        If obj1 Is Nothing Then Throw New NullReferenceException("Dense Array is not valid, Null Ref Exception.")
        Dim Result As New DArray(obj1.Rows, obj1.Columns)
        For i = 0 To obj1.Val.Length - 1
            Result.index(i) = obj1.index(i) - obj2
        Next
        Return Result
    End Operator

    Public Shared Function Sum(Source As DArray) As Double
        Dim Result As Double
        For i = 0 To Source.Val.Length - 1
            Result += Source.Val(i)
        Next
        Return Result
    End Function

    Protected Overrides Sub Finalize()
        Erase Val
        Erase iCol
        MyBase.Finalize()
    End Sub
End Class

