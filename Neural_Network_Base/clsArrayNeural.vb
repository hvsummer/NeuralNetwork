


Public Class DArray

    Private Val() As Double
    Private iCol() As Integer
    Public Rows%
    Public Columns%

    Public Sub New(x%, y%)
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
    Public Sub New(Arr As List(Of Double))
        Dim y = Arr.Count

        Erase Val
        Erase iCol

        Val = Arr.ToArray
        For i = 0 To y - 1
            iCol(i) = i * y
        Next

        Rows = 1
        Columns = y
    End Sub
    Protected Overrides Sub Finalize()
        Erase Val
        Erase iCol
        MyBase.Finalize()
    End Sub

    Default Public Property Items(RowIndex%, ColIndex%) As Double
        Get
            Return Val(iCol(ColIndex) + RowIndex)
        End Get
        Set(value As Double)
            Val(iCol(ColIndex) + RowIndex) = value
        End Set
    End Property

    Public Property index(RowIndex%) As Double
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
                Dim NewRow% = Columns
                Dim NewCol% = Rows
                For j = 0 To NewCol
                    For i = 0 To NewRow
                        Result.index(j * NewRow + i) = Val(i * NewCol + j)
                    Next
                Next
        End Select
        Return Result
    End Function

    Public Sub Fill(Value As Double)
        For i = 0 To Val.Length - 1
            Val(i) = Value
        Next
    End Sub
    Public Sub Fill(StartValue As Double, EndValue As Double)
        Dim r As New Random(CInt(Date.Now.ToOADate * 10 ^ 5))
        For i = 0 To Val.Length - 1
            Val(i) = r.Next(CInt(StartValue * 10 ^ 6), CInt(EndValue * 10 ^ 6)) / 10 ^ 6
        Next
    End Sub

    Public Sub ApplyFunc(Fomular As Func(Of Double, Double))
        For i = 0 To Val.Length - 1
            Val(i) = Fomular(Val(i))
        Next
    End Sub


    '//Operators
    Public Shared Operator *(obj1 As DArray, obj2 As DArray) As DArray
        If obj1 Is Nothing Or obj2 Is Nothing Then Throw New NullReferenceException("Dense Array is not valid, Null Ref Exception.")
        If obj1.Rows <> obj2.Rows Or obj1.Columns <> obj2.Columns Then Throw New Exception("Rows or column of Dense Array is not matched")
        Dim Result As New DArray(obj1.Rows, obj1.Columns)
        For i = 0 To obj1.Val.Length - 1
            Result.index(i) = obj1.index(i) * obj2.index(i)
        Next
        Return Result
    End Operator
    Public Shared Operator *(obj1 As DArray, obj2 As Double) As DArray
        If obj1 Is Nothing Then Throw New NullReferenceException("Dense Array is not valid, Null Ref Exception.")
        Dim Result As New DArray(obj1.Rows, obj1.Columns)
        For i = 0 To obj1.Val.Length - 1
            Result.index(i) = obj1.index(i) * obj2
        Next
        Return Result
    End Operator

    Public Shared Operator /(obj1 As DArray, obj2 As DArray) As DArray
        If obj1 Is Nothing Or obj2 Is Nothing Then Throw New NullReferenceException("Dense Array is not valid, Null Ref Exception.")
        If obj1.Rows <> obj2.Rows Or obj1.Columns <> obj2.Columns Then Throw New Exception("Rows or column of Dense Array is not matched")
        Dim Result As New DArray(obj1.Rows, obj1.Columns)
        For i = 0 To obj1.Val.Length - 1
            Result.index(i) = obj1.index(i) / obj2.index(i)
        Next
        Return Result
    End Operator
    Public Shared Operator /(obj1 As DArray, obj2 As Double) As DArray
        If obj1 Is Nothing Then Throw New NullReferenceException("Dense Array is not valid, Null Ref Exception.")
        Dim Result As New DArray(obj1.Rows, obj1.Columns)
        For i = 0 To obj1.Val.Length - 1
            Result.index(i) = obj1.index(i) / obj2
        Next
        Return Result
    End Operator

    Public Shared Operator +(obj1 As DArray, obj2 As DArray) As DArray
        If obj1 Is Nothing Or obj2 Is Nothing Then Throw New NullReferenceException("Dense Array is not valid, Null Ref Exception.")
        If obj1.Rows <> obj2.Rows Or obj1.Columns <> obj2.Columns Then Throw New Exception("Rows or column of Dense Array is not matched")
        Dim Result As New DArray(obj1.Rows, obj1.Columns)
        For i = 0 To obj1.Val.Length - 1
            Result.index(i) = obj1.index(i) + obj2.index(i)
        Next
        Return Result
    End Operator
    Public Shared Operator +(obj1 As DArray, obj2 As Double) As DArray
        If obj1 Is Nothing Then Throw New NullReferenceException("Dense Array is not valid, Null Ref Exception.")
        Dim Result As New DArray(obj1.Rows, obj1.Columns)
        For i = 0 To obj1.Val.Length - 1
            Result.index(i) = obj1.index(i) + obj2
        Next
        Return Result
    End Operator

    Public Shared Operator -(obj1 As DArray, obj2 As DArray) As DArray
        If obj1 Is Nothing Or obj2 Is Nothing Then Throw New NullReferenceException("Dense Array is not valid, Null Ref Exception.")
        If obj1.Rows <> obj2.Rows Or obj1.Columns <> obj2.Columns Then Throw New Exception("Rows or column of Dense Array is not matched")
        Dim Result As New DArray(obj1.Rows, obj1.Columns)
        For i = 0 To obj1.Val.Length - 1
            Result.index(i) = obj1.index(i) - obj2.index(i)
        Next
        Return Result
    End Operator
    Public Shared Operator -(obj1 As DArray, obj2 As Double) As DArray
        If obj1 Is Nothing Then Throw New NullReferenceException("Dense Array is not valid, Null Ref Exception.")
        Dim Result As New DArray(obj1.Rows, obj1.Columns)
        For i = 0 To obj1.Val.Length - 1
            Result.index(i) = obj1.index(i) - obj2
        Next
        Return Result
    End Operator

    Public Function Zip(ByRef b As DArray, Fomular As Func(Of Double, Double, Double)) As DArray
        If b Is Nothing Then Throw New NullReferenceException("Dense Array is not valid, Null Ref Exception.")
        Dim Result As New DArray(Rows, Columns)
        For i = 0 To Val.Length - 1
            Result.index(i) = Fomular(Val(i), b.Val(i))
        Next
        Return Result
    End Function

    Public Function Dot(ByRef Source As DArray) As Double
        Return Zip(Source, Function(x, y) x * y).Sum()
    End Function

    Public Shared Function Multiply(ByRef s1 As DArray, ByRef S2 As DArray) As DArray
        Dim c As New DArray(s1.Rows, S2.Columns)
        For i = 1 To s1.Rows
            For j = 1 To S2.Columns
                c.Items(i, j) = 0
                For k = 1 To s1.Columns
                    c.Items(i, j) += s1.Items(i, k) * S2.Items(k, j)
                Next
            Next
        Next        Return c
    End Function
    Public Function Multiply(ByRef Source As DArray) As DArray
        Dim c As New DArray(Rows, Source.Columns)
        For i = 1 To Rows
            For j = 1 To Source.Columns
                c.Items(i, j) = 0
                For k = 1 To Columns
                    c.Items(i, j) += Items(i, k) * Source.Items(k, j)
                Next
            Next
        Next        Return c
    End Function

    Private Function Sum() As Double
        Dim Result As Double
        For i = 0 To Val.Length - 1
            Result += Val(i)
        Next
        Return Result
    End Function

    Public Shared Function Sum(Source As DArray) As Double
        Dim Result As Double
        For i = 0 To Source.Val.Length - 1
            Result += Source.Val(i)
        Next
        Return Result
    End Function

End Class

