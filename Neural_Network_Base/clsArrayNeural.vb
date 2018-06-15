


Public Class DArray

    Private Val() As Double
    Private iCol() As Integer
    Public Rows%
    Public Columns%

    Public Sub New(x%, y%)
        Val = New Double(x * y - 1) {}
        iCol = Enumerable.Range(0, y).Zip(Enumerable.Repeat(x, y), Function(a, b) a * b).ToArray

        'For i = 0 To y - 1
        'iCol(i) = i * x
        'Next

        Rows = x
        Columns = y
    End Sub
    Public Sub New(Arr As DArray)
        Val = Arr.Val
        iCol = Arr.iCol
        Rows = Arr.Rows
        Columns = Arr.Columns
    End Sub
    Public Sub New(Arr As List(Of Double))
        Dim y = Arr.Count - 1
        ReDim iCol(0 To y)

        Val = Arr.ToArray
        iCol = Enumerable.Range(0, y + 1).ToArray

        Rows = 1
        Columns = y + 1
    End Sub
    Public Sub New(Arr As List(Of Double), split%)
        Dim y = Arr.Count - 1
        ReDim iCol(0 To CInt((y + 1) / split) - 1)

        For i = 0 To y Step split
            If Val Is Nothing Then
                Val = Arr.GetRange(i, split).ToArray()
            Else
                Val = Val.Concat(Arr.GetRange(i, split).ToArray()).ToArray
            End If
        Next

        For i = 0 To CInt((y + 1) / split) - 1
            iCol(i) = i * split
        Next

        Rows = split
        Columns = CInt((y + 1) / split)
    End Sub
    Protected Overrides Sub Finalize()
        Erase Val
        Erase iCol
        MyBase.Finalize()
    End Sub


    Public ReadOnly Property T() As DArray
        Get
            Dim Result As New DArray(Columns, Rows)
            Dim NewRow% = Columns
            Dim NewCol% = Rows
            For i = 0 To NewCol - 1
                For j = 0 To NewRow - 1
                    Result.Items(j, i) = Items(i, j)
                Next
            Next
            Return Result
        End Get
    End Property
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

    'Convert Method
    Public Function ToList() As List(Of Double)
        Return Val.ToList()
    End Function

    Public Shared Widening Operator CType(ByVal Value As List(Of Double)) As DArray
        Return New DArray(Value)
    End Operator
    Public Shared Narrowing Operator CType(ByVal Value As DArray) As List(Of Double)
        Return Value.Val.ToList()
    End Operator

    Public Overrides Function ToString() As String
        Dim s As New Text.StringBuilder
        For i = 0 To Rows - 1
            For j = 0 To Columns - 1
                s.Append(Math.Round(Items(i, j), 8).ToString("0.############################")).Append(vbTab)
            Next
            s.Append(vbCrLf)
        Next
        Return s.ToString
    End Function

    'Methods

    Public Function Fill(Value As Double) As DArray
        For i = 0 To Val.Length - 1
            Val(i) = Value
        Next
        Return Me
    End Function
    Public Function Fill(StartValue As Double, EndValue As Double) As DArray
        Dim r As New Random(CInt(Date.Now.Ticks And &HFFFF))
        Dim fv As Double
        For i = 0 To Val.Length - 1
            Do
                fv = r.Next(CInt(StartValue * 10 ^ 6), CInt(EndValue * 10 ^ 6)) / 10 ^ 6
            Loop Until fv <> 0
            Val(i) = fv
        Next
        Return Me
    End Function
    Public Function Fill(StartValue As Double, EndValue As Double, TargetValue As Double) As DArray
        Dim r As New Random(CInt(Date.Now.Ticks And &HFFFF))
        Dim fv As Double
        For i = 0 To Val.Length - 1
            If Math.Abs(Val(i)) > TargetValue Then
                Do
                    fv = r.Next(CInt(StartValue * 10 ^ 6), CInt(EndValue * 10 ^ 6)) / 10 ^ 6
                Loop Until fv <> 0
                Val(i) = fv
            End If
        Next
        Return Me
    End Function

    Public Function ApplyFunc(Fomular As Func(Of Double, Double), Optional ChangeSource As Boolean = False) As DArray
        If ChangeSource Then
            For i = 0 To Val.Length - 1
                Val(i) = Fomular(Val(i))
            Next
            Return Me
        Else
            Dim R As New DArray(Rows, Columns)
            For i = 0 To Val.Length - 1
                R.Val(i) = Fomular(Val(i))
            Next
            Return R
        End If

    End Function


    '//Operators
    Public Shared Operator *(obj1 As DArray, obj2 As DArray) As DArray
        If obj1.Columns <> obj2.Rows Then
            If obj1.Rows = obj2.Rows And obj1.Columns = obj2.Columns Then Return ScalarMulti(obj1, obj2)
            Throw New Exception("Result does not exits for 2 matrix A and B which A.Columns <> B.Rows")
            Return Nothing
        End If
        Dim c As New DArray(obj1.Rows, obj2.Columns)
        For i = 0 To obj1.Rows - 1
            For j = 0 To obj2.Columns - 1
                c.Items(i, j) = 0
                For k = 0 To obj1.Columns - 1
                    c.Items(i, j) += obj1.Items(i, k) * obj2.Items(k, j)
                Next
            Next
        Next
        Return c
    End Operator
    Public Shared Operator *(obj1 As DArray, obj2 As Double) As DArray
        If obj1 Is Nothing Then Throw New NullReferenceException("Dense Array is not valid, Null Ref Exception.")
        Return obj1.Zip(obj2, Function(x, y) x * y)
    End Operator

    Public Shared Operator /(obj1 As DArray, obj2 As DArray) As DArray
        Throw New NullReferenceException("There are no Divide calculation for Matrix A and Matrix B (A/B = undifined)") : Return Nothing
    End Operator
    Public Shared Operator /(obj1 As DArray, obj2 As Double) As DArray
        If obj1 Is Nothing Then Throw New NullReferenceException("Dense Array is not valid, Null Ref Exception.")
        Return obj1.Zip(obj2, Function(x, y) x / y)
    End Operator

    Public Shared Operator +(obj1 As DArray, obj2 As DArray) As DArray
        If obj1 Is Nothing Or obj2 Is Nothing Then Throw New NullReferenceException("Dense Array is not valid, Null Ref Exception.") : Return Nothing
        Dim Result As New DArray(obj1.Rows, obj1.Columns)
        If obj1.Columns <> obj2.Columns Or obj1.Rows <> obj2.Rows Then Throw New Exception("Row/Column of Dense Array is not matched") : Return Nothing

        For i = 0 To obj1.Val.Length - 1
            Result.index(i) = obj1.index(i) + obj2.index(i)
        Next
        Return Result
    End Operator
    Public Shared Operator +(obj1 As DArray, obj2 As Double) As DArray
        If obj1 Is Nothing Then Throw New NullReferenceException("Dense Array is not valid, Null Ref Exception.")
        Return obj1.Zip(obj2, Function(x, y) x + y)
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
        Return obj1.Zip(obj2, Function(x, y) x - y)
    End Operator

    Public Function Zip(ByRef b As DArray, Fomular As Func(Of Double, Double, Double)) As DArray
        If b Is Nothing Then Throw New NullReferenceException("Dense Array is not valid, Null Ref Exception.")
        Dim Result As New DArray(Rows, Columns)
        For i = 0 To Val.Length - 1
            Result.index(i) = Fomular(Val(i), b.Val(i))
        Next
        Return Result
    End Function
    Public Function Zip(ByRef b As Double, Fomular As Func(Of Double, Double, Double)) As DArray
        Dim Result As New DArray(Rows, Columns)
        For i = 0 To Val.Length - 1
            Result.index(i) = Fomular(Val(i), b)
        Next
        Return Result
    End Function

    Public Shared Function ScalarMulti(ByRef obj1 As DArray, ByRef obj2 As DArray) As DArray
        If obj1 Is Nothing Or obj2 Is Nothing Then Throw New NullReferenceException("Dense Array is not valid, Null Ref Exception.")
        If obj1.Rows <> obj2.Rows Or obj1.Columns <> obj2.Columns Then Throw New Exception("Rows or column of Dense Array is not matched")
        Return obj1.Zip(obj2, Function(x, y) x * y)
    End Function
    Public Function ScalarMulti(ByRef Source As DArray) As DArray
        Return Me.Zip(Source, Function(x, y) x * y)
    End Function

    Private Function Sum() As Double
        Dim Result As Double
        For i = 0 To Val.Length - 1
            Result += Val(i)
        Next
        Return Result
    End Function
    Public Function Min() As Double
        Return Val.Min()
    End Function
    Public Function Max() As Double
        Return Val.Max()
    End Function

    Public Shared Function Sum(Source As DArray) As Double
        Dim Result As Double
        For i = 0 To Source.Val.Length - 1
            Result += Source.Val(i)
        Next
        Return Result
    End Function

End Class

