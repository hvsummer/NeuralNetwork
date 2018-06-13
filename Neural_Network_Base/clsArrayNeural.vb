


Public Class DArray

    Private Val() As Double
    Private iCol() As Integer
    Public Rows%
    Public Columns%

    Public Sub New(x%, y%)
        Val = New Double(x * y - 1) {}
        iCol = New Integer(y - 1) {}
        For i = 0 To y - 1
            iCol(i) = i * x
        Next
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
        iCol = Enumerable.Range(0, y).ToArray

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
                s.Append(Items(i, j)).Append(vbTab)
            Next
            s.Append(vbCrLf)
        Next
        Return s.ToString
    End Function

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
                        Result.index(i * NewRow + j) = Val(i * NewRow + j)
                    Next
                Next
        End Select
        Return Result
    End Function

    Public Function Transpose(ByVal Mat(,) As Double) As Double(,)
        Dim Tr_Mat(,) As Double
        Dim i, j, Rows, Cols As Integer

        On Error GoTo Error_Handler

        Rows = Mat.GetUpperBound(0)
        Cols = Mat.GetUpperBound(1)
        ReDim Tr_Mat(Cols, Rows)

        For i = 0 To Cols
            For j = 0 To Rows
                Tr_Mat(j, i) = Mat(i, j)
            Next j
        Next i

        Return Tr_Mat

Error_Handler:
        Err.Raise(5028, , "In order to do this operation values must be assigned to the matrix !!")
    End Function
    Public Function Fill(Value As Double) As DArray
        For i = 0 To Val.Length - 1
            Val(i) = Value
        Next
        Return Me
    End Function
    Public Function Fill(StartValue As Double, EndValue As Double) As DArray
        Dim r As New Random(CInt(Date.Now.ToOADate * 10 ^ 5))
        Dim fv As Double
        For i = 0 To Val.Length - 1
            Do
                fv = r.Next(CInt(StartValue * 10 ^ 6), CInt(EndValue * 10 ^ 6)) / 10 ^ 6
            Loop Until fv <> 0
            Val(i) = fv
        Next
        Return Me
    End Function

    Public Function ApplyFunc(Fomular As Func(Of Double, Double)) As DArray
        For i = 0 To Val.Length - 1
            Val(i) = Fomular(Val(i))
        Next
        Return Me
    End Function


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
        If obj1 Is Nothing Or obj2 Is Nothing Then Throw New NullReferenceException("Dense Array is not valid, Null Ref Exception.") : Return Nothing
        Dim Result As New DArray(If(obj1.Rows > obj2.Rows, obj1.Rows, obj2.Rows), obj1.Columns)
        If obj1.Columns <> obj2.Columns Then
            Throw New Exception("column of Dense Array is not matched") : Return Nothing
        Else
            If (obj1.Rows = 1 Or obj2.Rows = 1) And obj1.Rows <> obj2.Rows Then
                For i = 0 To If(obj1.Rows > obj2.Rows, obj1.Rows, obj2.Rows) - 1
                    For j = 0 To obj1.Columns - 1
                        If obj1.Rows = 1 Then
                            Result.Items(i, j) = obj2.Items(i, j) + obj1(0, j)
                        Else
                            Result.Items(i, j) = obj1.Items(i, j) + obj2(0, j)
                        End If
                    Next
                Next
                Return Result
            Else
                Throw New Exception("row and column of Dense Array is not matched") : Return Nothing
            End If
        End If
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
        For i = 0 To s1.Rows - 1
            For j = 0 To S2.Columns - 1
                c.Items(i, j) = 0
                For k = 0 To s1.Columns - 1
                    c.Items(i, j) += s1.Items(i, k) * S2.Items(k, j)
                Next
            Next
        Next
        Return c
    End Function
    Public Function Multiply(ByRef Source As DArray) As DArray
        Dim c As New DArray(Rows, Source.Columns)
        For i = 0 To Rows - 1
            For j = 0 To Source.Columns - 1
                c.Items(i, j) = 0
                For k = 0 To Columns - 1
                    c.Items(i, j) += Items(i, k) * Source.Items(k, j)
                Next
            Next
        Next
        Return c
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

