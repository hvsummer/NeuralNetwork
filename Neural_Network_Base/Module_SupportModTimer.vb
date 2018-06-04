﻿Option Explicit On

Module Module_SupportModTimer
    Private CTime As Double
    Private wTime As Stopwatch
    ' ****************************************************************
    Public Function GetTime()
        Dim res As Double = DateTime.Now.ToOADate
        CTime = DateTime.Now.ToOADate
        Return Math.Round(res - CTime, 6)
    End Function
    Public Sub setTime()
        CTime = DateTime.Now.ToOADate
    End Sub
    Public Sub Perf_Start()
        If wTime Is Nothing Then wTime = New Stopwatch
        wTime.Start()
    End Sub
    Public Function Perf_Lap() As Double
        Dim res As Double
        res = wTime.Elapsed.TotalMilliseconds
        wTime.Reset()
        wTime.Start()
        Return res
    End Function
End Module
