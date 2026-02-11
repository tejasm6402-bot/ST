Imports System.Windows.Forms
Imports System.Drawing
Imports Expert.Message.ExpertMessageBox
Imports ES.Common
Imports System.Threading
Imports System.IO
Imports ES.DAC
Imports ES.PAL
Imports System.Reflection
Imports System.Data.SqlClient
Imports System.Globalization
Imports System.IO.Ports
Imports System.Net
Imports Newtonsoft.Json.Linq


Public Class ApcPlay
    Dim FirstPlayString, SecondPlayString, ThiredPlayString As String
    Dim DvMyClips As New DataView
    Dim L_BusReportingID As Long
    Public L_StartingStationMasterId As String
    Public L_DestinationStationMasterId As Long
    Public L_ViaStationMasterId As Long
    Public L_ConductorBatchCode As String
    Public L_DriverBatchCode As String
    Public L_PlatFormNo As Integer
    Public L_BusNo As String
    Public AnnPlayBefore1Hour As Integer
    Public ScheduleAnnAfter As Integer
    Public ScheduleAnnTimeSpan As Integer
    Public AutoAnnSmartCArd As Boolean
    Dim _ETADFlag As Boolean = True
    Public VTS_Integration As String = "No"
    Public VTS_IntegrationCityBus As String = "No"
    Public VTS_IntegrationAutoBusAnn As String = "No"
    Public Auto_Integration As String = "N"
    Dim _BusReportingThread As Threading.Thread
    Dim _AutoBusAnnThread As Threading.Thread
    Dim _UPSReportingThread As Threading.Thread
    Dim _CSThread As Threading.Thread
    Dim _VrittiApcThread As Threading.Thread
    Dim _STAMainLogThread As Threading.Thread
    Dim _STAMainLogThreadCityBus As Threading.Thread

    Dim _PlayThread As Threading.Thread
    Dim _VTS As New VTSINTEREGRATION.ClsVTS
    Dim _VrittiApcDownloader As New VTSINTEREGRATION.ClsVrittiApcDownloader

    Dim UPSPORT As Integer = System.Configuration.ConfigurationSettings.AppSettings("UPSPORT")
    Dim UPSBAUDRATE As Integer = System.Configuration.ConfigurationSettings.AppSettings("UPSBAUDRATE")
    Dim MainStatusRate As Integer = System.Configuration.ConfigurationSettings.AppSettings("MainStatusRate")
    Dim MainPresent As Integer = System.Configuration.ConfigurationSettings.AppSettings("MainPresent")
    Dim MainAbsent As Integer = System.Configuration.ConfigurationSettings.AppSettings("MainAbsent")
    Dim UPSTIMER As Integer = System.Configuration.ConfigurationSettings.AppSettings("UPSTIMER")
    Dim UPSON As String = System.Configuration.ConfigurationSettings.AppSettings("UPSON")
    Dim UPS As String = System.Configuration.ConfigurationSettings.AppSettings("UPS")
    Dim ETAErrorCnt As Integer = 0
    Dim DualDisplay As Integer = 1
    Dim screen1position, screen2position As Point
    Dim mainFlag As String = ""
    Private SerialPort As SerialPort
    Dim stringComparer__1 As StringComparer = StringComparer.OrdinalIgnoreCase
    Dim SerialPorts As String() = System.IO.Ports.SerialPort.GetPortNames()
    Dim Mainsoncnt As Integer = 0
    Dim Mainsoffcnt As Integer = 0

    Private Sub Form1_Load(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles MyBase.Load
        Try
            GC.SuppressFinalize(Me)
        Catch ex As Exception

        End Try
        SetDatabase()
        SetParameterInfo()


        ClsUPSLog.TraceService(Now.ToString & ": in load APC player")


        If UPSPORT <> 0 Then
            UPSPORT = UPSPORT - 1
        End If

        Try
            If ScheduleAnnTimeSpan > 0 And ScheduleAnnAfter > 0 Then
                TimerScheduleAnn.Enabled = True
                TimerScheduleAnn.Interval = 100 ''0 * ScheduleAnnAfter * 60
                TimerScheduleAnn.Start()
            End If
        Catch ex As Exception

        End Try

        Try
            If UPS = "Y" Then
                Timer1.Enabled = True
                Timer1.Interval = 1000
                Timer1.Start()
            End If

        Catch ex As Exception

        End Try

        If VTS_Integration.ToLower = "yes" Then
            _STAMainLogThread = New Threading.Thread(AddressOf VTSLogThread)
            _STAMainLogThread.IsBackground = True
            If _STAMainLogThread.IsAlive = False Then
                _STAMainLogThread.Start()
            End If
            _BusReportingThread = New Threading.Thread(AddressOf BusThread)
            _BusReportingThread.IsBackground = True
            If _BusReportingThread.IsAlive = False Then
                _BusReportingThread.Start()
            End If
            Call VTSLogThread()
        End If



        If VTS_IntegrationCityBus.ToLower = "yes" Then
            _STAMainLogThreadCityBus = New Threading.Thread(AddressOf VTSLogThreadCityBus)
            _STAMainLogThreadCityBus.IsBackground = True
            If _STAMainLogThreadCityBus.IsAlive = False Then
                _STAMainLogThreadCityBus.Start()
            End If
            Call VTSLogThreadCityBus()
        End If

        'VTS_IntegrationAutoBusAnn
        If VTS_IntegrationAutoBusAnn.ToLower = "yes" Then
            _AutoBusAnnThread = New Threading.Thread(AddressOf AutoBusAnnThread)
            _AutoBusAnnThread.IsBackground = True
            If _AutoBusAnnThread.IsAlive = False Then
                _AutoBusAnnThread.Start()
            End If
        End If


        _VrittiApcThread = New Threading.Thread(AddressOf VrittiApcThread)
        _VrittiApcThread.IsBackground = True
        If _VrittiApcThread.IsAlive = False Then
            _VrittiApcThread.Start()
        End If

        Me.Visible = True
        Dim x As Integer
        Dim y As Integer
        Try
            x = Screen.PrimaryScreen.WorkingArea.Width
            y = Screen.PrimaryScreen.WorkingArea.Height - Me.Height

            Do Until x = Screen.PrimaryScreen.WorkingArea.Width - Me.Width
                x = x - 1
                Me.Location = New Point(x, y)
            Loop
        Catch ex As Exception

        End Try

        Try

            AddHandler AppDomain.CurrentDomain.UnhandledException, AddressOf WorkerAPCThreadHandler
        Catch ex As Exception
            ' Skip
        End Try
        Me.WindowState = FormWindowState.Minimized
    End Sub

    Public Sub WorkerAPCThreadHandler(ByVal sender As Object, ByVal args As UnhandledExceptionEventArgs)
        Try
            If Not (TypeOf args.ExceptionObject Is ThreadAbortException) Then
                Dim exc As Exception = TryCast(args.ExceptionObject, Exception)
                Environment.Exit(1)
            End If

        Catch ex As Exception
        End Try
    End Sub
    Dim ConnString As String
    Public Sub SetDatabase()
        Try
            Dim ServerName As String
            Dim Pwd As String
            Dim StrQuery As String
            Dim DV As New DataView
            Dim IsControlRoom As Boolean

            ' ServerName = System.Configuration.ConfigurationSettings.AppSettings.Item("CnnString").ToString
            'Pwd = System.Configuration.ConfigurationSettings.AppSettings.Item("Pwd").ToString

            'SQL Server
            'Dim ConnString As String = "Server = " & ServerName & _
            ConnString = "Server = " & ".\STA " & _
                                        ";Database = " & "STA_GLOBAL_MASTER" & _
                                        ";User Id = " & "sa" & _
                                         ";Password = " & " sa_123 " & ";Timeout=30"
            ' ";Password = " & Pwd & ";Timeout=30"

            ES.Common.MyDbConnString = ConnString

            SetProvider(eMyProvider.SQLServer, ES.Common.MyDbConnString)

            StrQuery = "Select * from STGlobal Where DefaultDB = 1"
            DV = GetMyDataView(StrQuery)

            If DV.Count > 0 Then
                'ES.Common.MyDbConnString = "Server = " & ServerName & _
                ES.Common.MyDbConnString = "Server =  " & ".\STA " & _
                                                        ";Database = " & DV(0)("DBName") & _
                                                        ";User Id = " & DV(0)("UserId") & _
                                                        ";Password = " & DV(0)("Password")
                ES.Common.DatabaseName = DV(0)("DBName")
                ES.Common.InstallationId = DV(0)("InstallationId")
                ES.Common.DataSync = DV(0)("DataSync")
                ES.Common.IsControlRoom = DV(0)("CR")

            End If


            SetProvider(eMyProvider.SQLServer, ES.Common.MyDbConnString)

            ES.Common.GlbDistList = ""
            Dim i As Integer

            For i = 0 To DV.Count - 1
                If ES.Common.GlbDistList = "" Then
                    ES.Common.GlbDistList = DV(i)("InstallationId")
                Else
                    ES.Common.GlbDistList = ES.Common.GlbDistList & "," & DV(i)("InstallationId")
                End If
            Next

        Catch ex As Exception
        End Try
    End Sub

    Private Sub SetParameterInfo()
        Try
            Dim dvAMDM As DataView = GetMyDataView("select * from ParameterInfo")
            If dvAMDM.Count > 0 Then
                For i As Integer = 0 To dvAMDM.Count - 1
                    Select Case dvAMDM(i)("ParameterName").ToString

                        Case "ScheduleAnnAfter"
                            ScheduleAnnAfter = Convert.ToInt32(dvAMDM(i)("Value"))
                        Case "ScheduleAnnTimeSpan"
                            ScheduleAnnTimeSpan = Convert.ToInt32(dvAMDM(i)("Value"))
                        Case "VTS"
                            VTS_Integration = dvAMDM(i)("Value").ToString
                        Case "VTSCITYBUS"
                            VTS_IntegrationCityBus = dvAMDM(i)("Value").ToString
                        Case "UPS"
                            UPS = dvAMDM(i)("Value").ToString
                        Case "UPSPORT"
                            UPSPORT = Convert.ToInt32(dvAMDM(i)("Value"))
                        Case "UPSBaudRate"
                            UPSBAUDRATE = dvAMDM(i)("Value").ToString
                        Case "MainStatusRate"
                            MainStatusRate = dvAMDM(i)("Value").ToString
                        Case "MainPresent"
                            MainPresent = dvAMDM(i)("Value").ToString
                        Case "MainAbsent"
                            MainAbsent = dvAMDM(i)("Value").ToString
                        Case "UPSTIMER"
                            UPSTIMER = dvAMDM(i)("Value").ToString
                        Case "UPSON"
                            UPSON = dvAMDM(i)("Value").ToString
                        Case "VTSAUTOBUSANN"
                            VTS_IntegrationAutoBusAnn = dvAMDM(i)("Value").ToString

                    End Select
                Next
            End If
        Catch ex As Exception

        End Try
    End Sub

    Public Function GetMyDataView(ByVal _SqlQuery As String) As DataView
        Dim dt As New DataTable
        Dim connection As New SqlConnection
        Dim command As New SqlCommand
        Try
            connection.ConnectionString = ES.Common.MyDbConnString
            'connection.ConnectionString = ConnString
            command.CommandText = _SqlQuery
            command.Connection = connection
            Dim dataAdapter As New SqlDataAdapter(command)
            connection.Open()
            command.CommandTimeout = 0
            command.ExecuteNonQuery()
            dataAdapter.Fill(dt)
            connection.Close()
            Return dt.DefaultView
        Catch ex As Exception
            Throw New Exception(ex.Message)
        Finally
            connection.Close()
        End Try
    End Function


    Private Sub VTSLogThreadCityBus()


        ' MsgBox("Call Function VTSLogThread")

        ClsUPSLog.TraceService(Now.ToString & ": in VTSLogThread for city bus")
        'ClsTraceService.TraceService("VTSETD" & Now.ToString, "*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-")

        Dim ScheduleAnnflag As Boolean = True
        Dim _SqlQuery As String = ""
        Dim StrVTS As String = ""
        Dim MailCntETA As Integer = 0
        Dim MailCntETD As Integer = 0
        Dim URL As String = ""

        While 1 = 1
            Dim _DvAUTOANN As New DataView()
            If _ETADFlag Then
                _ETADFlag = False
                Try
                    If My.Computer.Network.IsAvailable = False Then
                        Exit Sub
                    End If
                    Dim _DvAnnouncementType As New DataView()
                    Dim _DvETA As New DataView()
                    Dim _DvETD As New DataView()
                    Dim _DvCITY As New DataView()

                    If VTS_IntegrationCityBus.ToLower = "yes" Then


                        ClsUPSLog.TraceService(Now.ToString & ": in VTSLogThread for city bus flag is yes")
                        '----------------------this code added by hemalata as on 16-10-2019-----------

                        StrVTS = ""
                        StrVTS = " select top 1* from AdvertisementScheduler where  AnnouncementType='ETD'  "
                        _DvCITY = GetMyDataView(StrVTS)
                        If _DvCITY.Count = 0 Then
                            _VTS.VTSDeleteBusScheduleETd()
                            ClsUPSLog.TraceService(Now.ToString & ": in VTSLogThread for city bus Before call function")
                            _VTS.VTSIntergrationwithCityBusETD(Convert.ToInt32(System.Configuration.ConfigurationSettings.AppSettings("PriorityCITY")))
                            ClsUPSLog.TraceService(Now.ToString & ": in VTSLogThread for city bus Befoafter call function")
                            Try
                                URL = _VTS.getWebRefURLCITY()
                                If URL = "" Then
                                    Return
                                End If

                            Catch ex As Exception

                            End Try

                        End If

                        '-----------------------------
                        Try
                            If _VTS._VtsETALog = False Then
                                If ETAErrorCnt > 55 * 600 Then
                                    _VTS.SendMailForETAORETDService("Please check ETA /ETD web service has been stoped : " & Environment.NewLine & " -:" & URL & Environment.NewLine & " -: Date Time :" & Now.ToString)
                                    ETAErrorCnt = 0
                                End If
                            End If
                            ETAErrorCnt += 1
                        Catch ex As Exception

                        End Try
                    End If

                Catch ex As Exception
                    ClsUPSLog.TraceService(Now.ToString & ": in Error City Bus " & ex.Message)
                Finally
                    _ETADFlag = True
                End Try
            End If

            Thread.Sleep(60000 * Convert.ToInt32(System.Configuration.ConfigurationSettings.AppSettings("WaitTimeofETAETD")))
            _STAMainLogThreadCityBus.Sleep(60000 * Convert.ToInt32(System.Configuration.ConfigurationSettings.AppSettings("WaitTimeofETAETD")))
        End While
    End Sub

    Private Sub VTSLogThread()


        ' MsgBox("Call Function VTSLogThread")

        ClsUPSLog.TraceService(Now.ToString & ": in VTSLogThread")
        'ClsTraceService.TraceService("VTSETD" & Now.ToString, "*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-")

        Dim ScheduleAnnflag As Boolean = True
        Dim _SqlQuery As String = ""
        Dim StrVTS As String = ""
        Dim MailCntETA As Integer = 0
        Dim MailCntETD As Integer = 0
        Dim URL As String = ""

        While 1 = 1
            Dim _DvAUTOANN As New DataView()
            If _ETADFlag Then
                _ETADFlag = False
                Try
                    If My.Computer.Network.IsAvailable = False Then
                        Exit Sub
                    End If
                    Dim _DvAnnouncementType As New DataView()
                    Dim _DvETA As New DataView()
                    Dim _DvETD As New DataView()
                    Dim _DvCITY As New DataView()


                    ClsUPSLog.TraceService(Now.ToString & ": in VTSLogThread" & VTS_Integration.ToLower)
                    If VTS_Integration.ToLower = "yes" Then

                        URL = _VTS.getWebRefURLETA()

                        ClsUPSLog.TraceService(Now.ToString & ": in VTSLogThread" & URL)
                        If URL = "" Then
                            Return
                        End If
                        StrVTS = ""
                        StrVTS = " select top 1* from AdvertisementScheduler where AnnouncementType='ETA'"
                        _DvETA = GetMyDataView(StrVTS)
                        If _DvETA.Count = 0 Then
                            Try
                                ' MsgBox("Call proc")


                                ClsUPSLog.TraceService(Now.ToString & ": in VTSLogThread  before delete ")
                                _VTS.VTSDeleteBusScheduleETA()

                                ClsUPSLog.TraceService(Now.ToString & ": in VTSLogThread  after delete ")
                                _VTS.VTSIntergrationwithPunBusETA(Convert.ToInt32(System.Configuration.ConfigurationSettings.AppSettings("PriorityETA")))

                                ClsUPSLog.TraceService(Now.ToString & ": in VTSLogThread  VTS.VTSIntergrationwithPunBusETA ()")
                            Catch ex As Exception
                                ClsUPSLog.TraceService(Now.ToString & " Error when call ETA " & ex.Message)

                            End Try
                        End If

                        StrVTS = ""
                        StrVTS = " select top 1* from AdvertisementScheduler where  AnnouncementType='ETD'  "
                        _DvETD = GetMyDataView(StrVTS)
                        If _DvETD.Count = 0 Then

                            ClsUPSLog.TraceService(Now.ToString & ": in VTSLogThread  before delete etd ")
                            _VTS.VTSDeleteBusScheduleETd()

                            ClsUPSLog.TraceService(Now.ToString & ": in VTSLogThread  after delete etd ")
                            _VTS.VTSIntergrationwithPunBusETD(Convert.ToInt32(System.Configuration.ConfigurationSettings.AppSettings("PriorityETD")))


                            ClsUPSLog.TraceService(Now.ToString & ": in VTSLogThread  VTS.VTSIntergrationwithPunBusETD ")
                            Try
                                URL = _VTS.getWebRefURLETA()
                                If URL = "" Then
                                    Return
                                End If

                            Catch ex As Exception
                                ClsUPSLog.TraceService(Now.ToString & " Error when call ETD " & ex.Message)
                            End Try

                        End If

                        '----------------------this code added by hemalata as on 16-10-2019-----------

                        'StrVTS = ""
                        'StrVTS = " select top 1* from AdvertisementScheduler where  AnnouncementType='CITY'  "
                        '_DvCITY = GetMyDataView(StrVTS)
                        'If _DvCITY.Count = 0 Then
                        '    _VTS.VTSDeleteBusScheduleETd()
                        '    _VTS.VTSIntergrationwithCityBusETD(Convert.ToInt32(System.Configuration.ConfigurationSettings.AppSettings("PriorityCITY")))
                        '    Try
                        '        URL = _VTS.getWebRefURLCITY()
                        '        If URL = "" Then
                        '            Return
                        '        End If

                        '    Catch ex As Exception

                        '    End Try

                        'End If

                        '-----------------------------
                        Try
                            If _VTS._VtsETALog = False Then
                                If ETAErrorCnt > 55 * 600 Then
                                    _VTS.SendMailForETAORETDService("Please check ETA /ETD web service has been stoped : " & Environment.NewLine & " -:" & URL & Environment.NewLine & " -: Date Time :" & Now.ToString)
                                    ETAErrorCnt = 0
                                End If
                            End If
                            ETAErrorCnt += 1
                        Catch ex As Exception

                        End Try
                    End If

                Catch ex As Exception
                Finally
                    _ETADFlag = True
                End Try
            End If

            Thread.Sleep(60000 * Convert.ToInt32(System.Configuration.ConfigurationSettings.AppSettings("WaitTimeofETAETD")))
            _STAMainLogThread.Sleep(60000 * Convert.ToInt32(System.Configuration.ConfigurationSettings.AppSettings("WaitTimeofETAETD")))


        End While
    End Sub

    Private Sub BusThread()

        ClsUPSLog.TraceService(Now.ToString & ": in BusThread")
        While 1 = 1
            Try
                If My.Computer.Network.IsAvailable = False Then
                    Exit Sub
                End If
                If VTS_Integration.ToLower = "yes" Then
                    _VTS.DataDownload(Convert.ToInt32(System.Configuration.ConfigurationSettings.AppSettings("PriorityDownloadBus")))
                End If
            Catch ex As Exception

            End Try
            _BusReportingThread.Sleep(600000)
        End While
    End Sub
    'AutoBusAnnThread

    Private Sub AutoBusAnnThread()

        ClsUPSLog.TraceService(Now.ToString & ": in AutoBusAnnThread")
        While 1 = 1
            Try
                If My.Computer.Network.IsAvailable = False Then
                    Exit Sub
                End If
                If VTS_Integration.ToLower = "yes" Then
                    _VTS.AutoBusAnn(Convert.ToInt32(System.Configuration.ConfigurationSettings.AppSettings("PriorityAutoAnnBus")))
                End If
            Catch ex As Exception

            End Try
            _AutoBusAnnThread.Sleep(600000)
        End While
    End Sub



    Private Sub VrittiApcThread()
        While 1 = 1
            Try
                If My.Computer.Network.IsAvailable = False Then
                    Exit Sub
                Else
                    _VrittiApcDownloader.VrittiAPcDownloader(ES.Common.InstallationId)
                End If

            Catch ex As Exception
            Finally
                System.Threading.Thread.Sleep(600000)
                _VrittiApcThread.Sleep(600000)
            End Try

        End While
    End Sub

    Private Sub TimerScheduleAnn_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles TimerScheduleAnn.Tick
        Try
            TimerScheduleAnn.Stop()
            ClsUPSLog.TraceService(Now.ToString & ": in TimerScheduleAnn_Tick")
            Dim _DvANN As New DataView
            Dim _SqlQuery As String = ""
            'TimerScheduleAnn.Stop()

            _SqlQuery = ""
            _SqlQuery = " select top 1* from AdvertisementScheduler where AnnouncementType='ANN'"
            _DvANN = GetMyDataView(_SqlQuery)
            If _DvANN.Count = 0 Then
                _VTS.PlayAutoBusAnnouncement(Convert.ToInt32(System.Configuration.ConfigurationSettings.AppSettings("PriorityAutoBus")))
                TimerScheduleAnn.Interval = 1000 * ScheduleAnnTimeSpan * 60
            Else
                TimerScheduleAnn.Interval = 100
            End If
            TimerScheduleAnn.Start()

        Catch ex As Exception
        Finally

        End Try
    End Sub

    Private Sub Timer1_Tick(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles Timer1.Tick
        Try
            ClsUPSLog.TraceService(Now.ToString & ": in Timer1_Tick")
            Timer1.Stop()
            PlayFunction()
            BatteryVolt()
        Catch ex As Exception
        Finally
            Timer1.Enabled = True
            Timer1.Interval = UPSTIMER * 60000
            Timer1.Start()
        End Try

    End Sub

    Private Sub PlayFunction()
        Try
            Dim dvUPS As DataView = GetMyDataView("Select top 1 ScheduleTime from [dbo].[tblUPSSchedule] WITH(NOlOCK) where ScheduleTime>=(Select getDate()) order by ScheduleTime asc")
            If dvUPS.Count > 0 Then
                UPSON = Convert.ToDateTime(dvUPS(0)(0).ToString).ToString("HH:mm")
            End If
        Catch ex As Exception
            ClsUPSLog.TraceService(Now.ToString & ":PlayFunction, Error " & ex.Message)
        End Try
    End Sub

    Private Sub BatteryVolt()
        ClsUPSLog.TraceService(Now.ToString & ": in BatteryVolt")
        Dim a1 As Integer = 0
        Dim B As String = ""


        ClsUPSLog.TraceService("*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-")
        ClsUPSLog.TraceService(Now.ToString & " : BatteryVolt Communication Process Started")

        SerialPort = New SerialPort()
        SerialPort.PortName = SerialPorts(UPSPORT)

        ClsUPSLog.TraceService(Now.ToString & " : BatteryVolt Port Is  " & UPSPORT & "  Port Name :" & SerialPort.PortName.ToString)

        SerialPort.BaudRate = UPSBAUDRATE
        ClsUPSLog.TraceService(Now.ToString & " :BatteryVolt, BaudRate {0} " & SerialPort.BaudRate)
        SerialPort.Parity = Parity.None
        SerialPort.StopBits = StopBits.One
        SerialPort.DataBits = 8
        SerialPort.Handshake = Handshake.None
        SerialPort.ReadTimeout = 1000
        SerialPort.WriteTimeout = 1000
        Try
            System.Threading.Thread.Sleep(7000)
            ClsUPSLog.TraceService(Now.ToString & " : BatteryVolt Port Is Open ")
            If SerialPort.IsOpen Then
                Try
                    ClsUPSLog.TraceService(Now.ToString & " : BatteryVolt Serial Port is already Open ")
                    SerialPort.Close()
                Catch ex As Exception
                    ClsUPSLog.TraceService(Now.ToString & " : BatteryVolt Error Occured , when we are close the Serail port ")
                End Try
                SerialPort.Open()
            Else

                SerialPort.Open()
            End If

        Catch ex As Exception
            ClsUPSLog.TraceService(Now.ToString & " : BatteryVolt Error Occured , when we are Open the Serail port ")
            ClsUPSLog.TraceService(Now.ToString & " :BatteryVolt, Error occured in serial communication failed " & ex.Message)
        End Try
        Try

            Try
                SerialPort.WriteLine("*")
            Catch ex As Exception

            End Try
            Try
                System.Threading.Thread.Sleep(7000)
                a1 = Convert.ToInt32(SerialPort.ReadExisting())
                B = Math.Round((a1 * 0.0540909375), 2).ToString()
            Catch ex As Exception

            End Try

            Try
                SerialPort.Close()
                ClsUPSLog.TraceService(Now.ToString & ":BatteryVolt,Serial port close")
            Catch ex As Exception
                ClsUPSLog.TraceService(Now.ToString & ":BatteryVolt,Serial port close error " & ex.Message)
            End Try

            System.Threading.Thread.Sleep(7000)
            mainFlag = Mainstatus()


            Dim UPSOnTime As DateTime = System.DateTime.Parse(UPSON)

            ClsUPSLog.TraceService(Now.ToString & ":BatteryVolt,Main status " & mainFlag)
            If Now > UPSOnTime Then
                If mainFlag = "Absent" Then
                    If a1 <= MainAbsent And a1 <> 0 Then
                        If Mainsoncnt = 0 Then
                            Mainson()
                            Mainsoncnt = Mainsoncnt + 1
                            Mainsoffcnt = 0
                        End If
                    End If
                End If
            End If

            If mainFlag = "Present" Then
                If a1 >= MainPresent And a1 <> 0 Then
                    If Mainsoffcnt = 0 Then
                        Mainsoff()
                        Mainsoncnt = Mainsoncnt + 1
                    End If
                End If
            End If
            System.Threading.Thread.Sleep(7000)
            If a1 <> 0 Then
                INSERTDATA(B, mainFlag, Convert.ToString(a1))
            End If
            ClsUPSLog.TraceService(Now.ToString & ":BatteryVolt,Battery ADC Value : " & a1.ToString & "  ,  Battery Voltage Value : " & B.ToString)

            ClsUPSLog.TraceService(Now.ToString & " : BatteryVolt Function  ended ")
            ClsUPSLog.TraceService(Environment.NewLine)
        Catch ex As Exception
            ClsUPSLog.TraceService(Now.ToString & " : BatteryVolt Error Occured , when we are pass * cmd to Serail port ")
            ClsUPSLog.TraceService(Now.ToString & " : BatteryVolt  error " & ex.ToString)
        End Try
    End Sub

    Private Function Mainstatus() As String

        Dim Mstatus As String = ""
        Try
            SerialPort = New SerialPort()
            SerialPort.PortName = SerialPorts(UPSPORT)
            SerialPort.BaudRate = UPSBAUDRATE
            SerialPort.Parity = Parity.None
            SerialPort.StopBits = StopBits.One
            SerialPort.DataBits = 8
            SerialPort.Handshake = Handshake.None
            SerialPort.ReadTimeout = 1000
            SerialPort.WriteTimeout = 1000
            Try
                If SerialPort.IsOpen Then
                    SerialPort.Close()
                    SerialPort.Open()
                Else
                    SerialPort.Open()
                End If

            Catch ex As Exception
                ClsUPSLog.TraceService(Now.ToString & ":Mainstatus, Serial Port Communication ")
            End Try

            Mstatus = ""
            Try
                SerialPort.WriteLine("#")
                System.Threading.Thread.Sleep(1000)
            Catch ex As Exception

            End Try

            Dim a As Integer = 0
            a = Convert.ToInt32(SerialPort.ReadExisting())
            If a > MainStatusRate Then
                Mstatus = "Present"
            Else
                Mstatus = "Absent"
            End If
            SerialPort.Close()
        Catch ex As Exception
            ClsUPSLog.TraceService(Now.ToString & ":Mainstatus, Error  " & ex.Message)
        Finally
        End Try
        Mainstatus = Mstatus
    End Function

    Private Function Mainsoff()
        ClsUPSLog.TraceService(Now.ToString & " : Mainsoff Process Start Time ")
        SerialPort = New SerialPort()
        SerialPort.PortName = SerialPorts(UPSPORT)
        SerialPort.BaudRate = UPSBAUDRATE
        SerialPort.Parity = Parity.None
        SerialPort.StopBits = StopBits.One
        SerialPort.DataBits = 8
        SerialPort.Handshake = Handshake.None
        SerialPort.ReadTimeout = 1000
        SerialPort.WriteTimeout = 1000
        Try
            If SerialPort.IsOpen Then
                SerialPort.Close()
                SerialPort.Open()
            Else
                SerialPort.Open()
            End If
        Catch ex As Exception
            ClsUPSLog.TraceService(Now.ToString & " : Mainsoff serial port communication error " & ex.ToString)
        End Try
        Try
            SerialPort.WriteLine("&")
            System.Threading.Thread.Sleep(1000)
            SerialPort.Close()
        Catch ex As Exception
            ClsUPSLog.TraceService(Now.ToString & " : Mainsoff serial port communication error " & ex.ToString)
        End Try
        ClsUPSLog.TraceService(Now.ToString & " : Mainsoff End Time ")
    End Function

    Private Function Mainson()
        Try
            ClsUPSLog.TraceService(Now.ToString & " : Mainson Process Start Time")
            SerialPort = New SerialPort()
            SerialPort.PortName = SerialPorts(UPSPORT)
            SerialPort.BaudRate = UPSBAUDRATE
            SerialPort.Parity = Parity.None
            SerialPort.StopBits = StopBits.One
            SerialPort.DataBits = 8
            SerialPort.Handshake = Handshake.None
            SerialPort.ReadTimeout = 1000
            SerialPort.WriteTimeout = 1000
            Try
                If SerialPort.IsOpen Then
                    SerialPort.Close()
                    SerialPort.Open()
                Else
                    SerialPort.Open()
                End If
            Catch ex As Exception
                ClsUPSLog.TraceService(Now.ToString & " : MainsOn serial port communication error " & ex.ToString)
            End Try
            Try
                SerialPort.WriteLine("%")
                System.Threading.Thread.Sleep(1000)
                SerialPort.Close()
            Catch ex As Exception
                ClsUPSLog.TraceService(Now.ToString & " : MainsOn serial port communication error " & ex.ToString)
            End Try

        Catch ex As Exception
            ClsUPSLog.TraceService(Now.ToString & " : MainsOn serial port communication error " & ex.ToString)
        End Try
        ClsUPSLog.TraceService(Now.ToString & " : MainsON Process End Time ")
    End Function

    Public Sub INSERTDATA(ByVal B As String, ByVal MainStatus As String, ByVal a As String)
        Dim StrQuery As String = ""
        Try
            StrQuery = ""
            StrQuery = " insert into tblUps(MainStatus,BatteryVoltage,BatteryADCVal,MeasureDatetime) values('" + MainStatus + "','" + B + "','" + a + "',GetDate())"
            ExecuteMyQuery(StrQuery)
            ClsUPSLog.TraceService(Now.ToString & " : INSERTDATA Sql Query : " & StrQuery)
        Catch ex As Exception
            ClsUPSLog.TraceService(Now.ToString & " : INSERTDATA error " & ex.ToString & " Sql Query " & StrQuery)
        End Try

    End Sub


End Class

Public Class ClsUPSLog
    Shared isflagUPSLog As Boolean = False
    Public Shared Sub TraceService(ByVal content As String)
        Try
            If isflagUPSLog = True Then
                Exit Sub
            End If
            isflagUPSLog = True
            Dim DateValue As String = System.DateTime.Now.ToString("dd-MM-yyyy")
            Dim path As String = Application.StartupPath + "\Logs"
            If Not Directory.Exists(path) Then
                Directory.CreateDirectory(path)
            End If
            Dim txtfilepath As String = path + "\UPSLog_" & DateValue & ".txt"
            If Not System.IO.File.Exists(txtfilepath) Then
                System.IO.File.Create(txtfilepath).Dispose()
            End If
            Dim filepath As String = txtfilepath

            Dim fs As New FileStream(filepath, FileMode.OpenOrCreate, FileAccess.Write)
            Dim sw As New StreamWriter(fs)
            sw.BaseStream.Seek(0, SeekOrigin.[End])
            sw.WriteLine(content)
            sw.Flush()
            sw.Close()
        Catch ex As Exception
        Finally
            isflagUPSLog = False
        End Try
    End Sub

End Class

