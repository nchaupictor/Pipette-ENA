'ENA Full Slide v1
'06/09/2016

'Tip 1 Position -> Tube 1 Position (Sample 1)
'Tip Waste Position Not Changed
'

'Grafische Oberflaeche Pictor Maximite MPP
'V13 22.06.2016 ST1000
'Variablen
Menge = 1.6

'Touchobjekte
'1: Startbutton
'2: Stopbutton
'3: Menge vermindern
'4: Menge erhoehen
'10: Einstellungen

' ----------------------------------------------------------------------------------------
A:
'Hauptprogramm
Open "COM2:9600,1024" As #1          'oeffne den seriellen Port
'Open "COM2:9600,1024" As #1
C:
Clear
Cls
'Print #1, "/MI"                     'referiere MPP
sende_com("/MI")

Colour White 
offsetXGlobal = 70 '0
offsetYGlobal = -240 '-240
offsetZGlobal = 2400 '2650
offsetRight = 783 'Set offset right of well for aspiration 420
offsetLeft = -785 'Set offset left of well for aspiration -420
offsetDown = 475
offsetUp = -475

'Positions
Tip_Waste_X = 4000
Tip_Waste_Y = 600
Tip_Waste_Z = 2
Tip_Waste_Z_abwerfen = 9360
'--------------------------------------------------------------------
Tip_1_X = 1415 '1400 
Tip_1_Y = 9675
Tip_1_Z = 2
Tip_1_Z_andocken = 15870 - 450 '15240  15870 -600
Tip_1_Z_andocken_fast = 15780 - 600 - 1000
Tip_1_Z_abwerfen = 12560 '10560
Tip_Abstand_X = 2345
Tip_Abstand_Y = 1370
'--------------------------------------------------------------------
Tube_1_X = 18820 '1900 Position 1
Tube_1_Y = 6750 '3850
Tube_1_Z = 1
Tube_1_Z_aufnehmen = 11600 +1880 '11165 
Tube_1_Z_abgeben = 2360 +250  'Dispense Tube Depth
Tube_Abstand_X = 4230
Tube_Abstand_Y = 2850
'--------------------------------------------------------------------
WTube_1_X = 3750 '1900 Position 1
WTube_1_Y = 5250 '38500
WTube_1_Z = 1
WTube_1_Z_aufnehmen = 14000 +2150 '11165
WTube_1_Z_abgeben = 3000 +250  'Dispense Tube Depth
WTube_Abstand_X = 8750
WTube_Abstand_Y = 0
'--------------------------------------------------------------------
Well_X = 20110
Well_Y = 22600 '22425
Well_A1_Z = 2
Well_A1_Z_aufnehmen = 11330 +offsetZGlobal 'pick up 11145
Well_A1_Z_abgeben = 10350 +offsetZGlobal 'dispense 10350
Well_Abstand_X = 2350 
Well_Abstand_Y = 1565
'--------------------------------------------------------------------
Sample_X = 10930
Sample_Y = 1280
Sample_Z = 2
Sample_XDist = 2300
Sample_YDist = 1290 
Sample_ZDepth = 5250 +offsetZGlobal
'--------------------------------------------------------------------

p_unten_Abwurf_Tip = 11750
p_oben_Abwurffedern_entlastet = 8760
p_Aufnahme_Pos = 8760
'bereich 7488 200 microliter
p_factor = 36
'36 pro microliter

p_aktuell = 0  'p position absolut fuer relativ volumen aufnehmen abgeben

'--------------------------------------------------------------------
Dim tip_nummer As Integer = 1
Dim modNum as Integer = 8 'Default number of modules to process
CurrentWashVol = 0
'tipCount = 0
wasteTube = 1
washTube = 2
washVol = 200-20
conG = 1
det = 2
sub = 3
aspirateVol = 0
' ----------------------------------------------------------------------------------------

B:
Auswahl            'gehe zum Startbildschirm

Open "log.bas" For OUTPUT As #2
' ----------------------------------------------------------------------------------------
Sub sende_com(s_c$)
Pause 10
Print #1, s_c$
'an ST1000 nur einmal senden
'Print #1, s_c$
'
Print @(0,260) "-> ST1000 = ", s_c$
Open "log.bas" For APPEND As #2
Print #2, "-> ST1000 = ", s_c$
Close #2
End Sub

' ----------------------------------------------------------------------------------------
Sub empfange_com(Antwort$)
Antwort$ = ""
Starttime = Timer
Do
  Do
    SR$ = Input$(1, #1)
  Loop Until (SR$ <> Chr$(10))
  If SR$ <> Chr$(13) Then Antwort$ = Antwort$ + SR$
'  If Antwort$ <> "" Then Print Antwort$
  Endtime = Timer - Starttime
Loop Until ((SR$ = Chr$(13)) Or (Endtime > 5000))
If Endtime > 5000 Then Antwort$ = "TO"
Print @(0,260) "<- ST1000 = ", Antwort$
Open "log.bas" For APPEND As #2
Print #2, "<- ST1000 = ", Anwort$
Close #2
Puffer_leeren_Com2
End Sub


Sub Puffer_leeren_Com2
  Trash$ =""
  For x = 1 To 5
    Do
      SR$ = Input$(1, #1)
      Trash$ = Trash$ + SR$
    Loop Until (SR$ = "")
  Next x
End Sub


' ----------------------------------------------------------------------------------------
' Placement Mapping 

'Samples:
' 9 10 11 12 13 14 15 16
' 1 2  3  4  5  6  7  8

'Tubes
'              4 5 6 |                   R4 R5 R6
'   1     2          |    Waste    Wash    
'              1 2 3 |                   R1 R2 R3
'Tips
'1 2 3 4 5 6 7 8 9 10 11 12
'13 - - -             -  24
'-                        -
'-                        -
'85                      96

'Slide
' H   G   F   E   D   C   B   A
'15  13  11   9   7   5   3   1
'16  14  12  10   8   6   4   2

' ----------------------------------------------------------------------------------------

Sub Ablauf_PICTOR
'Open Com Port 2
Puffer_leeren_Com2

'Initialisation - Homing
Do
  Befehl$ = "/MSX5"
  sende_com(Befehl$)
  empfange_com(Antwort$)
'  print Antwort$
Loop Until (Antwort$ = "/M*")	
Pause 50

Do
  Befehl$ = "/MSY5"
  sende_com(Befehl$)
  empfange_com(Antwort$)
'  print Antwort$
Loop Until (Antwort$ = "/M*")	
Pause 50

Do
  Befehl$ = "/MSP4"
  sende_com(Befehl$)
  empfange_com(Antwort$)
'  print Antwort$
Loop Until (Antwort$ = "/M*")	
Pause 50

Puffer_leeren_Com2

'Open "log.bas" For OUTPUT As #2

Touch remove all
Cls

' ----------------------------------------------------------------------------------------
' ---------------------------------START--------------------------------------------------
Font #2
ModuleSelection 'Run module selection touch screen
Touch remove all
Cls

Colour Red
Print @(240,50) "Step 1.1 - Dispensing Sample"
Print @(240,60) "Step 1.2 - Aspirating Sample"
Print @(240,70) "Step 1.3 - Washing Sample"
Print @(240,80) "Step 2.1 - Dispensing Conjugate G"
Print @(240,90) "Step 2.2 - Washing Conjugate G"
Print @(240,100) "Step 3.1 - Dispensing Detection Agent"
Print @(240,110) "Step 3.2 - Washing Detection Agent"
Print @(240,120) "Step 4.1 - Dispensing Substrate"
Print @(240,130) "Step 4.2 - Washing Substrate"
Colour White
'Reset Timer
Timer = 0
Print @(0,0) "Time Elapsed (s): "
Print Timer/1000

PauseInit 'Initialise Pause Button
Touch Interrupt PauseTrigger 'Set up interrupt when button pressed

Dim tipCount as Integer = 0
Print @(0,30) "Step 1.1 - Dispensing Sample...                  "
sampleDispense(modNum)
tipCount = tipCount + modNum*2
Print @(0,0) "Time Elapsed (s): "
Print Timer/1000
'Memory

Print @(0,30) "Step 1.3 - Washing Sample...                      "
washSample(modNum,tipCount)
tipCount = tipCount + modNum*2
Print @(0,0) "Time Elapsed (s): "
Print Timer/1000

wash(modNum,tipCount,2)
tipCount = tipCount + 2
Print @(0,0) "Time Elapsed (s): "
Print Timer/1000
Colour Green
Print @(240,70) "Step 1.3 - Washing Sample"
Colour White

Print @(0,30) "Step 2.1 - Dispensing Conjugate G...          "                           
reagentDispense(modNum,tipCount,conG)
tipCount = tipCount + 1
Print @(0,0) "Time Elapsed (s): "
Print Timer/1000
Colour Green
Print @(240,80) "Step 2.1 - Dispensing Conjugate G"
Colour White

Print @(0,30) "Step 2.2 - Washing Conjugate G...                  "
wash(modNum,tipCount,3)
tipCount = tipCount + 3
Print @(0,0) "Time Elapsed (s): "
Print Timer/1000
Colour Green
Print @(240,90) "Step 2.2 - Washing Conjugate G"
Colour White

Print @(0,30) "Step 3.1 - Dispensing Detection Agent...     "
reagentDispense(modNum,tipCount,det)
tipCount = tipCount + 1
Print @(0,0) "Time Elapsed (s): "
Print Timer/1000
Colour Green
Print @(240,100) "Step 3.1 - Dispensing Detection Agent"
Colour White

Print @(0,30) "Step 3.2 - Washing Detection Agent...        "
wash(modNum,tipCount,3)
tipCount = tipCount + 3
Print @(0,0) "Time Elapsed (s): "
Print Timer/1000
Colour Green
Print @(240,110) "Step 3.2 - Washing Detection Agent"
Colour White


Print @(0,30) "Step 4.1 - Dispensing Substrate...             "
reagentDispense(modNum,tipCount,sub)
tipCount = tipCount + 1
Print @(0,0) "Time Elapsed (s): "
Print Timer/1000
Colour Green
Print @(240,120) "Step 4.1 - Dispensing Substrate"
Colour White

Print @(0,30) "Step 4.2 - Washing Substrate...           "
wash(modNum,tipCount,1)
Print @(0,0) "Time Elapsed (s): "
Print Timer/1000
Colour Green
Print @(240,130) "Step 4.2 - Washing Substrate"
Colour White
'Memory
warte_min(30)

Print @(0,0) "Time Elapsed (s): "
Print Timer/1000
Colour Green
Print @(240,140) "Assay Complete"

z_nach_oben_fahren

Font #2
Print @(0,40) "Tap to finish" 
Touch Wait 
GoTo C

'Close #2

End Sub



' ----------------------------------------------------------------------------------------
' ----------------------------------------------------------------------------------------
Sub Tip_holen(tip_nummer)
z_nach_oben_fahren
x_auf_tip_fahren(tip_nummer)
y_auf_tip_fahren(tip_nummer)

z_nach_unten_tip_andocken
z_nach_oben_fahren
p_in_Aufnahme_Pos
End Sub


' ----------------------------------------------------------------------------------------
Sub Tip_abwerfen(tip_nummer)
z_nach_oben_fahren
x_auf_tip_fahren(tip_nummer)
y_auf_tip_fahren(tip_nummer)

z_nach_unten_auf_Abwurfposition_Tiphalter
p_nach_unten_auf_Abwurf_Tip
p_nach_oben_bis_Abwurffedern_wieder_entlastet
z_nach_oben_fahren
End Sub


' ----------------------------------------------------------------------------------------
Sub Tip_in_waste_abwerfen
z_nach_oben_fahren
x_auf_waste_Position_fahren
y_auf_waste_Position_fahren
z_nach_unten_auf_Abwurfposition_Waste
p_nach_unten_auf_Abwurf_Tip
p_nach_oben_bis_Abwurffedern_wieder_entlastet
z_nach_oben_fahren
End Sub


' ----------------------------------------------------------------------------------------
Sub aus_tube_aufnehmen(tube_nummer,volumen)
z_nach_oben_fahren
y_auf_tube_fahren(tube_nummer)
x_auf_tube_fahren(tube_nummer)
z_nach_unten_auf_aus_tube_aufnehmen_Position
p_nach_oben_fahren_um(volumen)
z_nach_oben_fahren
End Sub


' ----------------------------------------------------------------------------------------
Sub aus_Wtube_aufnehmen(tube_nummer,volumen)
z_nach_oben_fahren
y_auf_Wtube_fahren(tube_nummer)
x_auf_Wtube_fahren(tube_nummer)
z_nach_unten_auf_aus_Wtube_aufnehmen_Position
p_nach_oben_fahren_um(volumen)
z_nach_oben_fahren
End Sub


' ----------------------------------------------------------------------------------------
Sub in_tube_abgeben(tube_nummer,volumen)
z_nach_oben_fahren
y_auf_tube_fahren(tube_nummer)
x_auf_tube_fahren(tube_nummer)
z_nach_unten_auf_in_tube_abgeben_Position
volumen = volumen + 5
p_nach_unten_fahren_um(volumen)
z_nach_oben_fahren
End Sub
' ----------------------------------------------------------------------------------------
Sub in_Wtube_abgeben(tube_nummer,volumen)
z_nach_oben_fahren
y_auf_wtube_fahren(tube_nummer)
x_auf_wtube_fahren(tube_nummer)
z_nach_unten_auf_in_Wtube_abgeben_Position
volumen = volumen + 5
p_nach_unten_fahren_um(volumen)
z_nach_oben_fahren
End Sub

' ----------------------------------------------------------------------------------------
Sub in_tube_mixen(tube_nummer,volumen,Anzahl_mix)
z_nach_oben_fahren
y_auf_tube_fahren(tube_nummer)
x_auf_tube_fahren(tube_nummer)
z_nach_unten_auf_aus_tube_aufnehmen_Position
' mixen_wiederholen
For counter_mixen = 1 To Anzahl_mix
'DO
    p_nach_oben_fahren_um(volumen)
    p_nach_unten_fahren_um(volumen)
'LOOP UNTIL(Anzahl_mix)
Next counter_mixen
'z_nach_oben_fahren
End Sub

' ----------------------------------------------------------------------------------------
Sub aus_sample_aufnehmen(tube_nummer,volumen)
z_nach_oben_fahren
y_auf_sample_fahren(tube_nummer)
x_auf_sample_fahren(tube_nummer)
z_nach_unten_auf_aus_sample_aufnehmen_Position
p_nach_oben_fahren_um(volumen)
z_nach_oben_fahren
End Sub

' ----------------------------------------------------------------------------------------
Sub in_sample_abgeben(tube_nummer,volumen)
z_nach_oben_fahren
y_auf_sample_fahren(tube_nummer)
x_auf_sample_fahren(tube_nummer)
z_nach_unten_auf_in_tube_abgeben_Position
volumen = volumen + 5
p_nach_unten_fahren_um(volumen)
z_nach_oben_fahren
End Sub

' ----------------------------------------------------------------------------------------
Sub aus_well_aufnehmen(module_num,well_nummer,volumen,offsetX,offsetY)
z_nach_oben_fahren
y_auf_well_fahrenOffset(well_nummer,offsetY)
x_auf_well_fahrenOffset(module_num,offsetX)
z_nach_unten_auf_aus_well_aufnehmen_Position
p_nach_oben_fahren_um(volumen)
z_nach_oben_fahren
End Sub


' ----------------------------------------------------------------------------------------
Sub in_well_abgeben(module_num,well_nummer,volumen)
z_nach_oben_fahren
y_auf_well_fahren(well_nummer)
x_auf_well_fahren(module_num)
z_nach_unten_auf_in_well_abgeben_Position
p_nach_unten_fahren_um(volumen)
z_nach_oben_fahren
End Sub


' ----------------------------------------------------------------------------------------
Sub warte_min(minuten)
'Print "Sub warte_min(minuten)"
'Print "wait "; minuten; " minutes"

'Cls
'Disable trigger
Touch Interrupt 0 
'Print @(0,30) "              "
'Touch remove all
Touch Size 100,40
Touch create 2, 180, 222, "Skip", Red, B
TouchVal(2) = 0

warte_timer_minuten = 0
warte_timer_sekunden = 0
warte_timer_m_sekunden = Timer
Do
  Do
    Do
    Loop Until (Timer >= warte_timer_m_sekunden + 1000)
    warte_timer_m_sekunden = Timer
    warte_timer_sekunden = warte_timer_sekunden + 1
    If TouchVal(2) = 1 Then
      warte_timer_sekunden = 61
      warte_timer_minuten = minuten + 1
    EndIf
    Print @(0,50) w_s$
    Print "          "
    Print "          "
    Print @(0,50) w_s$
    Print "min: "; minuten - warte_timer_minuten - 1
    Print "sec: "; 60 - warte_timer_sekunden
  Loop Until warte_timer_sekunden >= 60
  warte_timer_minuten = warte_timer_minuten + 1
  warte_timer_sekunden = 0
Loop Until (warte_timer_minuten >= minuten)
'Touch remove all
'Cls
'PauseInit
Touch Remove 2
Print @(0,50) "                           "
Print @(0,60) "                           "
Print @(0,70) "                           "
Touch Interrupt PauseTrigger
End Sub

' ----------------------------------------------------------------------------------------
'weitere Sub - Subroutinen

warte_MT:
Do
'ST1000 sende /MT
'  Pause 100
  Pause 50
  Befehl$ = "/MT"
  sende_com(Befehl$)
  empfange_com(Antwort$)
Loop Until (Antwort$ = "/M!")	
RETURN

Sub Warte_XA
GOSUB warte_MT
End Sub

' ----------------------------------------------------------------------------------------
Sub Warte_XB
GOSUB warte_MT
End Sub

' ----------------------------------------------------------------------------------------
Sub Warte_YA
GOSUB warte_MT
End Sub

' ----------------------------------------------------------------------------------------
Sub Warte_YB
GOSUB warte_MT
End Sub

' ----------------------------------------------------------------------------------------
Sub z_nach_oben_fahren
  Befehl$ = "/MMZ"+Format$(1)
  sende_com(Befehl$)
  Warte_XA
End Sub

' ----------------------------------------------------------------------------------------
Sub z_nach_unten_tip_andocken
  'Traverse 75% depth before slowing down
  Befehl$ = "/MMZ"+Format$(Tip_1_Z_andocken_fast)
  sende_com(Befehl$)
  Befehl$ = "/MSX9"
  sende_com(Befehl$)
  Befehl$ = "/MMZ"+Format$(Tip_1_Z_andocken)
  sende_com(Befehl$)
  Warte_XA
End Sub

' ----------------------------------------------------------------------------------------
Sub z_nach_unten_auf_Abwurfposition_Tiphalter
  Befehl$ = "/MMZ"+Format$(Tip_1_Z_abwerfen)
  sende_com(Befehl$)
  Warte_XA
End Sub

' ----------------------------------------------------------------------------------------
Sub z_nach_unten_auf_Abwurfposition_Waste
  Befehl$ = "/MMZ"+Format$(Tip_Waste_Z_abwerfen)
  sende_com(Befehl$)
  Warte_XA
End Sub

' ----------------------------------------------------------------------------------------
Sub z_nach_unten_auf_aus_tube_aufnehmen_Position
  Befehl$ = "/MMZ"+Format$(Tube_1_Z_aufnehmen)
   sende_com(Befehl$)
  Warte_XA
End Sub

' ----------------------------------------------------------------------------------------
Sub z_nach_unten_auf_aus_Wtube_aufnehmen_Position
  Befehl$ = "/MMZ"+Format$(WTube_1_Z_aufnehmen)
   sende_com(Befehl$)
  Warte_XA
End Sub
' ----------------------------------------------------------------------------------------
Sub z_nach_unten_auf_aus_sample_aufnehmen_Position
  Befehl$ = "/MMZ"+Format$(Sample_ZDepth)
   sende_com(Befehl$)
  Warte_XA
End Sub

' ----------------------------------------------------------------------------------------
Sub z_nach_unten_auf_in_tube_abgeben_Position
  Befehl$ = "/MMZ"+Format$(Tube_1_Z_abgeben)
  sende_com(Befehl$)
  Warte_XA
End Sub
' ----------------------------------------------------------------------------------------
Sub z_nach_unten_auf_in_Wtube_abgeben_Position
  Befehl$ = "/MMZ"+Format$(WTube_1_Z_abgeben)
  sende_com(Befehl$)
  Warte_XA
End Sub

' ----------------------------------------------------------------------------------------
Sub z_nach_unten_auf_aus_well_aufnehmen_Position
  Befehl$ = "/MMZ"+Format$(Well_A1_Z_aufnehmen)
  sende_com(Befehl$)
  Warte_XA
End Sub

' ----------------------------------------------------------------------------------------
Sub z_nach_unten_auf_in_well_abgeben_Position
  Befehl$ = "/MMZ"+Format$(Well_A1_Z_abgeben)
  sende_com(Befehl$)
  Warte_XA
End Sub

' ----------------------------------------------------------------------------------------
Sub x_auf_tip_fahren(tip_nummer)
'Traverse 75% distance at full speed while switching to low speed 
If tip_nummer < 13 Then
  Befehl$ = "/MMX"+Format$(Tip_1_X+(Tip_Abstand_X*(tip_nummer - 1)))
ElseIf tip_nummer > 12 And tip_nummer < 25 Then
  Befehl$ = "/MMX"+Format$(Tip_1_X+(Tip_Abstand_X*(tip_nummer - 13)))
ElseIf tip_nummer > 24 And tip_nummer < 37 Then  
  Befehl$ = "/MMX"+Format$(Tip_1_X+(Tip_Abstand_X*(tip_nummer - 25)))
ElseIf tip_nummer > 36 And tip_nummer < 49 Then 
  Befehl$ = "/MMX"+Format$(Tip_1_X+(Tip_Abstand_X*(tip_nummer - 37)))
ElseIf tip_nummer > 48 And tip_nummer < 61 Then
  Befehl$ = "/MMX"+Format$(Tip_1_X+(Tip_Abstand_X*(tip_nummer - 49)))
EndIf

sende_com(Befehl$)
Warte_XA
End Sub

' ----------------------------------------------------------------------------------------
Sub y_auf_tip_fahren(tip_nummer)
If tip_nummer < 13 Then
  Befehl$ = "/MMY"+Format$(Tip_1_Y)
ElseIf tip_nummer > 12 And tip_nummer < 25 Then
  Befehl$ = "/MMY"+Format$(Tip_1_Y + Tip_Abstand_Y*1)
ElseIf tip_nummer > 24 And tip_nummer < 37 Then  
  Befehl$ = "/MMY"+Format$(Tip_1_Y + Tip_Abstand_Y*2)
ElseIf tip_nummer > 36 And tip_nummer < 49 Then 
  Befehl$ = "/MMY"+Format$(Tip_1_Y + Tip_Abstand_Y*3)
ElseIf tip_nummer > 48 And tip_nummer < 61 Then
  Befehl$ = "/MMY"+Format$(Tip_1_Y + Tip_Abstand_Y*4)
EndIf

sende_com(Befehl$)
Warte_YA
End Sub

' ----------------------------------------------------------------------------------------
Sub x_auf_sample_fahren(i)
If i < 9 Then
  Befehl$ = "/MMX"+Format$(Sample_X+(Sample_XDist*(i-1)))
ElseIf i > 8 Then  
  Befehl$ = "/MMX"+Format$(Sample_X+(Sample_XDist*(i-9)))
EndIf
sende_com(Befehl$)
Warte_XA
End Sub

' ----------------------------------------------------------------------------------------
Sub y_auf_sample_fahren(i)
If i < 9 Then
  Befehl$ = "/MMY"+Format$(Sample_Y)
ElseIf i > 8 Then
  Befehl$ = "/MMY"+Format$(Sample_Y - Sample_YDist)
EndIf
sende_com(Befehl$)
Warte_YA
End Sub

' ----------------------------------------------------------------------------------------
Sub x_auf_tube_fahren(i)
If i < 4 Then
  Befehl$ = "/MMX"+Format$(Tube_1_X+(Tube_Abstand_X*(i-1)))
ElseIf i > 3 Then
  Befehl$ = "/MMX"+Format$(Tube_1_X+(Tube_Abstand_X*(i-8)))
EndIf
sende_com(Befehl$)
Warte_XA
End Sub

' ----------------------------------------------------------------------------------------
Sub y_auf_tube_fahren(i)
If i < 4 Then
  Befehl$ = "/MMY"+Format$(Tube_1_Y)
ElseIf i > 3 Then
  Befehl$ = "/MMY"+Format$(Tube_1_Y - Tube_Abstand_Y)
EndIf

sende_com(Befehl$)
Warte_YA
End Sub

' ----------------------------------------------------------------------------------------
Sub x_auf_wtube_fahren(i)
Befehl$ = "/MMX"+Format$(WTube_1_X+(WTube_Abstand_X*(i-1)))

sende_com(Befehl$)
Warte_XA
End Sub

' ----------------------------------------------------------------------------------------
Sub y_auf_wtube_fahren(i)

Befehl$ = "/MMY"+Format$(WTube_1_Y)
sende_com(Befehl$)
Warte_YA
End Sub

' ----------------------------------------------------------------------------------------
Sub x_auf_well_fahren(j)
  Befehl$ = "/MMX"+Format$(Well_X-(Well_Abstand_X*(j-1)))
  sende_com(Befehl$)
  Warte_XA
End Sub

' ----------------------------------------------------------------------------------------
Sub x_auf_well_fahrenOffset(j,offset)   'Offset function for aspiration
  Befehl$ = "/MMX"+Format$(Well_X-(Well_Abstand_X*(j-1))+offset) '225
  sende_com(Befehl$)
  Warte_XA
End Sub

' ----------------------------------------------------------------------------------------
Sub y_auf_well_fahren(i)
If i Mod 2 = 0 Then
  Befehl$ = "/MMY"+Format$(Well_Y + Well_Abstand_Y)
Else
  Befehl$ = "/MMY"+Format$(Well_Y)
EndIf
  sende_com(Befehl$)
  Warte_YA
End Sub

' ----------------------------------------------------------------------------------------
Sub y_auf_well_fahrenOffset(i,offset)
If i Mod 2 = 0 Then
  Befehl$ = "/MMY"+Format$(Well_Y+Well_Abstand_Y+offset)
Else
  Befehl$ = "/MMY"+Format$(Well_Y+offset)
EndIf
  sende_com(Befehl$)
  Warte_YA
End Sub
' ----------------------------------------------------------------------------------------
Sub x_auf_waste_Position_fahren
  Befehl$ = "/MMX"+Format$(Tip_Waste_X)
  sende_com(Befehl$)
  Warte_XA
End Sub

' ----------------------------------------------------------------------------------------
Sub y_auf_waste_Position_fahren
  Befehl$ = "/MMY"+Format$(Tip_Waste_Y)
  sende_com(Befehl$)
  Warte_YA
End Sub


' ----------------------------------------------------------------------------------------
Sub p_nach_unten_auf_Abwurf_Tip
  Befehl$ = "/MMP"+Format$(p_unten_Abwurf_Tip)
  sende_com(Befehl$)
  Warte_YB
p_aktuell = p_unten_Abwurf_Tip
End Sub

' ----------------------------------------------------------------------------------------
Sub p_nach_oben_bis_Abwurffedern_wieder_entlastet
  Befehl$ = "/MMP"+Format$(p_oben_Abwurffedern_entlastet)
  sende_com(Befehl$)
  Warte_YB
p_aktuell = p_oben_Abwurffedern_entlastet
End Sub

' ----------------------------------------------------------------------------------------
Sub p_nach_oben_fahren_um(volumen)
p_r_pos = Cint((volumen * p_factor) + 45)
  Befehl$ = "/MMP"+Format$(p_aktuell - p_r_pos)
  sende_com(Befehl$)
  Warte_YB
p_aktuell = p_aktuell - p_r_pos
Pause 750
End Sub

' ----------------------------------------------------------------------------------------
Sub p_nach_unten_fahren_um(volumen)
volumen = volumen + 0
p_r_pos = Cint(volumen * p_factor)
  Befehl$ = "/MMP"+Format$(p_aktuell + p_r_pos)
  sende_com(Befehl$)
  Warte_YB
p_aktuell = p_aktuell + p_r_pos
End Sub

' ----------------------------------------------------------------------------------------
Sub p_in_Aufnahme_Pos
  Befehl$ = "/MMP"+Format$(p_Aufnahme_Pos)
  sende_com(Befehl$)
  Warte_YB
p_aktuell = p_Aufnahme_Pos
End Sub

'Ende weitere Sub - Subroutinen
'---------------------------------------------

'Subroutinen

'Subroutine fuer die Auswahl der Menge
Sub Auswahl

Font #1 '1
Print@(0,0) "Pictor ENA Full Slide v1"
Print "Tube Placement Map:"


Touch Size 40,40
Touch create 2,150,60,"S1",Blue,B,D
TouchVal(2) = 0

Touch Size 40,40
Touch create 3,190,60,"S2",Blue,B,D
TouchVal(3) = 0

Touch Size 40,40
Touch create 4,230,60,"S3",Blue,B,D
TouchVal(4) = 0

Touch Size 40,40
Touch create 5,270,60,"S4",Blue,B,D
TouchVal(5) = 0

Touch Size 40,40
Touch create 6,310,60,"S5",Blue,B,D
TouchVal(6) = 0

Touch Size 40,40
Touch create 7,350,60,"S6",Blue,B,D
TouchVal(7) = 0

Touch Size 40,40
Touch create 8,390,60,"S7",Blue,B,D
TouchVal(8) = 0

Touch Size 40,40
Touch create 9,430,60,"S8",Blue,B,D
TouchVal(9) = 0

Touch Size 40,40
Touch create 10,150,10,"S9",Blue,B,D
TouchVal(10) = 0

Touch Size 40,40
Touch create 11,190,10,"S10",Blue,B,D
TouchVal(11) = 0

Touch Size 40,40
Touch create 12,230,10,"S11",Blue,B,D
TouchVal(12) = 0

Touch Size 40,40
Touch create 13,270,10,"S12",Blue,B,D
TouchVal(13) = 0

Touch Size 40,40
Touch create 14,310,10,"S13",Blue,B,D
TouchVal(14) = 0

Touch Size 40,40
Touch create 15,350,10,"S14",Blue,B,D
TouchVal(15) = 0

Touch Size 40,40
Touch create 16,390,10,"S15",Blue,B,D
TouchVal(16) = 0

Touch Size 40,40
Touch create 17,430,10,"S16",Blue,B,D
TouchVal(17) = 0

Touch Size 80,80
Touch create 18, 10,126,"Waste",Yellow,B,D
TouchVal(18) = 0

Touch Size 80,80
Touch create 19, 117,126,"Wash",Yellow,B,D
TouchVal(19) = 0

Touch Size 75,50
Touch create 20, 224,156, "ConjG",Yellow,B,D
TouchVal(20) = 0

Touch Size 75,50
Touch create 21, 310,156,"Det",Yellow,B,D
TouchVal(21) = 0

Touch Size 75,50
Touch create 22, 395,156,"Sub",Yellow,B,D
TouchVal(22) = 0

Touch Size 100,40
Touch create 1, 370, 220, "Start",Green, B
TouchVal(1) = 0

'Touch size 90,30
'Touch create 3, 90, 150, "-", Cyan, B
'TouchVal(3) = 0

'Touch size 90,30
'Touch create 4, 90, 50, "+", Cyan, B
'TouchVal(4) = 0

'Print @(10,10) "Abfuellmenge:"
'Print @(150,105) "ml"

'Touch Size 100,40
'Touch create 10, 360, 10, "Edit", Blue, B
'TouchVal(10) = 0

Do
'  On key Systemmenu                         'zugriff auf Systemmenu
'  Print @(100,105) Format$(Menge,"%.1f")    'zeige ausgewaehlte Menge (x.x)
' If TouchVal(3) Then
'    Menge = Menge - 0.4                     'Menge niedriger
'    If Menge < 0.4 Then Menge = 0.4
'  ElseIf TouchVal(4) Then
'    Menge = Menge + 0.4                     'Menge hoeher
'    If Menge > 3.2 Then Menge = 3.2
'  ElseIf TouchVal(1) = 1 Then
  If TouchVal(1) = 1 Then
     Ablauf_PICTOR
  ElseIf TouchVal(10) = 1 Then
    Systemmenu
  EndIf
Loop
End Sub

' ----------------------------------------------------------------------------------------
'Subroutine fuer die Befuellung
'Sub Befuellung(Menge1)
'End Sub

'Sub Abfuellen
'End Sub

Sub NotAus
Print "Break"
 Befehl$ = "Finish"
' Print #1, Befehl$
sende_com(Befehl$)
 Cls
 Auswahl                             'Abbrechen des Vorgangs
End Sub

'Subroutine fuer das Systemmenu
Sub Systemmenu
Cls
Touch remove all
Font #1 '2
Print @(0,0) "Systemmenu"

Touch Size 100,40
Touch create 5, 360, 210, "Beenden", Red, B
TouchVal(5) = 0

Touch Size 200,40
Touch create 6, 30, 50, "Druck", Blue, B
TouchVal(6) = 0

Touch Size 200,40
Touch create 7, 30, 100, "Demofahrt", Blue, B
TouchVal(7) = 0

Touch Size 200,40
Touch create 8, 250, 50, "Initialisieren", Blue, B
TouchVal(8) = 0

Touch Size 200,40
Touch create 9, 250, 100, "Positionen", Blue, B
TouchVal(9) = 0

Do
  If TouchVal(5) = 1 Then
    Cls
    Auswahl                                'kehre zur Mengenauswahl zurueck
  ElseIf TouchVal(7) = 1 Then
'  Print #1,"go"
sende_com("go")
  EndIf
Loop
End Sub

' ---------------------------------------------------------------------------------
Sub ModuleSelection

Font #1
Print "Select the number of modules to process:"

row1x = 75
row2x = 150

Touch Size 50,50
Touch create 1, 56,row1x,"1",Blue,B
TouchVal(1) = 0

Touch create 2, 162,row1x,"2",Blue,B
TouchVal(2) = 0

Touch create 3, 268,row1x,"3",Blue,B
TouchVal(3) = 0

Touch create 4, 374,row1x,"4",Blue,B
TouchVal(4) = 0

Touch create 5, 56,row2x,"5",Blue,B
TouchVal(5) = 0

Touch create 6, 162,row2x,"6",Blue,B
TouchVal(6) = 0

Touch create 7, 268,row2x,"7",Blue,B
TouchVal(7) = 0

Touch create 8, 374,row2x,"8",Blue,B
TouchVal(8) = 0

Touch Wait
Do
  If TouchVal(1) = 1 Then
     modNum = 1
     Exit Sub
  ElseIf TouchVal(2) = 1 Then
     modNum = 2
     Exit Sub
  ElseIf TouchVal(3) = 1 Then
     modNum = 3
     Exit Sub
  ElseIf TouchVal(4) = 1 Then
     modNum = 4
     Exit Sub
  ElseIf TouchVal(5) = 1 Then
     modNum = 5
     Exit Sub
  ElseIf TouchVal(6) = 1 Then
     modNum = 6
     Exit Sub
  ElseIf TouchVal(7) = 1 Then
     modNum = 7 
     Exit Sub 
  ElseIf TouchVal(8) = 1 Then
     modNum = 8
     Exit Sub
  EndIf
Loop


End Sub

' ---------------------------------------------------------------------------------
Sub PauseInit       ' Initialises Pause Button 
  Touch Size 190,40
  Touch create 1,290,222,"TAP TO PAUSE",Red,P
  TouchVal(1) = 0

End Sub
' ---------------------------------------------------------------------------------
Sub PauseTrigger    'Function triggers when the Pause Button is pressed, resume by pressing button again

  'Pause(3000000) 'Set max pause time to 30 minutes

  Do While TouchVal(1) = 1
    Touch Beep 500
  Touch Beep 500
  'TouchVal(1) = 0 'Reset Pause button
  Font #2
  Colour Red
  Print @(300,10) "Job Paused"
  Colour White
    Touch Wait
    'Font #2
    'Print @(300,10) "Job Paused"
    'Print TouchVal(1)
    If TouchVal(1) = 0 Then
      Print @(300,10) "                               "
      Font #1
      TouchVal(1) = 0
    EndIf
  Loop
  'Touch Wait
  'If TouchVal(1) = 1 Then
    'TouchVal(1) = 0
    'Print @(0,40) "                  "
    'Font #1
    'Exit Sub
  

  'EndIf

  'While TouchVal(1) = 1
    'Font #2
    'Print @(0,40) "Job Paused"
    'Print TouchVal(1)
  'Wend
  'Print TouchVal(1)
  'TouchVal(1) = 0
  'Print @(0,30) "                 "
  'Font #1
  'Exit Sub
Font #1
End Sub
' ---------------------------------------------------------------------------------
Sub sampleDispense(modNum)              
Dim j as Integer = 1
For i as Integer = 1 To (modNum*2)
  Tip_holen(i)
  'print "count: "
  'print count
  aus_sample_aufnehmen(i,50)
  in_well_abgeben(j,i,50)
  Tip_abwerfen(i)
  'print "count:"
  'print count
  if i Mod 2 = 0 Then
    j = j + 1    
  EndIf
  Print @(0,0) "Time Elapsed (s): "
  Print Timer/1000
Next
Print @(0,0) "Time Elapsed (s): "
Print Timer/1000
Colour Green
Print @(240,50) "Step 1.1 - Dispensing Sample"
Colour White
'30 minute incubation
Print @(0,40) "30 minute incubation"
warte_min(30)
Print @(0,40) "                          "
j = 0

Print @(0,0) "Time Elapsed (s): "
Print Timer/1000
Print @(0,30) "Step 1.2 - Aspirating Sample..."
'Aspirate sample from slide
For i as Integer = 1 To (modNum*2)
    Tip_holen(i)
    aus_well_aufnehmen(j,i,75,offsetRight,0)
    aus_well_aufnehmen(j,i,75,offsetLeft,0)
    in_Wtube_abgeben(wasteTube,155) 
    Tip_in_waste_abwerfen    


    if i Mod 2 = 0 Then
      j = j + 1    
    EndIf
    Print @(0,0) "Time Elapsed (s): "
    Print Timer/1000
Next
Print @(0,0) "Time Elapsed (s): "
Print Timer/1000
Colour Green
Print @(240,60) "Step 1.2 - Aspirating Sample"
Colour White

End Sub
' ---------------------------------------------------------------------------------
Sub washSample (modNum,tipCount) 
Dim k as Integer = 1
'Print "k: "
'Print k
if modNum Mod 2 > 0 Then 
  odd = 1
EndIf

Tip_holen(tipCount+1)
For i as Integer = 1 To modNum*2
  If (i-1) Mod 4 = 0 And (modNum*2 - i) > 1 Then          'Otherwise                      
      aus_Wtube_aufnehmen(washTube,washVol)       
      CurrentWashVol = CurrentWashVol + washVol 
  ElseIf i = modNum * 2 -1 And odd = 1 Then    'Last wash cycle 100uL if odd modules / 200 if even i = modNum * 2 -1 And odd = 1 And i > 1
      aus_Wtube_aufnehmen(washTube,washVol-100)
      CurrentWashVol = CurrentWashVol + washVol - 100
  EndIf

    in_well_abgeben(k,i,45)
    if i Mod 2 = 0 Then
      k = k + 1    
    EndIf
    'Print "k: "
    'Print k 
    Print @(0,0) "Time Elapsed (s): "
    Print Timer/1000
  Next
k = 0

For i as Integer = 1 To modNum*2
  Tip_holen(tipCount + i)
  aus_well_aufnehmen(k,i,100,offsetRight,0)
  in_Wtube_abgeben(wasteTube,100)
  Tip_in_waste_abwerfen
  if i Mod 2 = 0 Then
      k = k + 1    
  EndIf
  'Print "j: "
  'Print k 
  Print @(0,0) "Time Elapsed (s): "
  Print Timer/1000
Next

k = 0

End Sub

' ---------------------------------------------------------------------------------
Sub wash (modNum,tipCount,num)
Dim l as Integer = 1
if modNum Mod 2 > 0 Then
  odd = 1
EndIf

'Print "Wash 1.1 - 1.3"
For m as Integer = 1 To num 
  Tip_holen(tipCount + m)
  'Tip_holen(modNum * 2 + 1)
  For i as Integer = 1 To modNum * 2                       'First wash cycle
    If (i-1) Mod 4 = 0 And (modNum*2 - i) > 1 Then          'Otherwise                      
      aus_Wtube_aufnehmen(washTube,washVol)       
      CurrentWashVol = CurrentWashVol + washVol 
    ElseIf i = modNum * 2 -1 And odd = 1 Then    'Last wash cycle 100uL if odd modules / 200 if even i = modNum * 2 -1 And odd = 1 And i > 1
      aus_Wtube_aufnehmen(washTube,washVol-100)
      CurrentWashVol = CurrentWashVol + washVol - 100
    EndIf

    in_well_abgeben(l,i,45)
    'print "CurrentWashVol: "
    'print CurrentWashVol

    If i Mod 2 = 0 Then
      l = l + 1    
    EndIf
    Print @(0,0) "Time Elapsed (s): "
    Print Timer/1000

  Next

  l = 0
  Print @(0,0) "Time Elapsed (s): "
  Print Timer/1000

  For i as Integer = 1 To modNum * 2
    'If m = 3 Then
      'aus_well_aufnehmen(j,i,27,offsetRight,0)
      'aus_well_aufnehmen(j,i,27,0,offsetUp)
      'aus_well_aufnehmen(j,i,27,offsetLeft,0)
      'aus_well_aufnehmen(j,i,27,0,offsetDown)
      'aspirateVol = aspirateVol + 108
    'Else
      aus_well_aufnehmen(l,i,100,offsetRight,0)
      aspirateVol = aspirateVol + 100
    'EndIf
    
    If i Mod 2 = 0 And i > 1 Then          'Otherwise                      
      in_Wtube_abgeben(wasteTube,aspirateVol)
      aspirateVol = 0
    ElseIf i = modNum *2 Then
      in_Wtube_abgeben(wasteTube,aspirateVol) 
      aspirateVol = 0      
    EndIf

    if i Mod 2 = 0 Then
      l = l + 1    
    EndIf   
    Print @(0,0) "Time Elapsed (s): "
    Print Timer/1000
  Next

  Tip_in_waste_abwerfen
  l = 0
Next

End Sub
' ---------------------------------------------------------------------------------
Sub reagentDispense(modNum,tipCount,tubeNum)
Dim n as Integer = 0

if modNum Mod 2 > 0 Then
  odd = 1
EndIf

  Tip_holen(tipCount + 1)
  If tubeNum = conG Then
    Print @(0,40) "Mixing Conjugate"
    in_tube_mixen(conG,200,2)   
    Print @(0,40) "                                                   "     
  EndIf

  If tubeNum = sub Then
    Print @(0,40) "Mixing Substrate"
    in_tube_mixen(sub,200,2)   
    Print @(0,40) "                                                   "     
  EndIf

  For i as Integer = 1 To modNum * 2                       'First wash cycle
    If (i-1) Mod 4 = 0 And (modNum*2 - i) > 1 Then          'Otherwise                      
      aus_tube_aufnehmen(tubeNum,washVol)       
    ElseIf i = modNum * 2 -1 And odd = 1 Then    'Last wash cycle 100uL if odd modules / 200 if even i = modNum * 2 -1 And odd = 1 And i > 1
      aus_tube_aufnehmen(tubeNum,washVol-100)
    EndIf

    in_well_abgeben(n,i,45)

  if i Mod 2 = 0 Then
    n = n + 1    
  EndIf
  Print @(0,0) "Time Elapsed (s): "
  Print Timer/1000
  Next

'30 minute incubation
If tubeNum = 3 Then
  Print @(0,40) "5 minute incubation"
  warte_min(5)
  Touch Beep 500
  Print @(0,40) "                             "
Else
  Print @(0,40) "30 minute incubation"
  warte_min(30)
  Touch Beep 500
  Print @(0,40) "                            "
EndIf

n = 0

Print @(0,0) "Time Elapsed (s): "
Print Timer/1000

'Aspirate sample from slide
For i as Integer = 1 To (modNum*2)    
    aus_well_aufnehmen(n,i,75,offsetRight,0)
    aus_well_aufnehmen(n,i,75,offsetLeft,0)
    in_Wtube_abgeben(wasteTube,150) 
      
    if i Mod 2 = 0 Then
      n = n + 1    
    EndIf
    Print @(0,0) "Time Elapsed (s): "
    Print Timer/1000
Next
Tip_in_waste_abwerfen
Print @(0,0) "Time Elapsed (s): "
Print Timer/1000
n = 0 
End Sub
' ---------------------------------------------------------------------------------