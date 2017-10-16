' ----------------------------------------------------------------------------------------
' ----------------------------------------------------------------------------------------
'ENA v1
'13/04/16

'Features added (from PICTOR34.bas):
' - Reversed tube direction 
' - Added aspirate after 3rd wash to pick up right and left edges of the well 
' - Added error logging, default to log.bas
' - Created global variables for offsets for calibration
' - Created offset function, x_auf_well_fahrenOffset



' ----------------------------------------------------------------------------------------
' ----------------------------------------------------------------------------------------


' Grafische Oberflaeche Pictor Maximite MPP

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
Open "COM2:9600,2048" As #1          'oeffne den seriellen Port
C:
Clear
Cls
'Print #1, "/MI"                     'referiere MPP
sende_com("/MI")

offsetXGlobal = -120
offsetYGlobal = 100

offsetRight = 210 'Set offset right of well for aspiration '170
offSetLeft = -400 'Set offset left of well for aspiration  '-430

'Positions
Tip_Waste_X = 2000 
Tip_Waste_Y = 300
Tip_Waste_Z = 1
Tip_Waste_Z_abwerfen = 5800

Tip_1_X = 5500 - offsetXGlobal '5500
'Tip_1_Y = 0
Tip_1_Y = 700
Tip_1_Z = 1
Tip_1_Z_andocken = 7775
Tip_1_Z_abwerfen = 5600
Tip_9_X = 5500 - offsetXGlobal
'Tip_9_Y = 700
Tip_9_Y = 0 + offsetYGlobal
Tip_9_Z = 1
Tip_9_Z_andocken = 7775
Tip_9_Z_abwerfen = 5600 	'5400			'Tip 9-z throw
Tip_Abstand_X = 1172  					'Tip x distance

Tube_1_X = 13750 
Tube_1_Y = 1925 + offsetYGlobal
Tube_1_Z = 1
Tube_1_Z_aufnehmen = 6000				'Draw Tube Depth
Tube_1_Z_abgeben = 1500					'Dispense Tube Depth
Tube_Abstand_X = 1172
Tube_Abstand_Y = 875

Well_A1_X = 11500 -offsetXGlobal -75
Well_A1_Y = 5750 - offsetYGlobal -50
Well_A1_Z = 1
Well_A1_Z_aufnehmen = 5690  'pick up 5615
Well_A1_Z_abgeben = 5235    'dispense
Well_A1_Abstand_X = 1100
Well_A1_Abstand_Y = 775

Well_B1_X = 11500 -offsetXGlobal
Well_B1_Y = 7300 - offsetYGlobal
Well_B1_Z = 1
Well_B1_Z_aufnehmen = 5540
Well_B1_Z_abgeben = 5220
Well_B1_Abstand_X = 925

p_unten_Abwurf_Tip = 5820   '5820
p_oben_Abwurffedern_entlastet = 4380
p_Aufnahme_Pos = 4380
'bereich 3744 200 microliter

p_factor = 18
'18 pro microliter

p_aktuell = 0  'p position absolut fuer relativ volumen aufnehmen abgeben

e_c = 0

' ----------------------------------------------------------------------------------------


B:
Auswahl            'gehe zum Startbildschirm
' ----------------------------------------------------------------------------------------
Sub sende_com(s_c$)
Pause 10
Print #1, s_c$
Print #1, s_c$
End Sub
' ----------------------------------------------------------------------------------------
Sub empfange_com(Antwort$)
'Input #1, Antwort$
Antwort$ = ""
Starttime = Timer
Do
  Do
    SR$ = Input$(1, #1)
  Loop Until (SR$ <> Chr$(10))
If SR$ <> Chr$(13) Then
  Antwort$ = Antwort$ + SR$
EndIf
Endtime = Timer - Starttime
Loop Until ((SR$ = Chr$(13)) Or (Endtime > 2000))
If Endtime > 2000 Then Antwort$ = "TO"
End Sub

' ----------------------------------------------------------------------------------------
' Tube Placement Map: (CONJM Optional Test Basis)
' |Waste 1	DET  CONJG	WASH1  S3  S1|
' |Waste 2 	SUB  CONJM	WASH2  S4  S2|

' |11                           3   1|
' |12				    	    4   2|
' ----------------------------------------------------------------------------------------

Sub Ablauf_PICTOR

Open "log.bas" FOR OUTPUT AS #2

Touch remove all
'Schritt 1 add 50 serum from sample tubes 1-4 to slide wells A1,A2,B1,B2
Print "step 1 add 50 serum from sample tubes 1-4 to slide wells A1,A2,B1,B2"
Print #2, "step 1 add 50 serum from sample tubes 1-4 to slide wells A1,A2,B1,B2"

Font #2 '2
Tip_holen(1)                 'tip nummer
aus_tube_aufnehmen(1,50)       'tube nummer, volumen
in_well_abgeben (1,1,50)       'well zeile 1=A 2=B , well nummer, volumen
Tip_abwerfen(1)

Tip_holen(2)                 'tip nummer
aus_tube_aufnehmen(2,50)       'tube nummer, volumen
in_well_abgeben (2,1,50)       'well zeile 1=A 2=B, well nummer, volumen
Tip_abwerfen(2)

Tip_holen(3)                 'tip nummer
aus_tube_aufnehmen(3,50)       'tube nummer, volumen
in_well_abgeben (1,2,50)       'well zeile 1=A 2=B, well nummer, volumen
Tip_abwerfen(3)

Tip_holen(4)                 'tip nummer
aus_tube_aufnehmen(4,50)       'tube nummer, volumen
in_well_abgeben (2,2,50)       'well zeile 1=A 2=B, well nummer, volumen
Tip_abwerfen(4)


'Wait 30 min bei 37C
w_s$ = "wait 30 min 37C"
warte_min(30)                ;Wartezeit in min
Font #2 '2



Print "aspirate slide wells A1,A2,B1,B2"
Print #2,"aspirate slide wells A1,A2,B1,B2"
Font #2 '2

Tip_holen(1)                 'tip nummer
aus_well_aufnehmen (1,1,50,offsetRight)    'well zeile, well nummer, volumen
in_tube_abgeben(12,50)
Tip_in_waste_abwerfen

Tip_holen(2)                 'tip nummer
aus_well_aufnehmen (2,1,50,offsetRight)    'well zeile, well nummer, volumen
in_tube_abgeben(12,50)
Tip_in_waste_abwerfen

Tip_holen(3)                 'tip nummer
aus_well_aufnehmen (1,2,50,offsetRight)    'well zeile, well nummer, volumen
in_tube_abgeben(12,50)
Tip_in_waste_abwerfen

Tip_holen(4)                 'tip nummer
aus_well_aufnehmen (2,2,50,offsetRight)    'well zeile, well nummer, volumen
in_tube_abgeben(12,50)
Tip_in_waste_abwerfen


'wash step 1.1
Print "wash step 1.1"
Print #2, "wash step 1.1"

Font #2 '2
Tip_holen(5)                 'tip nummer
aus_tube_aufnehmen(5,200)      'tube nummer, volumen
in_well_abgeben (1,1,50)       'well zeile, well nummer, volumen
in_well_abgeben (2,1,50)       'well zeile, well nummer, volumen
in_well_abgeben (1,2,50)       'well zeile, well nummer, volumen
in_well_abgeben (2,2,50)       'well zeile, well nummer, volumen

aus_well_aufnehmen (2,2,50,offsetRight)    'well zeile, well nummer, volumen
in_tube_abgeben(11,50)
Tip_in_waste_abwerfen
Tip_holen(6)                 'tip nummer
aus_well_aufnehmen (1,2,50,offsetRight)    'well zeile, well nummer, volumen
in_tube_abgeben(11,50)
Tip_in_waste_abwerfen
Tip_holen(7)                 'tip nummer
aus_well_aufnehmen (2,1,50,offsetRight)    'well zeile, well nummer, volumen
in_tube_abgeben(11,50)
Tip_in_waste_abwerfen
Tip_holen(8)                 'tip nummer
aus_well_aufnehmen (1,1,50,offsetRight)    'well zeile, well nummer, volumen
in_tube_abgeben(11,50)
Tip_in_waste_abwerfen


'wash step 1.2
Print "wash step 1.2"
Print #2, "wash step 1.2"
Font #2 '2
wash12(5,11)


'wash step 1.3, wie wash step 1.2, also 1.2 wiederholen
Print "wash step 1.3"
Print #2, "wash step 1.3"
Font #2 '2
wash12(5,11)

washaspirate(11)
Close #2
Open "log.bas" FOR APPEND AS #2
' ----------------------------------------------------------------------------------------
'Step 2: add 50 conjugate G off tube 7

Print "step 2: add 50 conjugate G off tube 7"
Print #2, "step 2: add 50 conjugate G off tube 7"
Font #2 '2
Tip_holen(13)                'tip nummer
in_tube_mixen(7,200,2)        'Tube nummer, volumen, Anzahl mix
aus_tube_aufnehmen(7,200)      'tube nummer, volumen
in_well_abgeben (1,1,50)       'well zeile, well nummer, volumen
in_well_abgeben (2,1,50)       'well zeile, well nummer, volumen
in_well_abgeben (1,2,50)       'well zeile, well nummer, volumen
in_well_abgeben (2,2,50)       'well zeile, well nummer, volumen
Tip_abwerfen(13)

'Wait 30min 37C
w_s$ = "wait 30 min 37C"
warte_min(30)                ;Wartezeit in min
Font #2 '2

Print "aspirate slide wells B2,B1,A2,A1"
Print #2, "aspirate slide wells B2,B1,A2,A1"
Font #2 '2
aspirate(13,25)


'Wash steps 2.1   2.3

'wash step 2.1, wie wash step 1.2, also 1.2 wiederholen
Print "wash step 2.1"
Print #2, "wash step 2.1"
Font #2 '2
wash12(5,11)

'wash step 2.2, wie wash step 1.2, also 1.2 wiederholen
Print "wash step 2.2"
Print #2, "wash step 2.2"
Font #2 '2
wash12(5,12)

'wash step 2.3, wie wash step 1.2, also 1.2 wiederholen
Print "wash step 2.3"
Print #2, "wash step 2.3"
Font #2 '2
wash12(6,12)

washaspirate(12)
Close #2
Open "log.bas" FOR APPEND AS #2
' ----------------------------------------------------------------------------------------
'Step 3: add 50 detection reagent off tube 9

Print "step 3: add 50 detection reagent off tube 9"
Print #2, "step 3: add 50 detection reagent off tube 9"
Font #2 '2
Tip_holen(14)                'tip nummer
aus_tube_aufnehmen(9,200)      'tube nummer, volumen
in_well_abgeben (1,1,50)       'well zeile, well nummer, volumen
in_well_abgeben (2,1,50)       'well zeile, well nummer, volumen
in_well_abgeben (1,2,50)       'well zeile, well nummer, volumen
in_well_abgeben (2,2,50)       'well zeile, well nummer, volumen
Tip_abwerfen(14)              'tip nummer

'Wait 30min 37C
w_s$ = "wait 30 min 37C"
warte_min(30)                ;Wartezeit in min
Font #2 '2

Print "aspirate slide wells B2,B1,A2,A1"
Print #2, "aspirate slide wells B2,B1,A2,A1"
Font #2 '2
aspirate(14,25)


'Wash steps 3.1   3.3
'wash step 3.1, wie wash step 1.2, also 1.2 wiederholen
Print "wash step 3.1"
Print #2, "wash step 3.1"
Font #2 '2
wash12(6,12)

'wash step 3.2, wie wash step 1.2, also 1.2 wiederholen
Print "wash step 3.2"
Print #2, "wash step 3.2"
Font #2 '2
wash12(6,12)

'wash step 3.3, wie wash step 1.2, also 1.2 wiederholen
Print "wash step 3.3"
Print #2, "wash step 3.3"
Font #2 '2
'wash12last(6)
wash12(6,12)

washaspirate(12)
Close #2
Open "log.bas" FOR APPEND AS #2
' ----------------------------------------------------------------------------------------
'Step 4: add 50 substrate solution off tube 10
Print "step 4: add 50 substrate solution off tube 10"
Print #2, "step 4: add 50 substrate solution off tube 10"
Font #2 '2
Tip_holen(15)                'tip nummer
aus_tube_aufnehmen(10,200)      'tube nummer, volumen
in_well_abgeben (1,1,50)       'well zeile, well nummer, volumen
in_well_abgeben (2,1,50)       'well zeile, well nummer, volumen
in_well_abgeben (1,2,50)       'well zeile, well nummer, volumen
in_well_abgeben (2,2,50)       'well zeile, well nummer, volumen
Tip_abwerfen(15)

'cool down to room temperature
'Print "cool down to room temperature"
w_s$ = "cool down to room temperature"
warte_min(5)                 ;Wartezeit in min
Touch BEEP 1000
'Print "incubation"
w_s$ = "incubation"
warte_min(3)     ;Wartezeit in min
Font #2 '2

Print "aspirate slide wells B2,B1,A2,A1"
Print #2, "aspirate slide wells B2,B1,A2,A1"
Font #2 '2
aspirate(15,25)
Tip_in_waste_abwerfen
Close #2
Open "log.bas" FOR APPEND AS #2

' ----------------------------------------------------------------------------------------
'Step 5: stopping the reaction by wash buffer
Print "step 5: stopping the reaction by wash buffer"
Print #2, "step 5: stopping the reaction by wash buffer"
Font #2 '2
Tip_holen(16)                'tip nummer
aus_tube_aufnehmen(6,200)      'tube nummer, volumen
in_well_abgeben (1,1,50)       'well zeile, well nummer, volumen
in_well_abgeben (2,1,50)       'well zeile, well nummer, volumen
in_well_abgeben (1,2,50)       'well zeile, well nummer, volumen
in_well_abgeben (2,2,50)       'well zeile, well nummer, volumen
aus_well_aufnehmen (2,2,50,offsetRight)   'well zeile, well nummer, volumen
aus_well_aufnehmen (1,2,50,offsetRight)   'well zeile, well nummer, volumen
aus_well_aufnehmen (2,1,50,offsetRight)   'well zeile, well nummer, volumen
aus_well_aufnehmen (1,1,50,offsetRight)   'well zeile, well nummer, volumen
in_tube_abgeben(12,200)
Tip_in_waste_abwerfen

'Wait 30min drying at 37C
w_s$ = "wait 30min drying 37C"
warte_min(30)                ;Wartezeit in min
Font #2 '2

Print "END"
Print #2, "END"
Font #2 '2
'Beep ready for scanning
Touch BEEP 1000
Pause 500
Touch BEEP 1000
Pause 500
'Ende Ablauf
'y_auf_tip_fahren(1)
'x_auf_tip_fahren(1)
z_nach_oben_fahren
GoTo C

Close #2

End Sub


' ----------------------------------------------------------------------------------------
'Subs fuer Ablauf
Sub wash12(tube_w,waste_nummer)
Tip_holen(9)                 'tip nummer
aus_tube_aufnehmen(tube_w,200)      'tube nummer, volumen
in_well_abgeben (1,1,50)       'well zeile, well nummer, volumen
in_well_abgeben (2,1,50)       'well zeile, well nummer, volumen
in_well_abgeben (1,2,50)       'well zeile, well nummer, volumen
in_well_abgeben (2,2,50)       'well zeile, well nummer, volumen

aus_well_aufnehmen (2,2,50,offsetRight)    'well zeile, well nummer, volumen
in_tube_abgeben(waste_nummer,50)         'tube nummer, volumen
Tip_abwerfen(9)              'tip nummer

Tip_holen(10)                'tip nummer
aus_well_aufnehmen (1,2,50,offsetRight)    'well zeile, well nummer, volumen
in_tube_abgeben(waste_nummer,50)         'tube nummer, volumen
Tip_abwerfen(10)              'tip nummer

Tip_holen(11)                'tip nummer
aus_well_aufnehmen (2,1,50,offsetRight)    'well zeile, well nummer, volumen
in_tube_abgeben(waste_nummer,50)         'tube nummer, volumen
Tip_abwerfen(11)              'tip nummer

Tip_holen(12)                'tip nummer
aus_well_aufnehmen (1,1,50,offsetRight)    'well zeile, well nummer, volumen
in_tube_abgeben(waste_nummer,50)         'tube nummer, volumen
Tip_abwerfen(12)              'tip nummer
End Sub


' ----------------------------------------------------------------------------------------
Sub washaspirate(waste_nummer)
Tip_holen(9)                 'tip nummer
aus_well_aufnehmen (2,2,50,offsetRight)    'well zeile, well nummer, volumen
aus_well_aufnehmen (2,2,50,offsetLeft)
in_tube_abgeben(waste_nummer,50)         'tube nummer, volumen
Tip_abwerfen(9)              'tip nummer

Tip_holen(10)                'tip nummer
aus_well_aufnehmen (1,2,50,offsetRight)    'well zeile, well nummer, volumen
aus_well_aufnehmen (1,2,50,offsetLeft)
in_tube_abgeben(waste_nummer,50)         'tube nummer, volumen
Tip_abwerfen(10)              'tip nummer

Tip_holen(11)                'tip nummer
aus_well_aufnehmen (2,1,50,offsetRight)    'well zeile, well nummer, volumen
aus_well_aufnehmen (2,1,50,offsetLeft)
in_tube_abgeben(waste_nummer,50)         'tube nummer, volumen
Tip_abwerfen(11)              'tip nummer

Tip_holen(12)                'tip nummer
aus_well_aufnehmen (1,1,50,offsetRight)    'well zeile, well nummer, volumen
aus_well_aufnehmen (1,1,50,offSetLeft)
in_tube_abgeben(waste_nummer,50)         'tube nummer, volumen
Tip_abwerfen(12)              'tip nummer
End Sub



' ----------------------------------------------------------------------------------------
Sub wash12last(tube_w)
Tip_holen(9)                 'tip nummer
aus_tube_aufnehmen(tube_w,200)      'tube nummer, volumen
in_well_abgeben (1,1,50)       'well zeile, well nummer, volumen
in_well_abgeben (2,1,50)       'well zeile, well nummer, volumen
in_well_abgeben (1,2,50)       'well zeile, well nummer, volumen
in_well_abgeben (2,2,50)       'well zeile, well nummer, volumen

aus_well_aufnehmen (2,2,50,offsetRight)    'well zeile, well nummer, volumen
in_tube_abgeben(12,50)
Tip_in_waste_abwerfen

Tip_holen(10)                'tip nummer
aus_well_aufnehmen (1,2,50,offsetRight)    'well zeile, well nummer, volumen
in_tube_abgeben(12,50)
Tip_in_waste_abwerfen

Tip_holen(11)                'tip nummer
aus_well_aufnehmen (2,1,50,offsetRight)    'well zeile, well nummer, volumen
in_tube_abgeben(12,50)
Tip_in_waste_abwerfen

Tip_holen(12)                'tip nummer
aus_well_aufnehmen (1,1,50,offsetRight)    'well zeile, well nummer, volumen
in_tube_abgeben(12,50)
Tip_in_waste_abwerfen

End Sub

' ----------------------------------------------------------------------------------------
Sub aspirate(tip_nummer,volumen)
Tip_holen(tip_nummer)
aus_well_aufnehmen(1, 2, volumen,offsetRight)    'well zeile, well nummer, volumen
aus_well_aufnehmen(1, 2, volumen,offSetLeft)    'well zeile, well nummer, volumen
aus_well_aufnehmen(2, 2, volumen,offsetRight)    'well zeile, well nummer, volumen
aus_well_aufnehmen(2, 2, volumen,offSetLeft)    'well zeile, well nummer, volumen
aus_well_aufnehmen(2, 1, volumen,offsetRight)    'well zeile, well nummer, volumen
aus_well_aufnehmen(2, 1, volumen,offSetLeft)    'well zeile, well nummer, volumen
aus_well_aufnehmen(1, 1, volumen,offsetRight)    'well zeile, well nummer, volumen
aus_well_aufnehmen(1, 1, volumen,offsetLeft)    'well zeile, well nummer, volumen
in_tube_abgeben(11,200)
Tip_in_waste_abwerfen
End Sub


' ----------------------------------------------------------------------------------------
Sub Tip_holen(tip_nummer)
Print "Sub Tip_holen(tip_nummer)"
Print #2, "Sub Tip_holen(tip_nummer)"
'Print tip_nummer
z_nach_oben_fahren
y_auf_tip_fahren(tip_nummer)
x_auf_tip_fahren(tip_nummer)
'xy_auf_tip_fahren(tip_nummer)
z_nach_unten_tip_andocken
z_nach_oben_fahren
p_in_Aufnahme_Pos
End Sub


' ----------------------------------------------------------------------------------------
Sub Tip_abwerfen(tip_nummer)
Print "Sub Tip_abwerfen(tip_nummer)"
Print #2, "Sub Tip_abwerfen(tip_nummer)"
'Print tip_nummer
Print "Sub z_nach_oben_fahren"
Print #2, "Sub z_nach_oben_fahren"
z_nach_oben_fahren
Print "Sub x_auf_tip_fahren(tip_nummer)"
Print #2, "Sub x_auf_tip_fahren(tip_nummer)"
x_auf_tip_fahren(tip_nummer)
Print "Sub y_auf_tip_fahren(tip_nummer)"
Print #2,"Sub y_auf_tip_fahren(tip_nummer)"
y_auf_tip_fahren(tip_nummer)
Print "Sub z_nach_unten_auf_Abwurfposition_Tiphalter"
Print #2, "Sub z_nach_unten_auf_Abwurfposition_Tiphalter"
'xy_auf_tip_fahren(tip_nummer)
z_nach_unten_auf_Abwurfposition_Tiphalter
Print "Sub p_nach_unten_auf_Abwurf_Tip"
Print #2, "Sub p_nach_unten_auf_Abwurf_Tip"
p_nach_unten_auf_Abwurf_Tip
Print "Sub p_nach_oben_bis_Abwurffedern_wieder_entlastet"
Print #2, "Sub p_nach_oben_bis_Abwurffedern_wieder_entlastet"
p_nach_oben_bis_Abwurffedern_wieder_entlastet
Print "Sub z_nach_oben_fahren"
Print #2, "Sub z_nach_oben_fahren"
z_nach_oben_fahren
End Sub


' ----------------------------------------------------------------------------------------
Sub Tip_in_waste_abwerfen
'Print "Sub Tip_in_waste_abwerfen"
z_nach_oben_fahren
x_auf_waste_Position_fahren
y_auf_waste_Position_fahren
'xy_auf_waste_Position_fahren
z_nach_unten_auf_Abwurfposition_Waste
p_nach_unten_auf_Abwurf_Tip
p_nach_oben_bis_Abwurffedern_wieder_entlastet
z_nach_oben_fahren
End Sub


' ----------------------------------------------------------------------------------------
Sub aus_tube_aufnehmen(tube_nummer,volumen)
Print "Sub aus_tube_aufnehmen(tube_nummer,volumen)"
Print #2, "Sub aus_tube_aufnehmen(tube_nummer,volumen)"
'Print tube_nummer,volumen
z_nach_oben_fahren
y_auf_tube_fahren(tube_nummer)
x_auf_tube_fahren(tube_nummer)
'xy_auf_tube_fahren(tube_nummer)
z_nach_unten_auf_aus_tube_aufnehmen_Position
p_nach_oben_fahren_um(volumen)
z_nach_oben_fahren
End Sub


' ----------------------------------------------------------------------------------------
Sub in_tube_abgeben(tube_nummer,volumen)
Print "Sub in_tube_abgeben(tube_nummer,volumen)"
Print #2, "Sub in_tube_abgeben(tube_nummer,volumen)"
'Print tube_nummer,volumen
z_nach_oben_fahren
y_auf_tube_fahren(tube_nummer)
x_auf_tube_fahren(tube_nummer)
'xy_auf_tube_fahren(tube_nummer)
z_nach_unten_auf_in_tube_abgeben_Position
volumen = volumen + 5
p_nach_unten_fahren_um(volumen)
z_nach_oben_fahren
End Sub


' ----------------------------------------------------------------------------------------
Sub in_tube_mixen(tube_nummer,volumen,Anzahl_mix)
'Print "Sub in_tube_mixen(tube_nummer,volumen,Anzahl_mix)"
'Print tube_nummer,volumen,Anzahl_mix
z_nach_oben_fahren
y_auf_tube_fahren(tube_nummer)
x_auf_tube_fahren(tube_nummer)
'xy_auf_tube_fahren(tube_nummer)
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
Sub aus_well_aufnehmen(well_zeile,well_nummer,volumen,offset)
Print "Sub aus_well_aufnehmen(well_zeile,well_nummer,volumen)"
Print #2, "Sub aus_well_aufnehmen(well_zeile,well_nummer,volumen)"
'Print well_zeile,well_nummer,volumen
z_nach_oben_fahren
y_auf_well_fahren(well_zeile,well_nummer)
x_auf_well_fahrenOffset(well_zeile,well_nummer,offset)
z_nach_unten_auf_aus_well_aufnehmen_Position
p_nach_oben_fahren_um(volumen)
z_nach_oben_fahren
End Sub


' ----------------------------------------------------------------------------------------
Sub in_well_abgeben(well_zeile,well_nummer,volumen)
Print "Sub in_well_abgeben(well_zeile,well_nummer,volumen)"
Print #2, "Sub in_well_abgeben(well_zeile,well_nummer,volumen)"
'Print well_zeile,well_nummer,volumen
z_nach_oben_fahren
y_auf_well_fahren(well_zeile,well_nummer)
x_auf_well_fahren(well_zeile,well_nummer)
'xy_auf_well_fahren(well_zeile,well_nummer)
z_nach_unten_auf_in_well_abgeben_Position
p_nach_unten_fahren_um(volumen)
z_nach_oben_fahren
End Sub


' ----------------------------------------------------------------------------------------
Sub warte_min(minuten)
'Print "Sub warte_min(minuten)"
'Print "wait "; minuten; " minutes"
Cls
Touch remove all
Touch Size 100,40
Touch create 1, 360, 210, "Skip", Red, B
TouchVal(1) = 0

warte_timer_minuten = 0
warte_timer_sekunden = 0
warte_timer_m_sekunden = Timer
Do
  Do
    Do
    Loop Until (Timer >= warte_timer_m_sekunden + 1000)
    warte_timer_m_sekunden = Timer
    warte_timer_sekunden = warte_timer_sekunden + 1
    If TouchVal(1) = 1 Then
      warte_timer_sekunden = 61
      warte_timer_minuten = minuten + 1
    EndIf
    Print @(0,10) w_s$
    Print "          "
    Print "          "
    Print @(0,10) w_s$
    Print "min: "; minuten - warte_timer_minuten - 1
    Print "sec: "; 60 - warte_timer_sekunden
  Loop Until warte_timer_sekunden >= 60
  warte_timer_minuten = warte_timer_minuten + 1
  warte_timer_sekunden = 0
Loop Until (warte_timer_minuten >= minuten)
Touch remove all
Cls
End Sub

' ----------------------------------------------------------------------------------------
'weitere Sub - Subroutinen

Sub Warte_XA
Print "XA            ", Befehl$
Print #2, "XA            ", Befehl$
Pause 50
'Exit Sub
Do
'Input #1, Antwort$
empfange_com(Antwort$)
Print Antwort$
Print #2, Antwort$
If Antwort$ <> "XA1>" Then Print "Warte_XA Error = ", Antwort$
If Antwort$ <> "XA1>" Then Print #2, "Warte_XA Error = ",Antwort$
Close #2
Open "log.bas" FOR APPEND AS #2
'Loop Until Antwort$ = "XA1>"
Loop Until ((Antwort$ = "XA1>") Or (Antwort$ = "TO"))
Pause 10
End Sub

' ----------------------------------------------------------------------------------------
Sub Warte_XB
Print "XB            ", Befehl$
Print #2, "XB            ", Befehl$
Pause 50
'Exit Sub
Do
'Input #1, Antwort$
empfange_com(Antwort$)
Print Antwort$
Print #2, Antwort$
If Antwort$ <> "XB1>" Then Print "Warte_XB Error = ", Antwort$
If Antwort$ <> "XB1>" Then Print #2, "Warte_XB Error = ",Antwort$
Close #2
Open "log.bas" FOR APPEND AS #2
'Loop Until Antwort$ = "XB1>"
Loop Until ((Antwort$ = "XB1>") Or (Antwort$ = "TO"))
Pause 10
End Sub

' ----------------------------------------------------------------------------------------
Sub Warte_YA
Print "YA            ", Befehl$
Print #2, "YA            ", Befehl$
Pause 50
'Exit Sub
Do
'Input #1, Antwort$
empfange_com(Antwort$)
Print Antwort$
Print #2, Antwort$
If Antwort$ <> "YA1>" Then Print "Warte_YA Error = ", Antwort$
If Antwort$ <> "YA1>" Then Print #2, "Warte_YA Error = ",Antwort$
Close #2
Open "log.bas" FOR APPEND AS #2
'Loop Until Antwort$ = "YA1>"
Loop Until ((Antwort$ = "YA1>") Or (Antwort$ = "TO"))
Pause 10
End Sub

' ----------------------------------------------------------------------------------------
Sub Warte_YB
Print "YB            ", Befehl$
Print #2, "YB            ", Befehl$
Pause 50
'Exit Sub
Do
'Input #1, Antwort$
empfange_com(Antwort$)
Print Antwort$
Print #2, Antwort$
If Antwort$ <> "YB1>" Then Print "Warte_YB Error = ", Antwort$
If Antwort$ <> "YB1>" Then Print #2, "Warte_YB Error = ",Antwort$
Close #2
Open "log.bas" FOR APPEND AS #2
'Loop Until Antwort$ = "YB1>"
Loop Until ((Antwort$ = "YB1>") Or (Antwort$ = "TO"))
Pause 10
End Sub

' ----------------------------------------------------------------------------------------
Sub z_nach_oben_fahren
  Befehl$ = "/MMZ"+Format$(1)
'  Print #1, Befehl$
  sende_com(Befehl$)
  Warte_XA
End Sub

' ----------------------------------------------------------------------------------------
Sub z_nach_unten_tip_andocken
  Befehl$ = "/MMZ"+Format$(Tip_1_Z_andocken)
'  Print #1, Befehl$
  sende_com(Befehl$)
  Warte_XA
End Sub

' ----------------------------------------------------------------------------------------
Sub z_nach_unten_auf_Abwurfposition_Tiphalter
  Befehl$ = "/MMZ"+Format$(Tip_1_Z_abwerfen)
'  Print #1, Befehl$
  sende_com(Befehl$)
  Warte_XA
End Sub

' ----------------------------------------------------------------------------------------
Sub z_nach_unten_auf_Abwurfposition_Waste
  Befehl$ = "/MMZ"+Format$(Tip_Waste_Z_abwerfen)
'  Print #1, Befehl$
  sende_com(Befehl$)
  Warte_XA
End Sub

' ----------------------------------------------------------------------------------------
Sub z_nach_unten_auf_aus_tube_aufnehmen_Position
  Befehl$ = "/MMZ"+Format$(Tube_1_Z_aufnehmen)
'  Print #1, Befehl$
   sende_com(Befehl$)
  Warte_XA
End Sub

' ----------------------------------------------------------------------------------------
Sub z_nach_unten_auf_in_tube_abgeben_Position
  Befehl$ = "/MMZ"+Format$(Tube_1_Z_abgeben)
'  Print #1, Befehl$
  sende_com(Befehl$)
  Warte_XA
End Sub

' ----------------------------------------------------------------------------------------
Sub z_nach_unten_auf_aus_well_aufnehmen_Position
  Befehl$ = "/MMZ"+Format$(Well_A1_Z_aufnehmen)
'  Print #1, Befehl$
  sende_com(Befehl$)
  Warte_XA
End Sub

' ----------------------------------------------------------------------------------------
Sub z_nach_unten_auf_in_well_abgeben_Position
  Befehl$ = "/MMZ"+Format$(Well_A1_Z_abgeben)
'  Print #1, Befehl$
  sende_com(Befehl$)
  Warte_XA
End Sub

' ----------------------------------------------------------------------------------------
Sub x_auf_tip_fahren(tip_nummer)
If tip_nummer < 9 Then
t_tip_nummer = tip_nummer - 1
  Befehl$ = "/MMX"+Format$(Tip_1_X+(Tip_Abstand_X*t_tip_nummer))
'  Print #1, Befehl$
  sende_com(Befehl$)
  Warte_XA
ElseIf tip_nummer > 8 Then
t_tip_nummer = tip_nummer - 9
  Befehl$ = "/MMX"+Format$(Tip_9_X+(Tip_Abstand_X*t_tip_nummer))
'  Print #1, Befehl$
  sende_com(Befehl$)
  Warte_XA
EndIf
End Sub

' ----------------------------------------------------------------------------------------
Sub y_auf_tip_fahren(tip_nummer)
If tip_nummer < 9 Then
  Befehl$ = "/MMY"+Format$(Tip_1_Y)
'  Print #1, Befehl$
  sende_com(Befehl$)
  Warte_YA
ElseIf tip_nummer > 8 Then
  Befehl$ = "/MMY"+Format$(Tip_9_Y)
'  Print #1, Befehl$
  sende_com(Befehl$)
  Warte_YA
EndIf
End Sub

' ----------------------------------------------------------------------------------------
Sub x_auf_tube_fahren(tube_nummer)
t_tube_nummer = tube_nummer - 1
  Befehl$ = "/MMX"+Format$(Tube_1_X-(Tube_Abstand_X*t_tube_nummer))
'  Print #1, Befehl$
  sende_com(Befehl$)
  Warte_XA
End Sub

' ----------------------------------------------------------------------------------------
Sub y_auf_tube_fahren(tube_nummer)
If tube_nummer < 1 Then
tube_nummer = 1
ElseIf tube_nummer > 12 Then
tube_nummer = 12
EndIf
If tube_nummer = 1 Then
  Befehl$ = "/MMY"+Format$(Tube_1_Y)
ElseIf tube_nummer = 2 Then
  Befehl$ = "/MMY"+Format$(Tube_1_Y + Tube_Abstand_Y)
ElseIf tube_nummer = 3 Then
  Befehl$ = "/MMY"+Format$(Tube_1_Y)
ElseIf tube_nummer = 4 Then
  Befehl$ = "/MMY"+Format$(Tube_1_Y + Tube_Abstand_Y)
ElseIf tube_nummer = 5 Then
  Befehl$ = "/MMY"+Format$(Tube_1_Y)
ElseIf tube_nummer = 6 Then
  Befehl$ = "/MMY"+Format$(Tube_1_Y + Tube_Abstand_Y)
ElseIf tube_nummer = 7 Then
  Befehl$ = "/MMY"+Format$(Tube_1_Y)
ElseIf tube_nummer = 8 Then
  Befehl$ = "/MMY"+Format$(Tube_1_Y + Tube_Abstand_Y)
ElseIf tube_nummer = 9 Then
  Befehl$ = "/MMY"+Format$(Tube_1_Y)
ElseIf tube_nummer = 10 Then
  Befehl$ = "/MMY"+Format$(Tube_1_Y + Tube_Abstand_Y)
ElseIf tube_nummer = 11 Then
  Befehl$ = "/MMY"+Format$(Tube_1_Y)
ElseIf tube_nummer = 12 Then
  Befehl$ = "/MMY"+Format$(Tube_1_Y + Tube_Abstand_Y)
EndIf
'Print #1, Befehl$
sende_com(Befehl$)
Warte_YA
End Sub


' ----------------------------------------------------------------------------------------
Sub x_auf_well_fahren(well_zeile,well_nummer)
t_well_nummer = well_nummer - 1
  Befehl$ = "/MMX"+Format$(Well_A1_X-(Well_A1_Abstand_X*t_well_nummer))
'  Print #1, Befehl$
  sende_com(Befehl$)
  Warte_XA
End Sub

' ----------------------------------------------------------------------------------------
Sub x_auf_well_fahrenOffset(well_zeile,well_nummer,offset)   'Offset function for aspiration
t_well_nummer = well_nummer - 1
  Befehl$ = "/MMX"+Format$(Well_A1_X-(Well_A1_Abstand_X*t_well_nummer)+offset) '225
'  Print #1, Befehl$
  sende_com(Befehl$)
  Warte_XA
End Sub

' ----------------------------------------------------------------------------------------
Sub y_auf_well_fahren(well_zeile,well_nummer)
t_well_zeile = well_zeile - 1
  Befehl$ = "/MMY"+Format$(Well_A1_Y+(Well_A1_Abstand_Y*t_well_zeile))
'  Print #1, Befehl$
  sende_com(Befehl$)
  Warte_YA
End Sub


' ----------------------------------------------------------------------------------------
Sub x_auf_waste_Position_fahren
  Befehl$ = "/MMX"+Format$(Tip_Waste_X)
'  Print #1, Befehl$
  sende_com(Befehl$)
  Warte_XA
End Sub

' ----------------------------------------------------------------------------------------
Sub y_auf_waste_Position_fahren
  Befehl$ = "/MMY"+Format$(Tip_Waste_Y)
'  Print #1, Befehl$
  sende_com(Befehl$)
  Warte_YA
End Sub


' ----------------------------------------------------------------------------------------
Sub p_nach_unten_auf_Abwurf_Tip
'Print "Sub p_nach_unten_auf_Abwurf_Tip"
  Befehl$ = "/MMP"+Format$(p_unten_Abwurf_Tip)
'  Print #1, Befehl$
  sende_com(Befehl$)
  Warte_YB
p_aktuell = p_unten_Abwurf_Tip
End Sub

' ----------------------------------------------------------------------------------------
Sub p_nach_oben_bis_Abwurffedern_wieder_entlastet
'Print "Sub p_nach_oben_bis_Abwurffedern_wieder_entlastet"
  Befehl$ = "/MMP"+Format$(p_oben_Abwurffedern_entlastet)
'  Print #1, Befehl$
  sende_com(Befehl$)
  Warte_YB
p_aktuell = p_oben_Abwurffedern_entlastet
End Sub

' ----------------------------------------------------------------------------------------
Sub p_nach_oben_fahren_um(volumen)
'Print "Sub p_nach_oben_fahren_um(volumen)"
'Print volumen
p_r_pos = Cint(volumen * p_factor)
  Befehl$ = "/MMP"+Format$(p_aktuell - p_r_pos)
'  Print #1, Befehl$
  sende_com(Befehl$)
  Warte_YB
p_aktuell = p_aktuell - p_r_pos
Pause 750
End Sub

' ----------------------------------------------------------------------------------------
Sub p_nach_unten_fahren_um(volumen)
'Print "Sub p_nach_unten_fahren_um(volumen)"
'Print volumen
volumen = volumen + 0
p_r_pos = Cint(volumen * p_factor)
  Befehl$ = "/MMP"+Format$(p_aktuell + p_r_pos)
'  Print #1, Befehl$
  sende_com(Befehl$)
  Warte_YB
p_aktuell = p_aktuell + p_r_pos
End Sub

' ----------------------------------------------------------------------------------------
Sub p_in_Aufnahme_Pos
'Print "Sub p_in_Aufnahme_Pos"
  Befehl$ = "/MMP"+Format$(p_Aufnahme_Pos)
'  Print #1, Befehl$
  sende_com(Befehl$)
  Warte_YB
p_aktuell = p_Aufnahme_Pos
End Sub

'Ende weitere Sub - Subroutinen
'---------------------------------------------

'Subroutinen

'Subroutine fuer die Auswahl der Menge
Sub Auswahl

Font #2 '1
Print "Pictor ENA v1"
Font #2 '2
Print "Tube Placement Map"
Font #2

Touch size 90,30
Touch create 2,360,100,"Sample 1",Green,B,D


Touch Size 100,40
Touch create 1, 360, 210, "Start", Green, B
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
