
10  POKE 34,0: POKE 35,24: TEXT : HOME 
20  VTAB 5: PRINT "  WELCOME TO THE SEARCH FOR THE LOST    DUTCHMAN'S GOLD.  I'M THE GHOST OF BACK-PACK SAM, YER GUIDE ON THIS ADVENTURE.                              YA CAN USE 2 WORD COMMANDS TO FIND CLUES'N MOVE 'BOUT.  GET, GO, PUT, DROP,
30  PRINT "SCORE, INVENTORY 'N 'BOUT 100 MORE WORDSMAKE SENSE TA ME.  ASK FER HELP IF YA GET STUMPED.": PRINT "                            I HOPE YA DON'T END UP A GHOST, LIKE ME!
35  REM 
36  DIM N$(26,7),O$(32,3),R$(46):B = 0:IN = B:LN = 50:LM = LN:X1 = B:X2 = B:T$ = "":M1$ = T$:M2$ = T$:QM$ = T$:U = B:U1 = B:IM = 2:BD = 21:L = 1:C$ =  CHR$ (13): PRINT "MON C,I,O"
40  FOR L = B TO 26: FOR I = B TO 7: READ N$(L,I): NEXT I,L
50  FOR I = B TO 32: READ O$(I,0),O$(I,1),O$(I,2):O$(I,3) = "": NEXT 
60  FOR I = B TO 46: READ R$(I):NEXT 
70  PRINT "HIT 'SPACE' TO START.": GET QM$:L = 1: INPUT "RESTORE OLD GAME?";QM$: IF  LEFT$ (QM$ + " ",1) <  > "Y" THEN 100
80  INPUT "NAME?";QM$:QM$ = "LDG/" + QM$: ONERR  GOTO 4000
85  PRINT "VERIFY "QM$: PRINT "OPEN "QM$: PRINT "READ "QM$
90  INPUT L,CL,LN,IN,LM,U,U1,IM
91  FOR X = 0 TO 32: INPUT O$(X,1),O$(X,2),O$(X,3): NEXT 
95  PRINT "CLOSE"QM$: POKE 216,0
    
100  IF ((O$(12,2) =  STR$ (L) OR O$(12,3) = "1") AND CL > 0) OR L < 15 THEN 150
102  POKE 34,0: POKE 35,23: HOME : PRINT "IT'S TOO DARK TA SEE !!": IF L = 15 AND O$(6,3) <  > "1" AND O$(4,3) <  > "1" THEN  PRINT "YA HEAR A STRANGE NOISE !
110  POKE 35,24: GOTO 350
120 T$ = "OK": RETURN 
140 B = 1: PRINT : PRINT "YA SEE : ";: RETURN 
150  IF CL = 1 THEN LN = LN - 1:IF LN < 1 THEN CL = 0
160 I =  FRE (00):L1 = L
170 B = 0: POKE 35,BD: POKE 34,0: VTAB 1: HOME : POKE 35,24: PRINT "YER "N$(L,0)".": FOR J = 0 TO 32: IF  VAL (O$(J,2)) <  > L THEN 240
175  IF B = 0 THEN  GOSUB 140
180  IF O$(J,3) = "-7" OR O$(J,3) = "-2" THEN  PRINT "BURNT ";
190  IF J <  > 1 OR  VAL (O$(1,2)) <  > L THEN 200
193  IF O$(1,3) = "" THEN  PRINT "UNTIED ";: GOTO 200
196  IF O$(1,3) = "-9" THEN  PRINT "DEAD ";
200  IF O$(J,3) = "-3" THEN  PRINT "SMASHED ";
210  IF L <  > 26 OR J <  > 32 THEN 220
213  IF U1 <  > 0 THEN  PRINT "UN";
216  PRINT "LOCKED ";
220  IF CL = 1 AND J = 12 THEN  PRINT "LIT ";
230  PRINT O$(J,0)". ";
240  NEXT J
250  IF U1 = 1 AND L = 1 THEN  PRINT "OPEN TRAP DOOR. ";
260  IF N$(L,1) = "" THEN  PRINT : GOTO 330
265  IF B = 0 THEN  GOSUB 140
270  IF L <  > 16 THEN 280
273  IF U <  > 0 THEN  PRINT "UN";
276  PRINT "LOCKED ";: GOTO 290
280  IF L <  > 17 GOTO 290
283  IF U = 1 THEN  PRINT "UNLOCKED ";: GOTO 290
286  PRINT "BLOCKED ";
290  PRINT N$(L,1)".
310  IF L = 17 AND U = 0 THEN N$(L,2) = " " +  RIGHT$ (N$(L,2),3)
320  IF L = 16 AND U = 1 THEN N$(L,2) = "E" +  RIGHT$ (N$(L,2),3)
330  CALL  - 868: PRINT : IF N$(L,2) = "" THEN 350
335  PRINT "OBVIOUS EXITS : ";: FOR I = 1 TO 4:P$ =  MID$ (N$(L,2),I,1): IF P$ = "N" THEN  PRINT "NORTH ";
336  IF P$ = "S" THEN  PRINT "SOUTH ";
337  IF P$ = "E" THEN  PRINT "EAST ";
338  IF P$ = "W" THEN  PRINT "WEST ";
340  NEXT : CALL  - 868: PRINT 
350 B = 0: PRINT "=======================================
360 BD =  PEEK (37): POKE 34,BD: VTAB 24: IF L = 9 AND O$(22,2) = "11" THEN  PRINT "THAR'S NOISE UP AHEAD.": PRINT "SOUNDS LIKE INJUNS.
370  IF L = 9 AND O$(22,2) = "" THEN O$(22,2) = "11
390  INPUT " --NOW WHAT?";QM$
410  IF LN < 10 AND CL = 1 THEN PRINT "YER RUNNIN' LOW ON KEROSENE.
415  IF QM$ = "SAVE" OR QM$ = "SAVE GAME" THEN 2520
420  IF QM$ <  > "SCORE" THEN 450
423  IF L <  > 6 THEN T$ = "YA GET NOTHIN' FOR BEIN' HERE!":GOTO 1900
426 Z = 0: FOR I = 14 TO 17: IF O$(I,2) = "6" THEN Z = Z + 1
    
430  NEXT 
440  PRINT "YA GOT "Z" TREASURES, TOT'LIN "(Z / 4) * 100"%": IF Z = 4 THEN  PRINT "YA MADE IT!!": GOTO 2220
445 T$ = "YA MISSED SUM TREASURE!": GOTO 1900
450  IF CL = 0 AND L = 15 AND O$(6,3) <  > "1" THEN  PRINT "YE GAD!": PRINT "YA BEEN BIT BY A RATTLER.": PRINT "YER DEAD.": GOTO 2220
460  IF L = 17 AND U = 1 AND  RND (1) * 9 + 1 < 2 THEN  PRINT "&(%]#&@  CAVE IN !! /:+*@": PRINT "THE IRON DOOR'S BLOCKED,": PRINT "YER TRAPPED!!":U = 0
480  IF QM$ = "HELP" THEN X =  VAL (N$(L,3)): ON X GOSUB 2300,2310,2320,2330,2350,2370,2380,2390: GOTO 100
490 X1 =  LEN (QM$): IF X1 < 3 THEN T$ = "WHAT?": GOTO 1900
493 X3 = 0: FOR X2 = 3 TO X1:M1$ =  MID$ (QM$,X2,1): IF M1$ = " " THEN X3 = X2:X2 = X1
495  NEXT X2:X2 = X3: IF X3 = 0 THEN X2 = X1
500 P$ =  LEFT$ (QM$,3): IF X2 < > X1 AND X2 + 3 > X1 THEN 610
501 II =  - 1: FOR I = 0 TO 42: IF P$ = R$(I) THEN II = I
502  NEXT : IF II >  - 1 THEN I = II: GOTO 520
510  IF X1 = X2 THEN T$ = "WHAT?": GOTO 1900
511 T$ = "SORRY, BUT YA CAN'T " + LEFT$ (QM$,X2): IF X2 < X1 THEN T$ = T$ +  RIGHT$ (QM$,X1 - X2)
512 T$ = T$ + ".": GOTO 1900
520  IF I < 8 AND X1 = X2 THEN  ON I + 1 GOSUB 2010,2010,2010,2220,680,2020,1920,2000: GOTO 1900
540 JJ =  - 1:M1$ =  MID$ (QM$,X2 + 1,3): FOR J = 0 TO 32: IF M1$ =  LEFT$ (O$(J,0),3) THEN JJ = J
542  NEXT :J = JJ: IF JJ >  - 1 THEN 630
543  IF M1$ = "IRO" THEN 580
545  IF I = 36 THEN 940
550  IF N$(L,1) = "" THEN 560
552 Z = 1:A = Z:X1 =  LEN (N$(L,1)): FOR X2 = 5 TO X1: IF  MID$ (N$(L,1),X2,1) <  >  CHR$ (46) AND X2 <  > X1 THEN  NEXT : GOTO 560
553 M2$ =  MID$ (N$(L,1),Z,3): IF M2$ = M1$ THEN 556
554 Z = X2 + 2:A = A + 1: NEXT : GOTO 560
556 X2 = X1: NEXT :B = A: IF I = 10 THEN  PRINT "YA SEE NOTHIN' SPECIAL.": GOTO 160
558  GOTO 630
560  FOR X = 43 TO 46: IF M1$ = R$(X) THEN X = 46: NEXT : GOTO 650
565  NEXT 
570  IF O$(4,3) = "1" OR O$(6,3)= "1" THEN  IF I = 23 AND M1$ = "SEL" THEN  PRINT "OK":PRINT "YER DEAD.": GOTO 2220
580  IF I = 32 THEN  GOSUB 780: GOTO 1900
590  IF I = 33 THEN  GOSUB 860: GOTO 1900
600 T$ = "I CAN'T TELL WACHA WANT.": GOTO 1900
610 T$ = "I MUST BE DUMB, YA DON'T MAKE SENSE.": GOTO 1900
630  IF I < 9 THEN 650
633  IF J < 33 AND J >  - 1 THEN B =  VAL (O$(J,2)): IF B <  > L AND B > 0 THEN 1890
636  IF J > 32 THEN J = 29
640  ON I - 8 GOSUB 1250,1390,1390,1390,1250,1700,1700,1700,1700,1820,1820,1250,700,700,1080,1060,1060,920,1150,1190,1210,730,730,780,860,1250,900,940,1700,1690,1320,1340,1820,2500
650  IF I < 9 THEN  ON I + 1 GOSUB 970,970,970,2220,680,970,1920,2000,1320
660  GOTO 1900
680  IF O$(3,3) <  > "1" THEN 1890
683  PRINT "OK": FOR I = 13 TO 17: IF  VAL (O$(I,2)) =  - L THEN T$ = "EUREKA! WE STRUCK GOLD?!??":O$(I,2) =  STR$ (L): RETURN 
685  NEXT 
686 T$ = "DAG NAB IT!  THAR'S NOTHIN' HERE!": RETURN 
700  IF J <  > 23 THEN 1060
701  IF L = 14 THEN O$(23,2) = "15": GOTO 120
702  IF L = 15 THEN O$(23,2) = "16": GOTO 120
703  IF L = 16 AND U = 1 THEN O$(23,2) = "17": GOTO 120
704  IF L = 17 THEN O$(23,2) = "19": GOTO 120
705  IF L = 19 THEN O$(23,2) = "23":O$(23,3) = "-3
710  GOTO 120
730  IF J = 3 OR J = 4 OR J = 6 OR J = 8 OR J = 9 OR J = 11 OR J = 12 OR J = 13 OR J = 19 OR J = 27 OR J = 28 THEN 740
735  GOTO 1050
740  IF O$(J,3) = "1" THEN IN = IN - 1
750 O$(J,3) = "-3":O$(J,2) =  STR$ (L): GOTO 120
770 T$ = "DON'T HAVTA.": RETURN 
780  IF L <  > 16 THEN 800
783  IF U <  > 0 THEN 770
786  IF O$(10,3) = "1" THEN U = 1: GOTO 120
788  GOTO 1890
800  IF L <  > 26 THEN 820
803  IF U1 <  > 0 THEN 770
806  IF O$(10,3) = "1" THEN U1 = 1: RETURN 
808  GOTO 1890
820  IF O$(1,3) <  > "1" THEN 840
825  IF O$(0,3) = "2" THEN O$(0,3) = "":O$(0,2) =  STR$ (L): GOTO 120
830  GOTO 1050
840  IF CL = 1 AND O$(12,3) = "1" THEN CL = 0: GOTO 120
860  IF (L = 16 OR L = 17) AND U = 1 THEN U = 0: GOTO 120
870  IF L = 26 AND U1 = 1 THEN U1 = 0: GOTO 120
880  GOTO 1050
900 T$ = "YA HEAR NOTHIN' SPECIAL.": RETURN 
920  IF L <  > 11 OR O$(9,3) <  > "1" THEN 1320
930  PRINT "THEY TOOK THE FIRE WATER 'N RAN.":IN = IN - 1:O$(9,2) = "":O$(9,3) = "":O$(22,2) = "": RETURN 
940  IF M1$ = "YOH" THEN T$ = "TURKEY!": PRINT "SORRY, YOUR IN THE WRONG ADVENTURE.": GOTO 1900
950  GOSUB 120:T$ = T$ +  CHR$ (13) +  RIGHT$ (QM$,X1 - X2) + ".": GOTO 1900
970  IF B = 0 THEN 980
973 L =  VAL (N$(L,B + 3)): IF O$(1,3) = "1" AND (L = 1 OR L = 6 OR L = 15) THEN T$ = "HE WON'T GO.":L = L1: RETURN 
    
976  IF L = 17 AND U = 0 THEN L = L1: GOTO 1890
978  IF L1 = 26 AND L = 1 AND U1 = 0 THEN L = L1: GOTO 1890
979  GOTO 1060
980  IF U1 = 1 AND L = 1 THEN L = 26: GOTO 120
990  IF U1 = 1 AND M1$ = "TRA" AND L = 26 THEN L = 1: RETURN 
1000  FOR X1 = 1 TO 4:M2$ =  MID$ (N$(L,2),X1,1): IF M2$ <  >  LEFT$ (M1$,1) THEN  NEXT X1: GOTO 1010
1003  FOR X2 = 43 TO 46: IF M1$ <  > R$(X2) THEN  NEXT X2: GOTO 1010
1006 L =  VAL (N$(L,X1 + 3)): IF L <  > 0 THEN 1060
1010  IF L = 23 THEN T$ = "IT'S TOO SLIPPERY.": RETURN 
1020  IF L = 25 THEN L = 26: RETURN 
    
1030  IF L = 26 THEN L = 25: RETURN 
    
1040  IF L = 19 THEN L = 23: PRINT "NIGH BROKE M'NECK!!": RETURN 
    
1050 T$ = "SORRY, BUTCHA CAN'T DO THAT.": RETURN 
1060 B = 0: GOTO 120
1080  IF O$(4,3) <  > "1" AND O$(6,3) <  > "1" THEN 1320
1083  IF J <  > 1 THEN 1110
1086 Z = 0: IF O$(J,3) = "1" THEN Z = 99
1090 O$(J,3) = "-9":O$(1,2) =  STR$ (L)
1100  IF Z = 99 THEN IN = IN - 1
    
1110  IF J = 22 THEN  PRINT "YA GOT ONE!": PRINT "BUT THE REST GOT YOU!": GOTO 2220
1120  IF J = 9 THEN T$ = "YA HITONE!": RETURN 
1130  GOTO 120
1150  IF O$(18,3) <  > "1" THEN 1890
1152  IF J < 0 THEN 511
1153  IF J = 4 OR (J > 5 AND J < 11) OR (J > 14 AND J < 18) OR J = 22 OR J = 29 OR J > 30 THEN 1050
1156  IF O$(J,3) = "1" THEN IN = IN - 1
1157 O$(J,3) = "-7":O$(J,2) =  STR$ (L)
1158  IF J > 0 THEN 1060
1160  IF O$(0,3) <  > "-7" THEN 1060
1163 O$(0,2) =  STR$ (L): FOR J = 5 TO 21: IF O$(J,2) = "-1" THEN O$(J,3) = "-7"
1170  NEXT : GOTO 1060
1190  IF J <  > 12 THEN 1150
1193  IF O$(J,3) <  > "1" OR O$(18,3) <  > "1" THEN 1150
1195 CL = 1: GOTO 120
1210  IF J = 13 THEN  PRINT "YEEECH!!": PRINT "IT'S KEROSENE!": PRINT "YA JUST POISONED YERSELF.": GOTO 2220
1220  IF J <  > 9 THEN 1050
1230  PRINT "WHEEEE!!": PRINT "YA GOT PLASTERED AND LOST A DAY.": IF CL = 1 THEN LN = LN - 10
1235  RETURN 
1250  IF J > 32 OR J < 0 THEN 1260
1253 B =  VAL (O$(J,2)): IF O$(J,3) = "2" OR B = L OR B =  - 1 THEN 1260
1255  IF J < 22 THEN 1890
1257 T$ = "IT AIN'T HERE.": RETURN 
    
1260  IF J > 21 OR J = 1 OR J = - 1 THEN T$ = "WHO THE HECK YA THINK YA IS?" + C$ + "PAUL BUNYON??!  YA AIN'T STRONG 'NUF.": RETURN 
1270  IF O$(J,3) = "-7" THEN T$ = "IT'S BURNT UP 'N RUINED.": RETURN 
1280  IF IN > 4 THEN T$ = "YA CAN'T! YER HANDS 'R FULL.": RETURN 
    
1283  IF O$(J,3) = "1" THEN  PRINT "YA ALREADY GOT IT!": RETURN 
    
1286  IF  VAL (O$(J,1)) = 0 AND I <  > 16 THEN 1290
1287 IN = IN + 1: IF O$(J,2) = "-1" THEN IM = IM - 1:O$(J,2) = "":O$(J,3) = "1": GOTO 1290
1288 O$(J,3) = "1":O$(J,2) = ""
1290  IF J <  > 9 AND J <  > 11 AND J <  > 2 THEN 120
1293  IF J = 9 THEN  IF O$(8,2) = "-6" THEN O$(8,2) = "6
1295  IF J = 11 THEN  IF O$(12,2) = "-8" THEN O$(12,2) = "8
1297  IF J = 2 THEN  IF O$(18,2) = "-14" THEN O$(18,2) = "14"
1300  RETURN 
1320  IF O$(J,3) <  > "1" THEN T$ = "YA DON'T HAVE IT!": RETURN 
    
1325 T$ = "OK":IN = IN - 1:O$(J,2) =  STR$ (L):O$(J,3) = "": RETURN 
1340  IF O$(5,3) <  > "1" THEN 1890
1350  IF L = 7 THEN L = 8: GOTO 120
1360  IF L = 12 THEN L = 13
1370  GOTO 120
1390  GOSUB 120: ON J + 1 GOTO 1400,1430,1440,1460,1470,1480,1490,1460,1460,1500,1510,1520,1530,1540,1460,1460,1460,1460,1460,1560,1570,1580,1590,1600,1610,1620,1460,1630,1460,1460,1460,1640,1460
1395  GOTO 1460
1400  IF  VAL (O$(0,3)) < 1 THEN T$ = "THEY'S LUMPY OLE LEATHER SADDLEBAGS.": RETURN 
1410  PRINT "THEY HOLD ";: IF IM = 0 THEN  PRINT "NOTHIN'.": T$ = "": RETURN 
1415  FOR I = 6 TO 22: IF O$(I,2) = "-1" THEN  PRINT " "O$(I,0)",";
1420  NEXT :T$ =  CHR$ (8) + ".": RETURN 
1430 T$ = "YA SEE A WEARY OLD GREY MULE.": RETURN 
1440 T$ = "IT'S AN OLE TORN SACK.": IF O$(18,2) = "-14" THEN O$(18,2) = "14"
1450  RETURN 
1460 T$ = "YA SEE NOTHIN' SPECIAL.": RETURN 
1470 T$ = "IT'S AN OLD WINCHESTER SINGLE-SHOT.": RETURN 
1480 T$ = "LOOKS LIKE A MAP TA TH' LOST DUTCHMAN'S MINE! THAR'S A SKETCH OF SOME BOULDERS.": RETURN 
1490 T$ = "NOTHIN' SPECIAL," + C$ + "JUS' A LOADED SIX SHOOTER.": RETURN 
1500  IF O$(8,2) = "-6" THEN T$ = "LOOKS LIKE SUMTHIN'S 'HIND THE BOTTLES.":O$(8,2) = "6": RETURN 
1505  GOTO 1460
1510 T$ = "THAR'R 3 KEYS, TIED WITH A LEATHER STRAP": RETURN 
    
1520  IF O$(12,2) = "-8" THEN T$ = "LOOKS LIKE THAR'S GLASS UNDER 'UM.":O$(12,2) = "8": RETURN 
1525  GOTO 1460
1530 T$ = "IT'S AN OLE KEROSENE LAMP," + C$ + "FULL TA THE '" +  STR$ (LN) + " TURNS LEFT' MARK.": RETURN 
1540  IF I = 11 THEN T$ = "IT SEZ'KEROSENE.'": RETURN 
1550 T$ = "THAR'S WRITING ON IT.": RETURN 
1560  IF O$(20,2) = "-17" THEN O$(20,2) = "17":T$ = "IT'S A MESSAGE!": RETURN 
1565  GOTO 1460
1570  IF I = 11 THEN T$ = "IT SEZ :" + C$ + "BRING TREASURE TO SALOON, SAY 'SCORE'": RETURN 
    
1575  GOTO 1550
1580  IF I = 11 THEN T$ = "IT SEZ:" + C$ + "WATCH FOR OTHER RIDER FANTASY CREATIONS ADVENTURES!": RETURN 
1585  GOTO 1550
1590 T$ = "THEY'S A SAVAGE LOOKIN' BAND," + C$ + "'N THEY SEEN YA!": RETURN 
1600 T$ = "JUST AN OLE ORE CART FULL O' ROCKS.": RETURN 
1610  IF O$(5,2) = "-25" THEN O$(5,2) = "1":T$ = "THAR'S SOMTHIN' HERE!!": RETURN 
1615  GOTO 1460
1620  IF I = 11 THEN T$ = "IT SAYS:" + C$ + "WELCOME TO FRONTERTOWN.": RETURN 
1625  GOTO 1550
1630  IF O$(10,2) = "-6" THEN O$(10,2) = "6":T$ = "THAR'S A SET OF KEYS THAR!": RETURN 
1635  GOTO 1460
1640 B =  INT ( RND (1) * 3 + 1): ON B GOTO 1650,1660,1670
1650 T$ = "IT'S A BARREL CACTUS.": RETURN 
1660 T$ = "IT'S A CHOLLA CACTUS.": RETURN 
1670 T$ = "IT'S A SAGUARO CACTUS.": RETURN 
1690  IF J = 4 AND I = 38 THEN 1780
1700  IF J <  > 1 OR  VAL (O$(1,3)) < 0 THEN T$ = "SORRY, B' THAT AIN'T POSSIBLE.": RETURN 
    
1710  IF I <  > 38 THEN 1720
1715  IF O$(1,2) = "" THEN T$ = "TENDERFOOT! YA HAVE TA LEAD HIM FIRST.": RETURN 
1717  GOTO 1780
1720  IF O$(1,3) = "1" THEN 1320
    
1723  IF O$(7,3) = "1" THEN  IF I <  > 16 THEN T$ = "TRY 'LEAD.'": RETURN 
1730  ON I - 13 GOTO 1740,1750,1760,1770
1740 T$ = "HE GOT AWAY FROM YA.": RETURN 
1750 T$ = "YER TOO SLOW, HE GOT AWAY.": RETURN 
1760  IF O$(7,3) <  > "1" THEN T$ = "BAD LUCK, YA TRIPPED AND HE RUN OFF.":O$(1,3) = "":O$(1,2) =  STR$ (L): RETURN 
    
1763  IF O$(1,3) = "" THEN 1280
1766  GOTO 1320
1770 T$ = "HE BUCKED YA OFF.": RETURN 
    
1780  INPUT "WITH WHAT?";QM$: IF J = 4 THEN 1800
1782  IF  LEFT$ (QM$ + " ",3) < > "SAD" THEN T$ = "IT FELL OFF." + C$ + "HE GOT AWAY.":O$(1,3) = "":IN = IN - 1:O$(1,2) =  STR$ (L): RETURN 
1790  IF O$(0,3) = "1" THEN O$(0,3) = "2":IN = IN - 1: GOTO 1060
1795  GOTO 1320
1800  IF  LEFT$ (QM$ + " ",3) < > "BUL" THEN 1050
1805  IF O$(8,3) <  > "1" OR O$(4,3) <  > "1" THEN 1320
1810  GOTO 120
1820  IF O$(J,3) <  > "1" THEN 1320
1822  INPUT "ON OR IN WHAT? (IE 'ON TABLE') ?";QM$:P$ =  LEFT$ (QM$ + " ",2): IF P$ <  > "IN" AND P$ <  > "ON" THEN T$ = "HUH?": RETURN 
1830 M1$ =  MID$ (QM$,4,3)
1840  IF P$ <  > "ON" THEN 1850
1841  IF M1$ = "MUL" THEN 1790
1843  IF J <  > 13 THEN 1320
1845  IF LM = 0 THEN T$ = "IT'S EMPTY.": RETURN 
1847 LM = 0: GOTO 1320
1850  IF M1$ <  > "SAD" THEN 1860
1852  IF J < 5 OR J = 19 OR J = 22 THEN T$ = "SORRY, IT DON'T FIT.": RETURN 
1855  IF IM > 4 THEN T$ = "THE BAGS 'R FULL.": RETURN 
1857 O$(J,2) = "-1":O$(J,3) = "":IN = IN - 1:IM = IM + 1:T$ = "OK": RETURN 
1860  IF M1$ = "MUL" THEN T$ = "YA GOT KICKED!": RETURN 
1870  IF M1$ <  > "LAN" THEN 1050
1880 T$ = "JAR'S EMPTY.": RETURN 
    
1890 T$ = "YA CAN'T DO THAT... YET!
1900  PRINT T$:T$ = "": GOTO 100
    
1920  PRINT "YA GOT WITH YA: ";: IF IN = 0 AND O$(1,3) <  > "1" THEN T$ = "NOTHIN'": RETURN 
    
1930 B =  VAL (O$(0,3)):X1 = B: IF B <  > 1 THEN 1940
1933  PRINT : PRINT O$(0,0)", CONTAINING: ";: FOR I = 1 TO 22: IF O$(I,2) = "-1" THEN  PRINT O$(I,0)". ";:X1 = 2
1936  NEXT : IF X1 <  > 2 THEN  PRINT "NOTHIN'! ";
1940  IF O$(1,3) = "1" THEN  PRINT : PRINT "THE MULE, WHICH YER LEADIN. ";: IF B = 2 THEN  PRINT "(CARRYIN' SADDLEBAGS) ";
1950  PRINT :X1 = 0: FOR I = 2 TO 22: IF  VAL (O$(I,3)) < 1 THEN 1970
1955  IF CL = 1 AND I = 12 THEN PRINT "LIT ";
1960  PRINT O$(I,0)". ";
1970  NEXT 
1980 T$ = "": RETURN 
2000  IF L = 19 THEN  HOME : PRINT "                            YA FELL 100 FEET 'N LANDED ON ROCKS.": PRINT "                            YER DEAD.": GOTO 2220
2005  IF J = 23 OR J = 32 OR M1$ = "P" THEN T$ = "OK": RETURN 
    
2006  GOTO 1050
2010 T$ = "WHERE?": RETURN 
2020  IF L = 25 THEN L = 26: GOTO 120
2030  IF L = 26 THEN L = 25: GOTO 120
2040  GOTO 120
2100  DATA ,,,,,,,,IN A MINER'S SHACK,WINDOW. DOOR,,1,3,3,,,IN A DESERT,ROAD. MOUNTAINS. DESERT,WNES,2,4,2,2,2,ON A DIRT PATH,MINER'S SHACK. ROAD. MOUNTAINS," W  ",5,1,4,2,2
2110  DATA ON A DIRT ROAD,MOUNTAINS. PATH. DESERT. TOWN,NE S,2,7,3,2,5,IN A GHOST TOWN,SALOON," N  ",1,6,4,,,IN A SALOON,,W   ,1,5,,,
2120  DATA AT THE SUPERSTITION MOUNTAINS,ROAD. DESERT,S N ,4,4,2,2,2,AT WEAVER'S NEEDLE,,NS  ,1,9,7,,
2130  DATA IN A NARROW DEFILE,BUSHES. CAVES,"  NS",1,11,10,12,8,IN A SMALL CAVE,,E   ,1,9,,,,BEHIND A BUSH,BUSH.,W   ,1,9,,,,IN A BOX CANYON,BUSHES. TREES. BOULDERS,"   S",4,12,12,12,9
2140  DATA IN FRONT OF A HIDDEN MINE,MINE SHAFT,ESNW,8,14,12,12,12,IN THE MOUTH OF A DIM MINE,MINE SHAFT,EW  ,1,15,13,,,IN A MINE,DARK TUNNEL,EW  ,6,16,14,,
2150  DATA AT THE END AF A TUNNEL,IRON DOOR," W  ",1,17,15,,,IN A LARGE CHAMBER,IRON DOOR,WNES,1,16,19,18,20,IN A MAZE OF TUNNELS,,NSEW,6,18,18,18,18,IN FRONT OF A PIT,DARK HOLE," S  ",7,23,17,,
2160  DATA AT AN INTERSECTION,," SNW",6,,22,17,21,IN A DEAD END TUNNEL,,E   ,8,20,,,,IN A BLOCKED TUNNEL,,N   ,8,20,,,,AT THE BOTTOM OF A SHAFT,WALL,S   ,8,24,,,
2170  DATA IN A TUNNEL,,NS  ,6,23,25,,,AT THE END OF A TUNNEL,LADDER," N  ",6,26,24,,,AT THE TOP OF A LADDER,,,6,1,25,,
2180  DATA SADDLEBAGS,2,1,MULE,,3,BURLAP SACK,4,14,SHOVEL,4,1,RIFLE,4,1,MAP,1,-25,GUN,1,-1,CARROTS,1,-1,BOX OF RIFLE BULLETS,1,-6,WHISKEY BOTTLES,2,6,KEYS,1,-6,PILE OF BONES (MINE),2,8
2190  DATA LANTERN,2,-8,JAR OF LIQUID,1,-11,*SPANISH COINS*,1,-10,*TOURQUISE*,1,-23,#SILVER#,1,-21,#GOLD#,1,-22,MATCHES,1,-14,CRATES,3,17,NOTE,1,-17,PAPER,1,6,INDIANS,,11
2200  DATA ORE CART,,14,BED,,1,SIGN,,5,BROKEN GLASS,,6,TABLES,,6,CHAIRS,,6,ROCKS,,10,WOODEN RAILS,,15,CACTUS,,2,TRAP DOOR,,26
2210  DATA GO ,ENT,RUN,QUI,DIG,CLI,INV,JUM,DRO,GET,EXA,REA,LOO,MOV,CAT,CHA,LEA,RID,PUT,PLA,PIC,PUS,PUL,SHO,OPE,CLO,GIV,BUR,LIG,DRI,BRE,HIT,UNL,LOC,TAK,LIS,SAY,FEE,LOA,UNT,FOL,POU,EAT,NOR,SOU,EAS,WES
2220  VTAB 24: INPUT "DO YA WANT TA TRY AGIN?";QM$: IF  LEFT$ (QM$ + " ",1) = "Y" THEN  10 
    
2230  POKE 34,0: POKE 35,24: HOME: GOTO 10 
2300  PRINT "TRY ESAM'NIN THIN'S.": RETURN 
2310  PRINT "ROADS GO PLACES.": RETURN 
2320  PRINT "MAYBE THE TRAIL GOES SUMWHAR.": RETURN 
2330  IF O$(5,3) <  > "1" THEN  PRINT "YA GOT A MAP?": RETURN 
2340  PRINT "TRY 'FOLLOW.'": RETURN 
    
2350  IF  VAL (O$(1,2)) = L THEN PRINT "THE MULE LOOKS THIN.": RETURN 
2360  GOTO 2300
2370  PRINT "KEEP GOIN'.": RETURN 
    
2380  PRINT "IT'S SLIPRY, BUTCHAMIGHT MAKE IT DOWN.": RETURN 
    
2390  PRINT "THIS HERE'S A MINE, YA KNOW.": RETURN 
2500  IF J = 7 THEN T$ = "THEY TASTE PURTY GOOD.": RETURN 
2510 T$ = "CAN'T DO THAT... WEIRDO !": RETURN 
2520  INPUT "SURE?";QM$: IF  LEFT$ (QM$ + " ",1) <  > "Y" THEN 390
2530  INPUT "NAME?";QM$:QM$ = "LDG/" + QM$: ONERR  GOTO 4100
    
2535  PRINT "OPEN "QM$: PRINT "WRITE "QM$
2540  PRINT L","CL","LN","IN","LM","U","U1","IM
2541  FOR X = 0 TO 32: PRINT O$(X,1)","O$(X,2)","O$(X,3): NEXT 
    
2550  PRINT "CLOSE "QM$: POKE 216,0: PRINT "SAVED!": GOTO 390
4000  POKE 216,0: PRINT 
4010  IF EE = 6 THEN  PRINT "NO FILE NAMED "QM$: GOTO 70
4020  PRINT ">>> DISK PROBLEM. ERROR #"EE: GOTO 70
4100  POKE 216,0: PRINT "CLOSE"QM$:EE =  PEEK (222)
4110  IF EE = 2 OR EE = 3 OR EE = 11 THEN  PRINT "BAD NAME. TRY ANOTHER.": GOTO 2530
4120  IF EE = 4 THEN  PRINT "DISK WRITE PROTECTED!": GOTO 2520
4130  IF EE = 9 THEN  PRINT "DISK FULL!": PRINT "DELET"QM$: GOTO 2520
4140  IF EE = 10 THEN  PRINT "FILE LOCKED! (AND I DIDN'T DO IT!)": GOTO 390
4150  PRINT ">>> DISK PROBLEM. ERROR #"EE: GOTO 390
9999  END 
10000  REM ***************
10001  REM  CONTRUBUTED BY
10002  REM   JERRY FICKE