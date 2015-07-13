package com.twilightfair.juno.language.asoft

import org.specs2.mutable.Specification

import scala.concurrent.Await
import scala.concurrent.duration._

/**
 * Created by jthomas on 12/11/13.
 */

class AsoftSpec extends Specification {

  def runScript(script: String): String = {
    val res = Asoft.run(script.toStream)
    val out = Await.result(res, 500 nano).outBuffer
    out.reverse.mkString("")
  }

  def graphScript(script: String): String = {
    Asoft.graph(script.toStream)
  }

  private val script =
    """
1 DIM A(5), B%(2,4)
2 A(2) = 900
3 FOR K = 2 TO 5
4 FOR J = 0 TO 6 STEP 2
5 PRINT "K,J: ";J;" ";K
6 NEXT J,K
7 FOR N = 5 TO 0 STEP -1: PRINT "N: ";N
8 NEXT N
9 FOR D = 100 TO 110:PRINT "D: ";D:NEXT D
10 PRINT "Array value: ";A(2)
11 PRINT "Bad Array value: ";B%(1,1)
12 B%(3,2) = 99
15 X% = 4 : Y = 9
17 PRINT "Multiple array value: ";B%(3,2)
20 PRINT 2 / (7 + (3 - X%) * 6 + Y / 6 ) - -5
22 X% = 5
23 Y = X%
24 PRINT 2 / (7 + (3 - X%) * 6 + Y / 6 ) - -5
25 GOSUB 100
26 IF X% <> 4 THEN X% = 7.6 : GOTO 40
30 PRINT "]";
40 IF X% == 6 THEN 60
50 PRINT "Say, ";
60 PRINT "Hello, ";"World ";X%
70 GOTO 150
100 PRINT "Gosub!"
110 ON X% GOTO 10, 20, 30, 40, 50, 130, 100
120 RETURN
130 PRINT "ON GOTO!"
140 RETURN
150 DIM LAZY$(8)
160 FOR I = 0 TO 7:READ LAZY$(I):NEXT I
170 FOR I = 0 TO 7:PRINT LAZY$(I);:NEXT I
180 READ X%, Y%, Z%, W%
190 RESTORE
200 READ A$
210 PRINT
220 X% = X% + Y% + Z% + W%
230 PRINT A$;" answer is: ";X%
240 PRINT "Done."
10000 DATA The, quick, brown fox, jumped over
10010 DATA some, lazy, " dogs, wasn't that", cute?
10020 DATA 10, 20, 30, 40
    """.stripMargin

  "The Asoft Language" should {

    "input" in {
      val out = runScript("""10 INPUT "Type soemthing: ", A$""")
      out mustEqual "Hello, World\n"
    }

    /*
        "print" in {
          val out = runScript("""10 PRINT "Hello, World"""")
          out mustEqual "Hello, World\n"
        }

        "print with a concatenation" in {
          val out = runScript("""10 PRINT "Hello, World";"!"""")
          out mustEqual "Hello, World!\n"
        }

        "print without a trailing carriage return" in {
          val out = runScript("10 PRINT \"Hello, World\";")
          out mustEqual "Hello, World"
        }

        "add two numbers" in {
          val out = runScript("""10 PRINT 1 + 2;""")
          out mustEqual "3.0"
        }

        "subtract two numbers" in {
          val out = runScript("""10 PRINT 1 - 2;""")
          out mustEqual "-1.0"
        }

        "multiply two numbers" in {
          val out = runScript("""10 PRINT 3 * 2;""")
          out mustEqual "6.0"
        }

        "divide two numbers" in {
          val out = runScript("""10 PRINT 10 / 2;""")
          out mustEqual "5.0"
        }

        "follow precedence" in {
          val out = runScript("""10 PRINT 2 + 3 * 4 - 1;""")
          out mustEqual "13.0"
        }

        "follow precedence 2" in {
          val out = runScript("""10 PRINT (1 - -5) + 1;""")
          out mustEqual "7.0"
        }

        "compute a big expression" in {
          val out = runScript("""10 PRINT 2 + (7 + (3 - 1) * 6 + 18 / -6 ) - -5;""")
          out mustEqual "23.0"
        }

        "execute one line multiple commands" in {
          val out = runScript("10 PRINT \"Hello, \";:PRINT \"Sailor!\";")
          out mustEqual
            """Hello, Sailor!"""
        }

        "run a for loop" in {
          val out = runScript("10 FOR D = 100 TO 110:PRINT \"D: \";D:NEXT D")
          out mustEqual
            """D: 100
    D: 101.0
    D: 102.0
    D: 103.0
    D: 104.0
    D: 105.0
    D: 106.0
    D: 107.0
    D: 108.0
    D: 109.0
    D: 110.0
    """
        }

        "run a for loop with a step" in {
          val out = runScript("10 FOR I = 0 TO 10 STEP 2:PRINT \"I: \";I:NEXT I")
          out mustEqual
            """I: 0
    I: 2.0
    I: 4.0
    I: 6.0
    I: 8.0
    I: 10.0
    """
        }

        "run a for loop with a negative step" in {
          val out = runScript("10 FOR I = 10 TO 0 STEP -1:PRINT \"I: \";I:NEXT I")
          out mustEqual
            """I: 10
    I: 9.0
    I: 8.0
    I: 7.0
    I: 6.0
    I: 5.0
    I: 4.0
    I: 3.0
    I: 2.0
    I: 1.0
    I: 0.0
    """
        }

        "run a nested for loop" in {
          val out = runScript("""
    10 FOR K = 2 TO 5
    20 FOR J = 0 TO 6 STEP 2
    30 PRINT "K,J: ";K;" ";J
    40 NEXT J,K
    """)
          out mustEqual
            """K,J: 2 0
    K,J: 2 2.0
    K,J: 2 4.0
    K,J: 2 6.0
    K,J: 3.0 0
    K,J: 3.0 2.0
    K,J: 3.0 4.0
    K,J: 3.0 6.0
    K,J: 4.0 0
    K,J: 4.0 2.0
    K,J: 4.0 4.0
    K,J: 4.0 6.0
    K,J: 5.0 0
    K,J: 5.0 2.0
    K,J: 5.0 4.0
    K,J: 5.0 6.0
    """
        }

        "store variables" in {
          val out = runScript(
            """
    10 X% = 4 : Y = 9 : S$ = "Petunia"
    20 PRINT X%;", ";Y;", ";S$;
            """)
          out mustEqual
            """4, 9.0, Petunia"""
        }

        "use variables in expressions" in {
          val out = runScript(
            """
    10 X% = 4 : Y = 9
    20 PRINT X% * Y + 1;
    """)
          out mustEqual
            """37.0"""
        }

        "assign an array value" in {
          val out = runScript(
            """
    10 DIM A(5)
    20 A(2) = 900
    30 PRINT A(2);
            """)
          out mustEqual
            """900.0"""
        }

        "assign a multiple array value" in {
          val out = runScript(
            """
    10 DIM B%(2,4)
    20 B%(1, 2) = 300
    30 PRINT B%(1, 2);
            """)
          out mustEqual
            """300"""
        }

        "assign an array value in a FOR" in {
          val out = runScript(
            """
    10 DIM A(5)
    20 FOR I = 0 TO 4:A(I) = I:NEXT I
    30 PRINT A(2);
            """)
          out mustEqual
            """2.0"""
        }

        "simple IF statement true" in {
          val out = runScript(
            """
    10 IF 2 > 1 THEN PRINT "Yes!";
    """)
          out mustEqual
            """Yes!"""
        }

        "simple IF statement false" in {
          val out = runScript(
            """
    10 IF 2 < 1 THEN PRINT "Yes!";
            """)
          out mustEqual
            """"""
        }

        "IF statement with compound line" in {
          val out = runScript(
            """
    10 IF 5 * 2 > 9 THEN X% = 10 : PRINT "A ";X%;
    20 PRINT "!";
            """)
          out mustEqual
            """A 10!"""
        }

        "IF doesn't execute compound on false" in {
          val out = runScript(
            """
    10 IF 2 > 4 THEN X% = 10 : PRINT "A ";X%;
    20 PRINT "!";
            """)
          out mustEqual
            """!"""
        }

        "should GOTO" in {
          val out = runScript(
            """
    10 GOTO 30
    20 PRINT "Hello, ";
    30 PRINT "World";
            """)
          out mustEqual
            """World"""
        }

        "should GOTO back and forward" in {
          val out = runScript(
            """
    10 GOTO 40
    20 PRINT "World";
    30 GOTO 60
    40 PRINT "Hello, ";
    50 GOTO 20
    60 PRINT "!";
            """)
          out mustEqual
            """Hello, World!"""
        }

        "should GOSUB and RETURN" in {
          val out = runScript(
            """
    10 GOSUB 40
    20 PRINT "World";
    30 GOTO 60
    40 PRINT "Hello, ";
    50 RETURN
    60 PRINT "!";
            """)
          out mustEqual
            """Hello, World!"""
        }

        "should GOSUB and RETURN to different locations" in {
          val out = runScript(
            """
    10 GOSUB 60
    20 PRINT "World! ";
    30 GOSUB 60
    40 PRINT "Sailor";
    50 GOTO 80
    60 PRINT "Hello, ";
    70 RETURN
    80 PRINT "!";
            """)
          out mustEqual
            """Hello, World! Hello, Sailor!"""
        }

        "IF THEN goto" in {
          val out = runScript(
            """
    5 X% = 2
    10 IF 10 / X% < X% * 3 THEN 40
    20 PRINT "World";
    30 GOTO 60
    40 PRINT "Hello, ";
    60 PRINT "!";
            """)
          out mustEqual
            """Hello, !"""
        }

        "ON GOTO" in {
          val out = runScript(
            """
    10 X% = 3
    20 ON X% GOTO 600, 600, 50, 600, 600, 130, 100
    30 PRINT "World";
    40 GOTO 60
    50 PRINT "Hello, ";
    60 PRINT "!";
            """)
          out mustEqual
            """Hello, !"""
        }

        "ON GOSUB" in {
          val out = runScript(
            """
    10 X% = 3
    20 ON X% GOSUB 600, 600, 50, 600, 600, 130, 100
    30 PRINT "World";
    40 GOTO 70
    50 PRINT "Hello, ";
    60 RETURN
    70 PRINT "!";
            """)
          out mustEqual
            """Hello, World!"""
        }

        "store DATA" in {
          val script = """
    10 DATA This, is a, test
    20 DATA Of, the DATA, command
                       """
          val out = Asoft.parse(script.toStream)
          out.elements.size mustEqual 7
          out.parser.data("Data").asInstanceOf[List[String]].mkString("") mustEqual " This is a test Of the DATA command"

        }

        "store DATA with quoted string" in {
          val script = """
    10 DATA This, is a, test
    20 DATA " Of, the DATA", command
                       """
          val out = Asoft.parse(script.toStream)
          out.elements.size mustEqual 7
          out.parser.data("Data").asInstanceOf[List[String]].mkString("") mustEqual " This is a test Of, the DATA command"

        }
    */

    "pull data into a var with a READ" in {
      val script = """
10 DATA This, is a, test
20 DATA Of, the DATA, command
40 READ A$
50 PRINT A$;
                   """
      val out = runScript(script)
      out mustEqual " This"
    }

    "pull multiple data into vars with a READ" in {
      val script = """
10 DATA This, 5, is a, test
20 DATA Of, the DATA, command
40 READ A$, B%
50 PRINT B%;" ";A$;
                   """
      val out = runScript(script)
      out mustEqual "5  This"
    }

    "pull multiple data into vars with more than one READ" in {
      val script = """
10 DATA This, 5, is a, test
20 DATA Of, the DATA, command
40 READ A$, B%
50 PRINT B%;" ";A$;
60 READ C$, A$, D$
70 PRINT C$;A$;D$;
                   """
      val out = runScript(script)
      out mustEqual "5  This is a test Of"
    }

    "pull multiple data using a FOR loop into an array" in {
      val script = """
5 DIM A$(6)
10 FOR I = 0 TO 5:READ A$(I):NEXT I
20 FOR I = 0 TO 5:PRINT A$(I);:NEXT I
30 DATA This, is a, test
40 DATA Of, the DATA, command
                   """
      val out = runScript(script)
      out mustEqual " This is a test Of the DATA command"
    }
    
    "use a RESTORE to reset READ data" in {
      val script = """
10 DATA This, is a, test
20 DATA Of, the DATA, command
40 READ A$, B$
50 PRINT A$;" ";B$;
60 RESTORE
70 READ C$, A$, D$
80 PRINT C$;A$;D$;
                   """
      val out = runScript(script)
      out mustEqual " This  is a This is a test"
    }


    "run a big script" in {
      val out = runScript(script)
      out mustEqual """K,J: 0 2
K,J: 2.0 2
K,J: 4.0 2
K,J: 6.0 2
K,J: 0 3.0
K,J: 2.0 3.0
K,J: 4.0 3.0
K,J: 6.0 3.0
K,J: 0 4.0
K,J: 2.0 4.0
K,J: 4.0 4.0
K,J: 6.0 4.0
K,J: 0 5.0
K,J: 2.0 5.0
K,J: 4.0 5.0
K,J: 6.0 5.0
N: 5
N: 4.0
N: 3.0
N: 2.0
N: 1.0
N: 0.0
D: 100
D: 101.0
D: 102.0
D: 103.0
D: 104.0
D: 105.0
D: 106.0
D: 107.0
D: 108.0
D: 109.0
D: 110.0
Array value: 900.0
Bad Array value: 0
Multiple array value: 99
5.8
4.52
Gosub!
Say, Hello, World 5
 The quick brown fox jumped over some lazy dogs, wasn't that cute?
 The answer is: 100
Done.
"""
    }
  }
}
