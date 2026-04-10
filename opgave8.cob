       PROGRAM-ID. opgave8.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                   SELECT CUSTOMER-FILE-IN ASSIGN "customers.txt"
                   ORGANIZATION IS LINE SEQUENTIAL.

                   SELECT CUSTOMER-KONTO-IN ASSIGN "kontoOpl.txt"
                   ORGANIZATION IS LINE SEQUENTIAL.

                   SELECT CUSTOMER-KONTO-OUT ASSIGN "customerKonto.txt"
                   ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
               FD CUSTOMER-FILE-IN.
               01 KUNDEOPL-IN.
                   COPY "customer.cpy".
               
               FD CUSTOMER-KONTO-OUT.
               01 KUNDEOPL-OUT.
                   05 LINE-CONTENT PIC X(300).

               FD CUSTOMER-KONTO-IN.
               01 KONTOOPL-IN.
                   COPY "kontoOpl.cpy".
    
           WORKING-STORAGE SECTION.
               01 EOF-FLAG PIC X VALUE "N".
               01 EOF-FLAG-KONTO PIC X VALUE "N".
               01 WS-LENGTH PIC 9(3) COMP.
       
       PROCEDURE DIVISION.
           OPEN INPUT CUSTOMER-FILE-IN.
           OPEN OUTPUT CUSTOMER-KONTO-OUT.

           PERFORM UNTIL EOF-FLAG = "Y"
               READ CUSTOMER-FILE-IN INTO KUNDEOPL-IN
                   AT END MOVE "Y" TO EOF-FLAG
                   NOT AT END
                       PERFORM FORMAT-KUNDE-ID
                       PERFORM FORMAT-NAME
                       PERFORM FORMAT-ADDRESS-LINE-1
                       PERFORM FORMAT-ADDRESS-LINE-2
                       PERFORM FORMAT-TELEFON
                       PERFORM FORMAT-EMAIL
                       PERFORM FORMAT-KONTO
                       PERFORM FORMAT-EMPTY-LINE
               END-READ
            END-PERFORM.         

           FORMAT-KUNDE-ID.
               MOVE SPACES TO LINE-CONTENT
               MOVE 1 TO WS-LENGTH

               STRING
                   "Kunde ID: " DELIMITED BY SIZE
                   FUNCTION TRIM(KUNDE-ID OF KUNDEOPL-IN)
                       DELIMITED BY SPACE
                   INTO LINE-CONTENT
                   WITH POINTER WS-LENGTH
               END-STRING

               WRITE KUNDEOPL-OUT
                   FROM LINE-CONTENT (1: WS-LENGTH)
                   AFTER ADVANCING 1 LINE
            EXIT.

            FORMAT-NAME.
               MOVE SPACES TO LINE-CONTENT
               MOVE 1 TO WS-LENGTH
               
               STRING 
                   FUNCTION TRIM(FIRST-NAME) DELIMITED BY SPACE
                       " " DELIMITED BY SIZE                     
                   FUNCTION TRIM(LAST-NAME) DELIMITED BY SPACE
                   INTO LINE-CONTENT
                   WITH POINTER WS-LENGTH
               END-STRING

               WRITE KUNDEOPL-OUT
                   FROM LINE-CONTENT (1: WS-LENGTH) 
                   AFTER ADVANCING 1 LINE
            EXIT.

            FORMAT-ADDRESS-LINE-1.
               MOVE SPACES TO LINE-CONTENT
               MOVE 1 TO WS-LENGTH
               
               STRING
                   FUNCTION TRIM(VEJNAVN) DELIMITED BY SIZE
                   " " DELIMITED BY SIZE
                   FUNCTION TRIM(HUSNR) DELIMITED BY SPACE
                   ", " DELIMITED BY SIZE  
                   FUNCTION TRIM(ETAGE) DELIMITED BY SPACE
                   " " DELIMITED BY SIZE 
                   FUNCTION TRIM(SIDE) DELIMITED BY SPACE
                   INTO LINE-CONTENT
                   WITH POINTER WS-LENGTH
               END-STRING

               WRITE KUNDEOPL-OUT
                   FROM LINE-CONTENT (1: WS-LENGTH) 
                   AFTER ADVANCING 1 LINE
            EXIT.

            FORMAT-ADDRESS-LINE-2.
               MOVE SPACES TO LINE-CONTENT
               MOVE 1 TO WS-LENGTH
                
               STRING
                   FUNCTION TRIM(POSTNR) DELIMITED BY SPACE
                      " " DELIMITED BY SIZE
                   FUNCTION TRIM(BY-X) DELIMITED BY SPACE
                      ", " DELIMITED BY SIZE
                   FUNCTION TRIM(LANDE-KODE)
                       DELIMITED BY SPACE
                   INTO LINE-CONTENT
                   WITH POINTER WS-LENGTH
               END-STRING

               WRITE KUNDEOPL-OUT
                   FROM LINE-CONTENT (1: WS-LENGTH)
                   AFTER ADVANCING 1 LINE
            EXIT.

            FORMAT-TELEFON.
               MOVE SPACES TO LINE-CONTENT
               MOVE 1 TO WS-LENGTH

               STRING
                   "Tlf.: " DELIMITED BY SIZE
                   FUNCTION TRIM(TELEFON) DELIMITED BY SPACE
                      INTO LINE-CONTENT
                      WITH POINTER WS-LENGTH
               END-STRING

               WRITE KUNDEOPL-OUT
                   FROM LINE-CONTENT (1: WS-LENGTH)
                   AFTER ADVANCING 1 LINE
           EXIT.

           FORMAT-EMAIL.
               MOVE SPACES TO LINE-CONTENT
               MOVE 1 TO WS-LENGTH
       
               STRING
                   "Email.: " DELIMITED BY SIZE
                   FUNCTION TRIM(EMAIL) DELIMITED BY SPACE
                      INTO LINE-CONTENT
                      WITH POINTER WS-LENGTH
               END-STRING

               WRITE KUNDEOPL-OUT
                   FROM LINE-CONTENT(1: WS-LENGTH) 
                   AFTER ADVANCING 1 LINE
           EXIT.

           FORMAT-KONTO.
                OPEN INPUT CUSTOMER-KONTO-IN.
                
                MOVE "N" TO EOF-FLAG-KONTO

                PERFORM UNTIL EOF-FLAG-KONTO = "Y"
                   READ CUSTOMER-KONTO-IN INTO KONTOOPL-IN
                       AT END MOVE "Y" TO EOF-FLAG-KONTO
                       NOT AT END
                           IF KUNDE-ID OF KUNDEOPL-IN = 
                               KUNDE-ID OF KONTOOPL-IN
                           THEN

                           MOVE SPACES TO LINE-CONTENT
                           MOVE 1 TO WS-LENGTH
                           STRING 
                               "Konto ID: " DELIMITED BY SIZE
                               FUNCTION TRIM(KONTO-ID)
                                   DELIMITED BY SPACE
                               INTO LINE-CONTENT
                               WITH POINTER WS-LENGTH
                           END-STRING

                           WRITE KUNDEOPL-OUT
                           FROM LINE-CONTENT (1: WS-LENGTH)
                           AFTER ADVANCING 1 LINE


                           MOVE SPACES TO LINE-CONTENT
                           MOVE 1 TO WS-LENGTH
                           STRING 
                               "Konto type: " DELIMITED BY SIZE
                               FUNCTION TRIM(KONTO-TYPE)
                                   DELIMITED BY SPACE
                               INTO LINE-CONTENT 
                               WITH POINTER WS-LENGTH
                           END-STRING

                           WRITE KUNDEOPL-OUT
                           FROM LINE-CONTENT (1: WS-LENGTH)
                           AFTER ADVANCING 1 LINE


                           MOVE SPACES TO LINE-CONTENT
                           MOVE 1 TO WS-LENGTH
                           STRING 
                               "Balance: " DELIMITED BY SIZE
                               FUNCTION TRIM(BALANCE OF KONTOOPL-IN)
                                   DELIMITED BY SPACE
                               " " DELIMITED BY SIZE
                               FUNCTION TRIM (VALUTA-KD)
                                   DELIMITED BY SPACE
                               INTO LINE-CONTENT
                               WITH POINTER WS-LENGTH
                           END-STRING

                           WRITE KUNDEOPL-OUT
                           FROM LINE-CONTENT (1: WS-LENGTH)
                           AFTER ADVANCING 1 LINE                              
                              
                           END-IF
                       END-READ
                   END-PERFORM.
                CLOSE CUSTOMER-KONTO-IN.
            EXIT.

           FORMAT-EMPTY-LINE.
               MOVE SPACES TO LINE-CONTENT
               MOVE 1 TO WS-LENGTH
               WRITE KUNDEOPL-OUT
                   FROM LINE-CONTENT (1: WS-LENGTH)
                   AFTER ADVANCING 1 LINE
           EXIT.         
       
      * Dummy paragraph to top pargraph fallthrough  
           DUMMY-PARA.
            EXIT.

           CLOSE CUSTOMER-KONTO-OUT.
           CLOSE CUSTOMER-FILE-IN. 

       STOP RUN.
