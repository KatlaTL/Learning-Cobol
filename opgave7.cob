       PROGRAM-ID. opgave7.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                   SELECT CUSTOMER-FILE-IN ASSIGN "customers.txt"
                   ORGANIZATION IS LINE SEQUENTIAL.

                   SELECT CUSTOMER-FILE-OUT ASSIGN "customersOut.txt"
                   ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
               FD CUSTOMER-FILE-IN.
               01 KUNDEOPL-IN.
                   COPY "customer.cpy".
               
               FD CUSTOMER-FILE-OUT.
               01 KUNDEOPL-OUT.
                   05 LINE-CONTENT PIC X(300).
    
           WORKING-STORAGE SECTION.
               01 EOF-FLAG PIC X VALUE "N".
               01 WS-LENGTH PIC 9(3) COMP.
       
       PROCEDURE DIVISION.
           OPEN INPUT CUSTOMER-FILE-IN.
           OPEN OUTPUT CUSTOMER-FILE-OUT.

           PERFORM UNTIL EOF-FLAG = "Y"
               READ CUSTOMER-FILE-IN INTO KUNDEOPL-IN
                   AT END MOVE "Y" TO EOF-FLAG
                   NOT AT END
                       PERFORM FORMAT-KUNDE-ID
                       WRITE KUNDEOPL-OUT
                           FROM LINE-CONTENT (1: WS-LENGTH)
                           AFTER ADVANCING 1 LINE
                           
                       PERFORM FORMAT-NAME
                       WRITE KUNDEOPL-OUT
                           FROM LINE-CONTENT (1: WS-LENGTH)
                           AFTER ADVANCING 1 LINE

                       PERFORM FORMAT-ADDRESS-LINE-1
                       WRITE KUNDEOPL-OUT
                           FROM LINE-CONTENT (1: WS-LENGTH)
                           AFTER ADVANCING 1 LINE

                       PERFORM FORMAT-ADDRESS-LINE-2
                       WRITE KUNDEOPL-OUT
                           FROM LINE-CONTENT (1: WS-LENGTH)
                           AFTER ADVANCING 1 LINE

                       PERFORM FORMAT-TELEFON
                       WRITE KUNDEOPL-OUT
                           FROM LINE-CONTENT (1: WS-LENGTH)
                           AFTER ADVANCING 1 LINE

                       PERFORM FORMAT-EMAIL
                       WRITE KUNDEOPL-OUT
                           FROM LINE-CONTENT (1: WS-LENGTH)
                           AFTER ADVANCING 1 LINE
                                           
                       MOVE SPACES TO LINE-CONTENT
                       MOVE 1 TO WS-LENGTH 

                       WRITE KUNDEOPL-OUT
                           FROM LINE-CONTENT (1: WS-LENGTH)
                           AFTER ADVANCING 1 LINE
               END-READ
           END-PERFORM.
           CLOSE CUSTOMER-FILE-OUT.
           CLOSE CUSTOMER-FILE-IN.

           FORMAT-KUNDE-ID.
               MOVE SPACES TO LINE-CONTENT
               MOVE 1 TO WS-LENGTH

               STRING
                   "Kunde ID: " DELIMITED BY SIZE
                   FUNCTION TRIM(KUNDE-ID) DELIMITED BY SPACE
                   INTO LINE-CONTENT
                   WITH POINTER WS-LENGTH
               END-STRING
               
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
           EXIT.

           FORMAT-EMPTY-LINE.
               
           EXIT.          

       STOP RUN.
