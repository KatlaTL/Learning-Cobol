       PROGRAM-ID. opgave11.

       ENVIRONMENT DIVISION.
           CONFIGURATION SECTION.
               SPECIAL-NAMES.
                  DECIMAL-POINT IS COMMA.

           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                   SELECT FD-BANKER ASSIGN "opgave11/Banker.txt"
                   ORGANIZATION IS LINE SEQUENTIAL.
           
                   SELECT FD-TRANSACTION-IN
                       ASSIGN "opgave11/Transaktioner.txt"
                   ORGANIZATION IS LINE SEQUENTIAL.

                   SELECT FD-TRANSACTION-OUT 
                       ASSIGN "opgave11/Kontoudskrifter.txt"
                   ORGANIZATION IS LINE SEQUENTIAL.

                   SELECT SORT-FILE ASSIGN TO WRK.
        
       DATA DIVISION.
           FILE SECTION.
               FD FD-BANKER.
               01 IN-BANKER.
                   COPY "opgave11/banker.cpy".
        
               FD FD-TRANSACTION-IN.
               01 IN-TRANSACTION.
                   COPY "opgave11/transactions.cpy".
               
               FD FD-TRANSACTION-OUT.
               01 OUT-TRANSACTION.
                   05 LINE-CONTENT PIC X(300).
               SD SORT-FILE.
               01 SORT-REC.
                   COPY "opgave11/transactions.cpy".

            WORKING-STORAGE SECTION.
               01 WS-BANKER-COUNT PIC 9(3) COMP VALUE 0.
               01 WS-BANKER-ENTRY OCCURS 0 TO 300 TIMES
                       DEPENDING ON WS-BANKER-COUNT
                       INDEXED BY IDX IDX2.
                   COPY "opgave11/banker.cpy".
               01 WS-TEMP-BANKER.
                   COPY "opgave11/banker.cpy".
               01 WS-EOF-FLAG-BANKER PIC X VALUE "N".
               01 WS-EOF-FLAG-TRANSACTION PIC X VALUE "N".
               01 WS-CURRENT-KONTO PIC X(15) VALUE SPACES.
               01 WS-CONVERTED-VALUTA-AMOUNT PIC S9(11)V99 COMP-3.
               01 WS-GROUP-SUM PIC S9(11)V99 COMP-3 VALUE 50000.
               01 WS-GROUP-SUM-DISPLAY PIC Z.ZZZ.ZZZ.ZZ9,99.
               01 WS-GROUP-SUM-IN PIC S9(11)V99 COMP-3 VALUE 0.
               01 WS-GROUP-SUM-IN-DISPLAY PIC Z.ZZZ.ZZZ.ZZ9,99.
               01 WS-GROUP-SUM-OUT PIC S9(11)V99 COMP-3 VALUE 0.
               01 WS-GROUP-SUM-OUT-DISPLAY PIC Z.ZZZ.ZZZ.ZZ9,99.
               01 WS-SPACES PIC X(45) VALUE SPACES.
                
       
       PROCEDURE DIVISION.
       
           MAIN-PROCESS.
               PERFORM INIT


               STOP RUN.

      *--------------------
           INIT.
               PERFORM LOAD-BANKER-FILE.

               SORT SORT-FILE
                  ON ASCENDING KEY KONTO-ID OF SORT-REC
                  INPUT PROCEDURE IS LOAD-TRANSACTIONERS-FILE
                  OUTPUT PROCEDURE IS PROCESS-SORTED
            EXIT.
      *--------------------
           LOAD-BANKER-FILE.
               OPEN INPUT FD-BANKER.
                   PERFORM UNTIL WS-EOF-FLAG-BANKER = "Y"
                       READ FD-BANKER
                           AT END
                               MOVE "Y" TO WS-EOF-FLAG-BANKER
                           NOT AT END
                               ADD 1 TO WS-BANKER-COUNT
                               MOVE IN-BANKER
                                   TO WS-BANKER-ENTRY(WS-BANKER-COUNT)
                       END-READ
                    END-PERFORM.
               CLOSE FD-BANKER.

               PERFORM SORT-BANK-TABLE.
            EXIT.
      *--------------------
           SORT-BANK-TABLE.
               PERFORM VARYING IDX FROM 1 BY 1 
                   UNTIL IDX > WS-BANKER-COUNT
                   PERFORM VARYING IDX2 FROM 1 BY 1
                       UNTIL IDX2 > WS-BANKER-COUNT
                       
                       IF REG-NR OF WS-BANKER-ENTRY(IDX) >
                           REG-NR OF WS-BANKER-ENTRY(IDX2)
                        THEN
                           PERFORM SWAP-BANKER
                       END-IF
                   END-PERFORM
               END-PERFORM.
            EXIT.
      *--------------------
           SWAP-BANKER.
               MOVE WS-BANKER-ENTRY(IDX) TO WS-TEMP-BANKER
               MOVE WS-BANKER-ENTRY(IDX2) TO WS-BANKER-ENTRY(IDX)
               MOVE WS-TEMP-BANKER TO WS-BANKER-ENTRY(IDX2)
            EXIT.
      *--------------------
           LOAD-TRANSACTIONERS-FILE.
               OPEN INPUT FD-TRANSACTION-IN.
                   PERFORM UNTIL WS-EOF-FLAG-TRANSACTION = "Y"
                       READ FD-TRANSACTION-IN
                           AT END
                               MOVE "Y" TO WS-EOF-FLAG-TRANSACTION
                           NOT AT END
                               MOVE IN-TRANSACTION TO SORT-REC
                               RELEASE SORT-REC
                       END-READ
                    END-PERFORM.
               CLOSE FD-TRANSACTION-IN.
            EXIT.
      *--------------------
           PROCESS-SORTED.
               OPEN OUTPUT FD-TRANSACTION-OUT.
               PERFORM UNTIL 1 = 2
                   RETURN SORT-FILE
                       AT END EXIT PERFORM 
                       NOT AT END

                           IF WS-CURRENT-KONTO = SPACES
                               MOVE KONTO-ID OF SORT-REC 
                                   TO WS-CURRENT-KONTO
                          
                               PERFORM PRINT-CUSTOMER
                           END-IF

                           IF KONTO-ID OF SORT-REC 
                               NOT = WS-CURRENT-KONTO
                           THEN
                               PERFORM PRINT-CUSTOMER-SALDO

                               MOVE 50000 TO WS-GROUP-SUM
                               MOVE 0 TO WS-GROUP-SUM-OUT
                               MOVE 0 TO WS-GROUP-SUM-IN

                               PERFORM PRINT-CUSTOMER

                               MOVE KONTO-ID OF SORT-REC 
                                   TO WS-CURRENT-KONTO
                           END-IF

                           PERFORM PRINT-CUSTOMER-TRANSACTIONS

                           PERFORM ADD-TO-GROUP-SUM

                   END-RETURN
               END-PERFORM.

      * Print saldo for last group
               PERFORM PRINT-CUSTOMER-SALDO
               
               CLOSE FD-TRANSACTION-OUT.
            EXIT.
      *--------------------
           PRINT-CUSTOMER.
               MOVE "-----------------------------------------------"
                   TO LINE-CONTENT
               PERFORM WRITE-LINE

               MOVE SPACES TO LINE-CONTENT

               STRING
                    "Kunde: " DELIMITED BY SIZE
                    NAVN OF SORT-REC DELIMITED BY SPACE
                    INTO LINE-CONTENT
               END-STRING
               PERFORM WRITE-LINE
               
               STRING
                    "Adresse: " DELIMITED BY SIZE
                    ADRESSE OF SORT-REC DELIMITED BY SPACE
                    INTO LINE-CONTENT
               END-STRING
               PERFORM WRITE-LINE

               PERFORM FIND-BANK

               MOVE SPACES TO LINE-CONTENT
               PERFORM WRITE-LINE
               PERFORM WRITE-LINE

               STRING
                    "Kontoudskrift for kontonr.: " DELIMITED BY SIZE
                    KONTO-ID OF SORT-REC DELIMITED BY SPACE
                    INTO LINE-CONTENT
               END-STRING
               PERFORM WRITE-LINE         
                                                       
               MOVE SPACES TO LINE-CONTENT
               PERFORM WRITE-LINE
           EXIT.
      *--------------------
           PRINT-CUSTOMER-TRANSACTIONS.
                STRING
                   "Dato: " DELIMITED BY SIZE
                   FUNCTION TRIM(TIDSPUNKT OF SORT-REC)
                       DELIMITED BY SPACE
                   ", " DELIMITED BY SIZE

                   "Transaktionstype: " DELIMITED BY SIZE
                   FUNCTION TRIM(TRANSAKTIONSTYPE OF SORT-REC)
                       DELIMITED BY SPACE
                   ", " DELIMITED BY SIZE

                   "Beløb: " DELIMITED BY SIZE
                   FUNCTION TRIM(BELØB OF SORT-REC)
                       DELIMITED BY SPACE
                   " " DELIMITED BY SIZE

                   FUNCTION TRIM(VALUTA OF SORT-REC)
                       DELIMITED BY SPACE
                   ", " DELIMITED BY SIZE

                   "Butik: " DELIMITED BY SIZE
                   FUNCTION TRIM(BUTIK OF SORT-REC)
                       DELIMITED BY SPACE

                   INTO LINE-CONTENT
               END-STRING
               
               PERFORM WRITE-LINE
           EXIT.
      *--------------------
           PRINT-CUSTOMER-SALDO.
               PERFORM PREP-SALDO-DISPLAY.

               MOVE SPACES TO LINE-CONTENT
               PERFORM WRITE-LINE
               PERFORM WRITE-LINE
               
               STRING
                   "Totalt indbetalt (DKK): " DELIMITED BY SIZE
                   FUNCTION TRIM(WS-GROUP-SUM-IN-DISPLAY)
                      DELIMITED BY SPACE
                   INTO LINE-CONTENT
               END-STRING
               PERFORM WRITE-LINE

               STRING
                   "Totalt udbetalt (DKK): " DELIMITED BY SIZE
                   FUNCTION TRIM(WS-GROUP-SUM-OUT-DISPLAY)
                      DELIMITED BY SPACE
                   INTO LINE-CONTENT
               END-STRING
               PERFORM WRITE-LINE
               
               STRING
                   "Saldo (DKK): " DELIMITED BY SIZE
                   FUNCTION TRIM(WS-GROUP-SUM-DISPLAY)
                      DELIMITED BY SPACE
                   INTO LINE-CONTENT
               END-STRING
               PERFORM WRITE-LINE

               MOVE SPACES TO LINE-CONTENT
               PERFORM WRITE-LINE
               PERFORM WRITE-LINE

               MOVE "Med venlig hilsen" TO LINE-CONTENT
               PERFORM WRITE-LINE

               MOVE "Asgers bank" TO LINE-CONTENT
               PERFORM WRITE-LINE

               MOVE SPACES TO LINE-CONTENT
               PERFORM WRITE-LINE
               PERFORM WRITE-LINE
               PERFORM WRITE-LINE
           EXIT.
      *--------------------
           PREP-SALDO-DISPLAY.
               MOVE WS-GROUP-SUM TO WS-GROUP-SUM-DISPLAY.
               MOVE WS-GROUP-SUM-IN TO WS-GROUP-SUM-IN-DISPLAY.
               MOVE WS-GROUP-SUM-OUT TO WS-GROUP-SUM-OUT-DISPLAY.

               IF WS-GROUP-SUM < 0
                   STRING
                       "-" DELIMITED BY SIZE
                       FUNCTION TRIM(WS-GROUP-SUM-DISPLAY(2: LENGTH 
                                       OF WS-GROUP-SUM-DISPLAY - 1))
                           DELIMITED BY SPACE

                       INTO WS-GROUP-SUM-DISPLAY
                   END-STRING
               ELSE
                   MOVE FUNCTION TRIM(WS-GROUP-SUM-DISPLAY)
                       TO WS-GROUP-SUM-DISPLAY
               END-IF

               IF WS-GROUP-SUM-IN < 0
                   STRING
                       "-" DELIMITED BY SIZE
                       FUNCTION TRIM(WS-GROUP-SUM-IN-DISPLAY(2: LENGTH 
                                       OF WS-GROUP-SUM-IN-DISPLAY - 1))
                           DELIMITED BY SPACE

                       INTO WS-GROUP-SUM-IN-DISPLAY
                   END-STRING
               ELSE
                   MOVE FUNCTION TRIM(WS-GROUP-SUM-IN-DISPLAY)
                       TO WS-GROUP-SUM-IN-DISPLAY
               END-IF

               IF WS-GROUP-SUM-OUT < 0
                   STRING
                       "-" DELIMITED BY SIZE
                       FUNCTION TRIM(WS-GROUP-SUM-OUT-DISPLAY(2: LENGTH 
                                       OF WS-GROUP-SUM-IN-DISPLAY - 1))
                           DELIMITED BY SPACE

                       INTO WS-GROUP-SUM-OUT-DISPLAY
                   END-STRING
               ELSE
                   MOVE FUNCTION TRIM(WS-GROUP-SUM-OUT-DISPLAY)
                       TO WS-GROUP-SUM-OUT-DISPLAY
               END-IF
            EXIT.
      *--------------------
           ADD-TO-GROUP-SUM.
               PERFORM CONVERT-VALUTA.

               ADD WS-CONVERTED-VALUTA-AMOUNT TO WS-GROUP-SUM

               IF TRANSAKTIONSTYPE OF SORT-REC = "Indbetaling" OR
                   TRANSAKTIONSTYPE OF SORT-REC = "Overfoersel"
               THEN
                   ADD WS-CONVERTED-VALUTA-AMOUNT
                       TO WS-GROUP-SUM-IN
               ELSE
                   ADD WS-CONVERTED-VALUTA-AMOUNT
                       TO WS-GROUP-SUM-OUT
               END-IF
           EXIT.
      *--------------------
           CONVERT-VALUTA.
               IF VALUTA OF SORT-REC = "USD"
                   COMPUTE WS-CONVERTED-VALUTA-AMOUNT = 
                       FUNCTION NUMVAL(BELØB OF SORT-REC) * 6,8
               ELSE IF VALUTA OF SORT-REC = "EUR"
                   COMPUTE WS-CONVERTED-VALUTA-AMOUNT = 
                       FUNCTION NUMVAL(BELØB OF SORT-REC) * 7,5
               ELSE
                   MOVE FUNCTION NUMVAL(BELØB OF SORT-REC) 
                       TO WS-CONVERTED-VALUTA-AMOUNT
               END-IF
           EXIT.
      *--------------------
           FIND-BANK.
               SET IDX TO 1.

               PERFORM UNTIL IDX > WS-BANKER-COUNT
                   IF REG-NR OF WS-BANKER-ENTRY(IDX) =
                       REG-NR OF SORT-REC(1:4)
                   THEN
                       MOVE SPACES TO LINE-CONTENT
                       PERFORM WRITE-LINE
                       
                       STRING
                           WS-SPACES DELIMITED BY SIZE
                           "Registreringsnummer: " DELIMITED BY SIZE
                           REG-NR OF WS-BANKER-ENTRY(IDX)
                              DELIMITED BY SPACE
                           INTO LINE-CONTENT
                       END-STRING
                       PERFORM WRITE-LINE
                       
                       STRING
                           WS-SPACES DELIMITED BY SIZE
                           "Bank: " DELIMITED BY SIZE
                           BANKNAVN OF WS-BANKER-ENTRY(IDX)
                              DELIMITED BY SPACE
                           INTO LINE-CONTENT
                       END-STRING
                       PERFORM WRITE-LINE
                       
                       STRING
                           WS-SPACES DELIMITED BY SIZE
                           "Bankadresse: " DELIMITED BY SIZE
                           BANKADRESSE OF WS-BANKER-ENTRY(IDX)
                              DELIMITED BY SPACE
                           INTO LINE-CONTENT
                       END-STRING
                       PERFORM WRITE-LINE

                       STRING
                           WS-SPACES DELIMITED BY SIZE
                           "Telefon: " DELIMITED BY SIZE
                           TELEFON OF WS-BANKER-ENTRY(IDX)
                              DELIMITED BY SPACE
                           INTO LINE-CONTENT
                       END-STRING
                       PERFORM WRITE-LINE
                       
                       STRING
                           WS-SPACES DELIMITED BY SIZE
                           "E-maiL: " DELIMITED BY SIZE
                           EMAIL OF WS-BANKER-ENTRY(IDX)
                              DELIMITED BY SPACE
                           INTO LINE-CONTENT
                       END-STRING
                       PERFORM WRITE-LINE

                       EXIT PERFORM
                   END-IF

                   SET IDX UP BY 1
               END-PERFORM.

               IF IDX > WS-BANKER-COUNT
                   MOVE "BANK NOT FOUND" TO LINE-CONTENT
                   PERFORM WRITE-LINE
               END-IF.
            EXIT.
      *--------------------
           WRITE-LINE.
               WRITE OUT-TRANSACTION FROM LINE-CONTENT
                   AFTER ADVANCING 1 LINE
           EXIT.
