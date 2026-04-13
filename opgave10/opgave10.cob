       PROGRAM-ID. opgave10.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                   SELECT FD-BANKER ASSIGN "opgave10/Banker.txt"
                   ORGANIZATION IS LINE SEQUENTIAL.
           
                   SELECT FD-TRANSACTION
                       ASSIGN "opgave10/Transaktioner.txt"
                   ORGANIZATION IS LINE SEQUENTIAL.

                   SELECT SORT-FILE ASSIGN TO WRK.
        
       DATA DIVISION.
           FILE SECTION.
               FD FD-BANKER.
               01 IN-BANKER.
                   COPY "opgave10/banker.cpy".
        
               FD FD-TRANSACTION.
               01 IN-TRANSACTION.
                   COPY "opgave10/transactions.cpy".
               
               SD SORT-FILE.
               01 SORT-REC.
                   COPY "opgave10/transactions.cpy".

            WORKING-STORAGE SECTION.
               01 WS-BANKER-COUNT PIC 9(3) COMP VALUE 0.
               01 WS-BANKER-ENTRY OCCURS 0 TO 300 TIMES
                       DEPENDING ON WS-BANKER-COUNT
                       INDEXED BY IDX IDX2.
                   COPY "opgave10/banker.cpy".
               01 WS-TEMP-BANKER.
                   COPY "opgave10/banker.cpy".
               01 WS-EOF-FLAG-BANKER PIC X VALUE "N".
               01 WS-EOF-FLAG-TRANSACTION PIC X VALUE "N".
               01 WS-CURRENT-KONTO PIC X(15) VALUE SPACES.
               01 WS-GROUP-SUM PIC S9(11)V99 COMP-3 VALUE 50000.
               01 WS-GROUP-SUM-IN PIC S9(11)V99 COMP-3 VALUE 0.
               01 WS-GROUP-SUM-OUT PIC S9(11)V99 COMP-3 VALUE 0.
               01 WS-SPACES PIC X(45) VALUE SPACES.
               01 WS-LINE PIC X(150).
                
       
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
               OPEN INPUT FD-TRANSACTION.
                   PERFORM UNTIL WS-EOF-FLAG-TRANSACTION = "Y"
                       READ FD-TRANSACTION
                           AT END
                               MOVE "Y" TO WS-EOF-FLAG-TRANSACTION
                           NOT AT END
                               MOVE IN-TRANSACTION TO SORT-REC
                               RELEASE SORT-REC
                       END-READ
                    END-PERFORM.
               CLOSE FD-TRANSACTION.
            EXIT.
      *--------------------
           PROCESS-SORTED.
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
            EXIT.
      *--------------------
           PRINT-CUSTOMER.
               DISPLAY "-----------------------------------------------"
               DISPLAY "Kunde: " NAVN OF SORT-REC
               DISPLAY "Adresse: " ADRESSE OF SORT-REC

               PERFORM FIND-BANK

               DISPLAY SPACES
               DISPLAY SPACES

               DISPLAY "Kontoudskrift for kontonr.: " KONTO-ID 
                                                       OF SORT-REC
               DISPLAY SPACES
           EXIT.
      *--------------------
           PRINT-CUSTOMER-TRANSACTIONS.
                STRING
                   "Dato: " DELIMITED BY SIZE
                   FUNCTION TRIM(TIDSPUNKT OF SORT-REC)
                       DELIMITED BY SPACE
                   ", " DELIMITED BY SIZE

                   "Transaktionstype: " DELIMITED BY SIZE
                   FUNCTION  TRIM(TRANSAKTIONSTYPE OF SORT-REC)
                       DELIMITED BY SPACE
                   ", " DELIMITED BY SIZE

                   "Beløb: " DELIMITED BY SIZE
                   FUNCTION  TRIM(BELØB OF SORT-REC)
                       DELIMITED BY SPACE
                   " " DELIMITED BY SIZE

                   FUNCTION  TRIM(VALUTA OF SORT-REC)
                       DELIMITED BY SPACE
                   ", " DELIMITED BY SIZE

                   "Butik: " DELIMITED BY SIZE
                   FUNCTION  TRIM(BUTIK OF SORT-REC)
                       DELIMITED BY SPACE

                   INTO WS-LINE
               END-STRING

               DISPLAY WS-LINE
           EXIT.
      *--------------------
           PRINT-CUSTOMER-SALDO.
               DISPLAY SPACES
               DISPLAY SPACES
               DISPLAY "Totalt indbetalt (DKK): " WS-GROUP-SUM-IN
               DISPLAY "Totalt udbetalt (DKK): " WS-GROUP-SUM-OUT
               DISPLAY "Saldo (DKK): " WS-GROUP-SUM

               DISPLAY SPACES
               DISPLAY SPACES
               DISPLAY "Med venlig hilse"
               DISPLAY "Asgers bank"
               DISPLAY SPACES
               DISPLAY SPACES
               DISPLAY SPACES
           EXIT.
      *--------------------
           ADD-TO-GROUP-SUM.
               ADD FUNCTION NUMVAL(BELØB OF SORT-REC) TO WS-GROUP-SUM

               IF TRANSAKTIONSTYPE OF SORT-REC = "Indbetaling" OR
                   TRANSAKTIONSTYPE OF SORT-REC = "Overfoersel"
               THEN
                   ADD FUNCTION NUMVAL(BELØB OF SORT-REC)
                       TO WS-GROUP-SUM-IN
               ELSE
                   ADD FUNCTION NUMVAL(BELØB OF SORT-REC)
                       TO WS-GROUP-SUM-OUT
               END-IF
           EXIT.
      *--------------------
           FIND-BANK.
               SET IDX TO 1.

               PERFORM UNTIL IDX > WS-BANKER-COUNT
                   IF REG-NR OF WS-BANKER-ENTRY(IDX) =
                       REG-NR OF SORT-REC(1:4)
                   THEN
                       DISPLAY SPACES
                       DISPLAY WS-SPACES "Registreringsnummer: " REG-NR
                               OF WS-BANKER-ENTRY(IDX)
                       DISPLAY WS-SPACES "Bank: " BANKNAVN
                               OF WS-BANKER-ENTRY(IDX)
                       DISPLAY WS-SPACES "Bankadresse: " BANKADRESSE 
                               OF WS-BANKER-ENTRY(IDX)
                       DISPLAY WS-SPACES "Telefon: " TELEFON
                               OF WS-BANKER-ENTRY(IDX)
                       DISPLAY WS-SPACES "E-maiL: " EMAIL
                               OF WS-BANKER-ENTRY(IDX)                      
                       EXIT PERFORM
                   END-IF

                   SET IDX UP BY 1
               END-PERFORM.

               IF IDX > WS-BANKER-COUNT
                   DISPLAY "BANK NOT FOUND"
               END-IF.
            EXIT.
               