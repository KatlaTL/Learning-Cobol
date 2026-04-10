       PROGRAM-ID. opgave10.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
                   SELECT FD-BANKER ASSIGN "opgave10/Banker.txt"
                   ORGANIZATION IS LINE SEQUENTIAL.
           
                   SELECT FD-TRANSACTION
                       ASSIGN "opgave10/Transaktioner.txt"
                   ORGANIZATION IS LINE SEQUENTIAL.
        
       DATA DIVISION.
           FILE SECTION.
               FD FD-BANKER.
               01 IN-BANKER.
                   COPY "opgave10/banker.cpy".
        
               FD FD-TRANSACTION.
               01 IN-TRANSACTION.
                   COPY "opgave10/transactions.cpy".

            WORKING-STORAGE SECTION.
               01 WS-BANKER-COUNT PIC 9(3) COMP VALUE 0.
               01 WS-BANKER-ENTRY OCCURS 0 TO 300 TIMES
                       DEPENDING ON WS-BANKER-COUNT.
                   COPY "opgave10/banker.cpy".
               01 WS-EOF-FLAG-BANKER PIC X VALUE "N".
               01 WS-BANKER-IX PIC 9(3) VALUE 1.

               01 WS-TRANSACTION-COUNT PIC 9(5) COMP VALUE 0.
               01 WS-TRANSACTION-ENTRY OCCURS 0 TO 60000 TIMES
                       DEPENDING ON WS-TRANSACTION-COUNT.
                   COPY "opgave10/transactions.cpy".
               01 WS-EOF-FLAG-TRANSACTION PIC X VALUE "N".
               01 WS-TRANSACTION-IX PIC 9(5) VALUE 1.
                
       
       PROCEDURE DIVISION.
       
           MAIN-PROCESS.
               PERFORM INIT

               STOP RUN.



      *--------------------
           INIT.
               OPEN INPUT FD-BANKER.
               OPEN INPUT FD-TRANSACTION.

               PERFORM LOAD-BANKER-FILE.
               PERFORM LOAD-TRANSACTIONERS-FILE.

               CLOSE FD-BANKER.
               CLOSE FD-TRANSACTION.
            EXIT.
      *--------------------
           LOAD-BANKER-FILE.
               PERFORM UNTIL WS-EOF-FLAG-BANKER = "Y"
                   READ FD-BANKER
                       AT END
                           MOVE "Y" TO WS-EOF-FLAG-BANKER
                       NOT AT END
                           MOVE IN-BANKER
                               TO WS-BANKER-ENTRY(WS-BANKER-IX)
                           ADD 1 TO WS-BANKER-IX
                   END-READ
                END-PERFORM.
            EXIT.
      *--------------------
           LOAD-TRANSACTIONERS-FILE.
               PERFORM UNTIL WS-EOF-FLAG-TRANSACTION = "Y"
                   READ FD-TRANSACTION
                       AT END
                           MOVE "Y" TO WS-EOF-FLAG-TRANSACTION
                       NOT AT END
                           MOVE IN-TRANSACTION
                           TO WS-TRANSACTION-ENTRY(WS-TRANSACTION-IX)

                           ADD 1 TO WS-TRANSACTION-IX
                   END-READ
                END-PERFORM.
            EXIT.
      *--------------------
