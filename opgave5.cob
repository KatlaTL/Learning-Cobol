       PROGRAM-ID. opgave5.

       DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 KUNDEOPL-1.
               COPY "customer.cpy".
           01 KUNDEOPL-2.
               COPY "customer.cpy".
       
       PROCEDURE DIVISION.
           MOVE "1234567890" TO KUNDE-ID OF KUNDEOPL-1.
           MOVE "Lars" TO FIRST-NAME OF KUNDEOPL-1.
           MOVE "Hansen" TO LAST-NAME OF KUNDEOPL-1.
           MOVE "DK12345678912345" TO KONTO-NUMMER OF KUNDEOPL-1.
           Move 2500.75 TO BALANCE OF KUNDEOPL-1.
           MOVE "DKK" TO VALUTA-CODE OF KUNDEOPL-1.

           DISPLAY "Kunde ID: " KUNDE-ID OF KUNDEOPL-1.
           DISPLAY "Name: " FIRST-NAME OF KUNDEOPL-1 
               LAST-NAME OF KUNDEOPL-1.
           DISPLAY "Konto nummer: " KONTO-NUMMER OF KUNDEOPL-1.
           DISPLAY "Balance: " BALANCE OF KUNDEOPL-1 " " 
               VALUTA-CODE OF KUNDEOPL-1.

           DISPLAY KUNDEOPL-1.
       STOP RUN.
