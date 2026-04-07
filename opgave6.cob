       PROGRAM-ID. opgave6.

       ENVIRONMENT DIVISION.
           INPUT-OUTPUT SECTION.
               FILE-CONTROL.
               SELECT CUSTOMER-FILE ASSIGN "customers.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
           FILE SECTION.
           FD CUSTOMER-FILE.
           01 KUNDEOPL-1.
               COPY "customer.cpy".

           WORKING-STORAGE SECTION.
           01 EOF-FLAG PIC X VALUE "N".
       
       PROCEDURE DIVISION.
           OPEN INPUT CUSTOMER-FILE.
               PERFORM UNTIL EOF-FLAG = "Y"
                   READ CUSTOMER-FILE INTO KUNDEOPL-1
                       AT END
                           MOVE "Y" TO EOF-FLAG
                       NOT AT END
                           DISPLAY "Kunde ID: " KUNDE-ID
                           DISPLAY "Name: " FIRST-NAME LAST-NAME 
                           DISPLAY "Konto nummer: " KONTO-NUMMER
                           DISPLAY "Balance: " BALANCE " " VALUTA-CODE
                   END-READ
               END-PERFORM.
            CLOSE CUSTOMER-FILE.
       STOP RUN.
