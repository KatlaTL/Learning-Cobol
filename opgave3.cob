       PROGRAM-ID. opgave3.

       DATA DIVISION.
           WORKING-STORAGE SECTION.
           01 KUNDE-ID PIC X(10) VALUE SPACES.
           01 FIRST-NAME PIC X(20) VALUE SPACES.
           01 LAST-NAME PIC X(20) VALUE SPACES.
           01 FULL-NAME PIC X(40) VALUE SPACES.
           01 KONTO-NUMMER PIC X(20) VALUE SPACES.
           01 BALANCE PIC 9(7)V99 VALUE ZEROS.
           01 BALANCE-DISP PIC Z(7).99.
           01 VALUTA-CODE PIC X(3) VALUE SPACES.    
            
           01 FULL-NAME-WITHOUT-SPACES PIC X(40) VALUE SPACES.
           01 IX PIC 99 VALUE ZERO.            
           01 OUTPUT-IX PIC 99 VALUE ZERO.            
           01 CURRENT-CHAR PIC X(1).
           01 PREVIOUS-CHAR PIC X(1).
            
       PROCEDURE DIVISION.
           MOVE "1234567890" TO KUNDE-ID.
           MOVE "Lars" TO FIRST-NAME.
           MOVE "Hansen" TO LAST-NAME.
           MOVE "DK12345678912345" TO KONTO-NUMMER.
           Move 2500.75 TO BALANCE.
           MOVE "DKK" TO VALUTA-CODE.
           MOVE BALANCE TO BALANCE-DISP.

           STRING FIRST-NAME DELIMITED BY SIZE
                   " " DELIMITED BY SIZE 
                   LAST-NAME DELIMITED BY SIZE
                   INTO FULL-NAME
           END-STRING.

           PERFORM VARYING IX FROM 1 BY 1 UNTIL IX > LENGTH OF FULL-NAME
               MOVE FULL-NAME(IX: 1) TO CURRENT-CHAR

      * The conditions are read from left to right, which is why the OR clause works here
               IF CURRENT-CHAR NOT = SPACE OR PREVIOUS-CHAR NOT= SPACE  
                   ADD 1 TO OUTPUT-IX
                   MOVE FULL-NAME(IX: 1) 
                       TO FULL-NAME-WITHOUT-SPACES(OUTPUT-IX: 1)
               END-IF

               MOVE CURRENT-CHAR TO PREVIOUS-CHAR
           END-PERFORM.

           DISPLAY "----------------------------------------"
           DISPLAY "Kunde ID : " KUNDE-ID.
           DISPLAY "Navn : " FULL-NAME-WITHOUT-SPACES.
           DISPLAY "Kontonummer : " KONTO-NUMMER.
           DISPLAY "Balance : " FUNCTION TRIM(BALANCE-DISP) " "
                VALUTA-CODE.  
           DISPLAY "----------------------------------------"

       STOP RUN.
