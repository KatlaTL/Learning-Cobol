           
               05 KUNDE-ID PIC X(10) VALUE SPACES.
               05 FIRST-NAME PIC X(20) VALUE SPACES.
               05 LAST-NAME PIC X(20) VALUE SPACES.
               05 TELEFON PIC X(8).
               05 EMAIL PIC X(50).
               05 ADDRESSE.
                   10 VEJNAVN PIC X(30).
                   10 HUSNR PIC X(5).
                   10 ETAGE PIC X(5).
                   10 SIDE PIC X(5).
                   10 BY-X PIC X(20).
                   10 POSTNR PIC X(4).
                   10 LANDE-KODE PIC X(2).
               05 KONTO-INFO. 
                   10 KONTO-NUMMER PIC X(20) VALUE SPACES.
                   10 BALANCE PIC 9(7)V99 VALUE ZEROS.
                   10 VALUTA-CODE PIC X(3) VALUE SPACES.
