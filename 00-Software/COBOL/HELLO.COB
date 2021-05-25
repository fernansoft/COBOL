       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
      *AUTOR. ANGELO LOTIERZO FILHO.
      *******************************************
      * MANUTENCAO DO PROGRAMA 0  *
      *******************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                         DECIMAL-POINT IS COMMA.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 APELIDO     PIC X(12) VALUE "FERNANDO".
       01 NOME          PIC X(30) VALUE SPACES.
       01 IDADE          PIC 9(03) VALUE ZEROS.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
           MOVE "FERNANDO OLIVEIRA" TO NOME
           DISPLAY (01 , 01) "NOME: "
           DISPLAY (01, 07) NOME
           DISPLAY (02, 01) "APELIDO: "
           DISPLAY (02, 10) APELIDO
           DISPLAY (03, 01) "IDADE: "
           DISPLAY (03, 08) IDADE

.          STOP RUN.
