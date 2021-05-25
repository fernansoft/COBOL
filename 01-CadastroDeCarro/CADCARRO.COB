       IDENTIFICATION DIVISION.
       PROGRAM-ID. CADCARRO.
       AUTHOR. FERNANDO OLIVEIRA DA COSTA.
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 PLACA           PIC X(08).
       01 PROPRIETARIO    PIC X(30).
       01 SITUACAO        PIC X(01).
       01 MARCA           PIC 9(01).
       01 ANOFABRICACAO   PIC 9(04).
       01 TEXMARCA        PIC X(13).
       01 IND             PIC 9(02).

       01 TABMARCA.
          03 TBMARCA    PIC X(13) OCCURS 9 TIMES.

       01 TABSITUACAOX.
          03 FILLER     PIC X(15) VALUE "MMANUTENCAO".
          03 FILLER     PIC X(15) VALUE "BBATIDO".
          03 FILLER     PIC X(15) VALUE "SSUCATA".
          03 FILLER     PIC X(15) VALUE "OORIGINAL".
          03 FILLER     PIC X(15) VALUE "NNORMAL".
      *
       01 TABSITUACAO REDEFINES TABSITUACAOX.
          03 TBSITUACAO   PIC X(15) OCCURS 5 TIMES.
      *
       01 TXTSITUACAO.
          03 TXTSITUACAO1 PIC X(01) VALUE SPACES.
          03 TXTSITUACAO2 PIC X(14) VALUE SPACES. 

      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELA2.
           05  BLANK SCREEN.
           05  LINE 03  COLUMN 26 VALUE  "**** CADASTRO DE CARRO ****".
           05  LINE 06  COLUMN 09 VALUE  "PLACA:".
           05  LINE 08  COLUMN 09 VALUE  "PROPRIETARIO:".
           05  LINE 10  COLUMN 09 VALUE  "ANO FABRICACAO:".
           05  LINE 12  COLUMN 09 VALUE  "SITUACAO:".
           05  LINE 14  COLUMN 09 VALUE  "MARCA:".
           05  LINE 22  COLUMN 04 VALUE  "MENSAGEM:".
           05  TPLACA
               LINE 06  COLUMN 17  PIC X(08)
               USING  PLACA
               HIGHLIGHT.
           05  TPROPRIETARIO
               LINE 08  COLUMN 24  PIC X(30)
               USING  PROPRIETARIO
               HIGHLIGHT.
           05  TANOFABRICACAO
               LINE 10  COLUMN 26  PIC X(04)
               USING  ANOFABRICACAO
               HIGHLIGHT.
           05  TSITUACAO
               LINE 12  COLUMN 20  PIC X(01)
               USING  SITUACAO
               HIGHLIGHT.
           05  TMARCA
               LINE 14  COLUMN 17  PIC X(01)
               USING  MARCA
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
           MOVE "Toyota"        TO TBMARCA(1)
           MOVE "Volkswagen"    TO TBMARCA(2)
           MOVE "Ford"          TO TBMARCA(3)
           MOVE "Honda"         TO TBMARCA(4)
           MOVE "Nissan"        TO TBMARCA(5)
           MOVE "Hyundai"       TO TBMARCA(6)
           MOVE "Chevrolet"     TO TBMARCA(7)
           MOVE "Mercedes-Benz" TO TBMARCA(8)
           MOVE "BMW"           TO TBMARCA(9).
      *
       R1.
           DISPLAY TELA2.
           ACCEPT TPLACA
           ACCEPT TPROPRIETARIO
           ACCEPT TANOFABRICACAO.
       R6.
           MOVE 1 TO IND
           ACCEPT TSITUACAO.

       R6A.
           MOVE TBSITUACAO(IND) TO TXTSITUACAO
           IF TXTSITUACAO1 NOT = SITUACAO
              ADD 1 TO IND
              IF IND < 5
                 GO TO R6A
              ELSE
                 DISPLAY (22, 16) "TIPO DE SITUACAO INCORRETA | "
                 STOP " DIGITE ENTER PARA CONTINUAR"
                 GO TO R6
           ELSE
               DISPLAY (12, 22) TXTSITUACAO2.

       R7.
           ACCEPT TMARCA.
           IF MARCA = 0 
                 DISPLAY (22, 15) "DIGITE APENAS DE 1 A 9 | "
                 STOP " DIGITE ENTER PARA CONTINUAR"
                 GO TO R7.
           MOVE TBMARCA(MARCA) TO TEXMARCA
           DISPLAY (14, 19) TEXMARCA.
           STOP RUN.
.