       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPPMEN.
       AUTHOR. FERNANDO OLIVEIRA DA COSTA.
      **********************************************************
      * MENU FOLHA DE PAGAMENTO *
      **********************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                    DECIMAL-POINT IS COMMA.
      *-----------------------------------------------------------------
       DATA DIVISION.
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-CONT          PIC 9(04) VALUE ZEROS.
       01 W-OPCAO       PIC 9(02) VALUE ZEROS.
       01 W-ACT         PIC 9(02) VALUE ZEROS.
       01 MENS              PIC X(50) VALUE SPACES.
       01 LIMPA              PIC X(50) VALUE SPACES.
       01 W-PROGR      PIC X(07) VALUE SPACES.

      *-----------------------------------------------------------------
       01 TAB-PROGR-R.
           03 FILLER PIC X(07) VALUE "FPP001".
           03 FILLER PIC X(07) VALUE "FPP002".
           03 FILLER PIC X(07) VALUE "FPP003".
           03 FILLER PIC X(07) VALUE "FPP004".
           03 FILLER PIC X(07) VALUE "FPP011".
           03 FILLER PIC X(07) VALUE "FPP012".
           03 FILLER PIC X(07) VALUE "FPP013".
           03 FILLER PIC X(07) VALUE "FPP014".
           03 FILLER PIC X(07) VALUE "FPP015".

      *
       01 TAB-PROGR   REDEFINES TAB-PROGR-R.
          03 TAB-PROG  PIC X(07) OCCURS 9 TIMES.
      **********************
      *
      ******************
      * ROTINA DE TELA *
      ******************
      *
       SCREEN SECTION.
       01  SMTMEN.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                             SISTEMA DE".
           05  LINE 02  COLUMN 41 
               VALUE  "FOLHA DE PAGAMENTOS".
           05  LINE 05  COLUMN 01 
               VALUE  "                          1 - CADASTRO D".
           05  LINE 05  COLUMN 41 
               VALUE  "E DEPARTAMENTOS".
	       05  LINE 06  COLUMN 01 
               VALUE  "                          2 - CADASTRO D".
           05  LINE 06  COLUMN 41 
               VALUE  "E CARGOS".
	       05  LINE 07  COLUMN 01 
               VALUE  "                          3 - CADASTRO D".
           05  LINE 07  COLUMN 41 
               VALUE  "E DEPENDENTES".
	       05  LINE 08  COLUMN 01 
               VALUE  "                          4 - CADASTRO D".
           05  LINE 08  COLUMN 41 
               VALUE  "E FUNCIONARIOS".
           05  LINE 09  COLUMN 01 
               VALUE  "                          5 - CONSULTA D".
           05  LINE 09  COLUMN 41 
               VALUE  "E DEPARTAMENTOS".
           05  LINE 10  COLUMN 01 
               VALUE  "                          6 - CONSULTA D".
           05  LINE 10  COLUMN 41 
               VALUE  "E CARGOS".
           05  LINE 11  COLUMN 01 
               VALUE  "                          7 - CONSULTA D".
           05  LINE 11  COLUMN 41 
               VALUE  "E DEPENDENTES".
           05  LINE 12  COLUMN 01 
               VALUE  "                          8 - CONSULTA D".
           05  LINE 12  COLUMN 41 
               VALUE  "E FUNCIONARIOS".
           05  LINE 13  COLUMN 01 
               VALUE  "                          9 - RELATORIO D".
           05  LINE 13  COLUMN 42 
               VALUE  "E FOLHA DE PAGAMENTO".  
                                      
           05  LINE 20  COLUMN 01 
               VALUE  "                             OPCAO :".
           05  LINE 20  COLUMN 41 
               VALUE  " ( 00 - ENCERRA )".
           05  LINE 23  COLUMN 01 
               VALUE  " MENSAGEM :".
           05  T-OPCAO
               LINE 20  COLUMN 38  PIC 9(02)
               USING  W-OPCAO
               HIGHLIGHT.
       
       01  TELALIMPA.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 10 
               VALUE  "                                               ".
           05  LINE 02  COLUMN 51 
               VALUE  "                                               ".
           05  LINE 05  COLUMN 01 
               VALUE  "                                               ".
           05  LINE 05  COLUMN 41 
               VALUE  "                                               ".       
           05  LINE 20  COLUMN 01 
               VALUE  "                                               ".
           05  LINE 20  COLUMN 41 
               VALUE  "                                               ".
           05  LINE 23  COLUMN 01 
               VALUE  "                                               ".
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.

      ****************************
      * ROTINA DE SELECAO NIVEL1 *
      ****************************
      *
       NIVEL1.
           DISPLAY SMTMEN.
           
           ACCEPT T-OPCAO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
           IF W-OPCAO = 00
                      GO TO ROT-FIM.             
           IF W-OPCAO > 10 
                 MOVE "*** FUNCAO NAO DISPONIVEL *** " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NIVEL1.
           MOVE TAB-PROG(W-OPCAO) TO W-PROGR
           IF W-PROGR = SPACES
                 MOVE "*** FUNCAO NAO IMPLEMENTADA *** " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NIVEL1.
      *
       ROT-EXEC.
           DISPLAY TELALIMPA.
           CALL W-PROGR ON OVERFLOW
              MOVE "* ERRO NA CHAMADA DO PROGRAMA SELECIONADO *" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO NIVEL1.
           CANCEL W-PROGR.
           GO TO NIVEL1.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
           EXIT PROGRAM.
       ROT-FIM1.
           DISPLAY (01, 01) ERASE.
           STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
           MOVE ZEROS TO W-CONT.
       ROT-MENS1.
           DISPLAY (23 12) MENS.
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 3000
              GO TO ROT-MENS2
           ELSE
              DISPLAY (23 12) LIMPA.
       ROT-MENS-FIM.
      *
      *---------------------*** FIM DE PROGRAMA ***--------------------*