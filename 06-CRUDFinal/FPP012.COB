       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP012.
       AUTHOR. FERNANDO OLIVEIRA DA COSTA.
      **************************************************
      * CONSULTA CARGOS*
      **************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *
       SELECT ARQCARG ASSIGN TO DISK
                   ORGANIZATION IS INDEXED
                   ACCESS MODE  IS DYNAMIC
                   RECORD KEY   IS CODCARGO
                   FILE STATUS  IS ST-ERRO
                   ALTERNATE RECORD KEY IS NOMECARGO WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
      *
       FD ARQCARG
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQCARG.DAT". 
       01 REGCARG.
           03 CODCARGO         PIC 9(3).
           03 NOMECARGO        PIC X(25).
           03 TIPOSALARIOAUX   PIC X(1).
           03 TIPOSALARIO.
               05 TIPOSALARIO1 PIC X(1).
               05 TIPOSALARIO2 PIC X(14).
           03 STATUSCAUX       PIC X.
           03 STATUSC.
               05 STATUSC1     PIC X.
               05 STATUSC2     PIC X(15).
           03 SALARIO          PIC 9(06)V99.
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-SEL            PIC 9(01) VALUE ZEROS.
       01 W-CONT           PIC 9(04) VALUE ZEROS.
       01 W-OPCAO          PIC X(01) VALUE SPACES.
       01 ST-ERRO          PIC X(02) VALUE "00".
       01 W-ACT            PIC 9(02) VALUE ZEROS.
       01 MENS             PIC X(50) VALUE SPACES.
       01 LIMPA            PIC X(55) VALUE SPACES.
       01 SOLIC            PIC X(20) VALUE SPACES.
       01 CONLIN           PIC 9(03) VALUE 001.
       01 CODCARGOENTR          PIC 9(09) VALUE ZEROS.
       01 NOMEDEPENTR         PIC X(30) VALUE SPACES.
      *-----------------------------------------------------------------
      *
      *************************
      * REPORT SECTION *
      *************************
      *
       01 DETALHE.
           03 DETCODCARGO       PIC 999.
           03 FILLER            PIC X(04) VALUE SPACES.
           03 DETNOME           PIC X(25) VALUE SPACES.
           03 FILLER            PIC X(01) VALUE SPACES.
           03 DETTIPOSALARIO    PIC X(15) VALUE SPACES.
           03 FILLER            PIC X(01) VALUE SPACES.  
           03 DETSALARIO        PIC ZZZ.ZZ9,99 VALUE ZEROS.        
           03 FILLER            PIC X(02) VALUE SPACES.
           03 DETSTATUS         PIC X(16) VALUE SPACES.
      
      *
      ******************
      * ROTINA DE TELA *
      ******************
      *
       SCREEN SECTION.
       01  TELA.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                          *** CONSULTA D".
           05  LINE 02  COLUMN 41 
               VALUE  "E CARGOS ***".
           05  LINE 03  COLUMN 01 
               VALUE  "----------------------------------------".
           05  LINE 03  COLUMN 41 
               VALUE  "----------------------------------------".
           05  LINE 04  COLUMN 02 
               VALUE  "CODIGO         NOMECARGO".
           05  LINE 04  COLUMN 37 
               VALUE  "TIPOSALARIO     SALARIO       STATUS".
           05  LINE 05  COLUMN 02 
               VALUE  "------ -------------------------".
           05  LINE 05  COLUMN 34 
               VALUE  " --------------- ----------- ----------------".
           05  LINE 22  COLUMN 01 
               VALUE  "----------------------------------------".
           05  LINE 22  COLUMN 41 
               VALUE  "----------------------------------------".
           05  LINE 23  COLUMN 01 
               VALUE  "MENSAGEM :".
           05  LINE 23  COLUMN 41 
               VALUE  "                                 FPP012".
           05  TCODCARGOENTR
               LINE 06  COLUMN 02  PIC 999
               USING  CODCARGOENTR
               HIGHLIGHT.
           05  TNOMEDEPENTR
               LINE 06  COLUMN 9  PIC X(30)
               USING  NOMEDEPENTR
               HIGHLIGHT.
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP1.
           OPEN INPUT  ARQCARG
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE ARQCARG NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                IF ST-ERRO = "95"
                    MOVE "ISAM NAO CARREGADO" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-FIM2
                ELSE
                  MOVE "ERRO ABERTURA DO ARQUIVO ARQCARG"  TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM2.
      *
       INC-001.
           MOVE ZEROS TO CODCARGOENTR
           MOVE SPACES TO NOMEDEPENTR
           DISPLAY  TELA.
       INC-001A.
           DISPLAY (23, 13)
            "SOLIC.=> F1=ENCERRA CONSULTA   F2=CONSULTA P/NOMECARGO."
           ACCEPT TCODCARGOENTR
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE ARQCARG
                      GO TO ROT-FIM.
           IF W-ACT = 03
                      GO TO INC-001B.
           IF W-ACT > 03
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
           MOVE CODCARGOENTR TO CODCARGO
           START ARQCARG KEY IS NOT LESS CODCARGO INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001.
           GO TO INC-RD2.
       INC-001B.
           DISPLAY (23, 13)
            "SOLIC.=> F1=ENCERRA CONSULTA    F2=CONSULTA P/CODIGO CARGO"
           ACCEPT TNOMEDEPENTR
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE ARQCARG
                      GO TO ROT-FIM.
           IF W-ACT = 03
                      GO TO INC-001A.
           IF W-ACT > 03
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
           MOVE NOMEDEPENTR TO NOMECARGO
           START ARQCARG KEY IS NOT LESS NOMECARGO INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001B.
       INC-RD2.
           DISPLAY (23, 13) LIMPA
           READ ARQCARG NEXT
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "10"
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NOV-SOL
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO ARQCARG"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.
       ROT-MONTAR.
           MOVE CODCARGO       TO DETCODCARGO
           MOVE NOMECARGO      TO DETNOME
           MOVE TIPOSALARIO    TO DETTIPOSALARIO
           MOVE SALARIO        TO DETSALARIO
           MOVE STATUSC        TO DETSTATUS 
           COMPUTE LIN = CONLIN + 6
           DISPLAY (LIN, 02) DETALHE
           ADD 1 TO CONLIN
           IF CONLIN < 16
                  GO TO INC-RD2.
      *
       ROT-SOL.
           DISPLAY (23, 13) " *** CONTINUA CONSULTA : . ***"
           MOVE "S" TO W-OPCAO
           ACCEPT  (23, 38) W-OPCAO  WITH UPDATE
           IF W-OPCAO = "S"
                  MOVE 01 TO CONLIN
                  DISPLAY TELA
                  GO TO INC-RD2.
           IF W-OPCAO NOT = "N"
                  MOVE "*** DIGITE S=SIM   N=NAO ***" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-SOL.
           MOVE 01 TO CONLIN.
           GO TO INC-001.
      *
       NOV-SOL.
           DISPLAY (23, 13) " *** NOVA CONSULTA : . ***"
           MOVE "S" TO W-OPCAO
           ACCEPT  (23, 34) W-OPCAO WITH UPDATE
           IF W-OPCAO = "S"
                  MOVE 01 TO CONLIN
                  GO TO INC-001.
           IF W-OPCAO NOT = "N"
                  MOVE "*** DIGITE S=SIM   N=NAO ***" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO NOV-SOL.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
           DISPLAY (01 01) ERASE.
           CLOSE ARQCARG.
       ROT-FIM2.
           EXIT PROGRAM.
       ROT-FIM3.
           STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
           MOVE ZEROS TO W-CONT.
       ROT-MENS1.
           DISPLAY (23, 13) MENS.
       ROT-MENS2.
           ADD 1 TO W-CONT
           IF W-CONT < 1000
              GO TO ROT-MENS2
           ELSE
              DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
           EXIT.
       FIM-ROT-MENS.
      *

      *---------------------*** FIM DE PROGRAMA ***--------------------*