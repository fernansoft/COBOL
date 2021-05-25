       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROG1303.
       AUTHOR. FERNANDO OLIVEIRA DA COSTA.
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCARRO ASSIGN TO DISK
                    ORGANIZATION         IS INDEXED
                    ACCESS MODE          IS DYNAMIC
                    RECORD KEY           IS PLACA
                    ALTERNATE RECORD KEY IS PROPRIETARIO WITH DUPLICATES
                    FILE STATUS          IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCARRO
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCARRO.DAT". 
       01 REGCARRO.
          03 PLACA             PIC X(08).
          03 PROPRIETARIO      PIC X(30).
          03 ANOFABRICACAO     PIC 9(04).
          03 SITUACAO          PIC X(01).
          03 MARCA             PIC 9(01).
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-CONT       PIC 9(06) VALUE ZEROS.
       77 W-OPCAO      PIC X(01) VALUE SPACES.
       77 W-ACT        PIC 9(02) VALUE ZEROS.
       77 MENS         PIC X(50) VALUE SPACES.
       77 LIMPA        PIC X(50) VALUE SPACES. 
       01 ST-ERRO      PIC X(02) VALUE "00".
       01 W-SEL        PIC 9(01) VALUE ZEROS.
       01 TXTMARCA     PIC X(13) VALUE SPACES.
       01 IND          PIC 9(02) VALUE ZEROS.

       01 TABMARCA.
          03 TBMARCA    PIC X(13) OCCURS 9 TIMES.

       01 TABSITUACAOX.
          03 FILLER     PIC X(15) VALUE "MMANUTENCAO".
          03 FILLER     PIC X(15) VALUE "BBATIDO".
          03 FILLER     PIC X(15) VALUE "SSUCATA".
          03 FILLER     PIC X(15) VALUE "OORIGINAL".
          03 FILLER     PIC X(15) VALUE "NNORMAL".
       01 TABSITUACAO REDEFINES TABSITUACAOX.
          03 TBSITUACAO   PIC X(15) OCCURS 5 TIMES.
       01 TXTSITUACAO.
          03 TXTSITUACAO1 PIC X(01) VALUE SPACES.
          03 TXTSITUACAO2 PIC X(14) VALUE SPACES. 

      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELA2.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 28 VALUE  "*** CADASTRO DE CARROS ***".
           05  LINE 05  COLUMN 04 VALUE  "PLACA :".
           05  LINE 07  COLUMN 04 VALUE  "PROPRIETARIO :".
           05  LINE 09  COLUMN 04 VALUE  "ANOFABRICACAO :".
           05  LINE 11  COLUMN 04 VALUE  "SITUACAO :".
           05  LINE 13  COLUMN 04 VALUE  "MARCA :".
           05  LINE 23  COLUMN 01 VALUE  "MENSAGEM :".
           05  TPLACA
               LINE 05  COLUMN 14  PIC X(08)
               USING  PLACA
               HIGHLIGHT.
           05  TPROPRIETARIO
               LINE 07  COLUMN 20  PIC X(30)
               USING  PROPRIETARIO
               HIGHLIGHT.
           05  TANOFABRICACAO
               LINE 09  COLUMN 21  PIC X(04)
               USING  ANOFABRICACAO
               HIGHLIGHT.
           05  TSITUACAO
               LINE 11  COLUMN 16  PIC X(01)
               USING  SITUACAO
               HIGHLIGHT.
           05  TTXTSITUACAO2
               LINE 11  COLUMN 18  PIC X(14)
               USING  TXTSITUACAO2
               HIGHLIGHT.
           05  TMARCA
               LINE 13  COLUMN 13  PIC 9
               USING  MARCA
               HIGHLIGHT.
           05  TTXTMARCA
               LINE 13  COLUMN 15  PIC X(13)
               USING  TXTMARCA
               HIGHLIGHT.
      *
       01  TELAMARCA.
           05  LINE 14  COLUMN 41 VALUE  "1-TOYOTA".
           05  LINE 15  COLUMN 41 VALUE  "2-VOLKSWAGEN".
           05  LINE 16  COLUMN 41 VALUE  "3-FORD".
           05  LINE 17  COLUMN 41 VALUE  "4-HONDA".
           05  LINE 18  COLUMN 41 VALUE  "5-NISSAN".
           05  LINE 19  COLUMN 41 VALUE  "6-HYUNDAI".
           05  LINE 20  COLUMN 41 VALUE  "7-CHEVROLET".
           05  LINE 21  COLUMN 41 VALUE  "8-MERCEDES-BENZ".
           05  LINE 22  COLUMN 41 VALUE  "9-BMW".
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
           MOVE "TOYOTA"        TO TBMARCA(1)
           MOVE "VOLKSWAGEN"    TO TBMARCA(2)
           MOVE "FORD"          TO TBMARCA(3)
           MOVE "HONDA"         TO TBMARCA(4)
           MOVE "NISSAN"        TO TBMARCA(5)
           MOVE "HYUNDAI"       TO TBMARCA(6)
           MOVE "CHEVROLET"     TO TBMARCA(7)
           MOVE "MERCEDES-BENZ" TO TBMARCA(8)
           MOVE "BMW"           TO TBMARCA(9).
       
       R0.
           OPEN I-O CADCARRO
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADCARRO
                 CLOSE CADCARRO
                 MOVE "*** ARQUIVO CADCARRO FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADMIGO" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      * 
      *------------[ INICIALIZACAO DAS VARIAVEIS ]---------------------               
       R1.
           MOVE SPACES TO PLACA PROPRIETARIO SITUACAO
           MOVE SPACES TO TXTSITUACAO TXTMARCA
           MOVE ZEROS TO MARCA W-SEL ANOFABRICACAO.
      *-------------[VISUALIZACAO DA TELA]------------------------------
           DISPLAY TELA2.
       R2.
           ACCEPT TPLACA
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
       LER-CADMCARRO.
           READ CADCARRO
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                PERFORM R6A
                PERFORM R7A
                DISPLAY TELA2
                MOVE "*** CARRO JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADCARRO" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** CARRO NAO CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.
       R3.       
           ACCEPT TPROPRIETARIO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.
 
       R4.
           ACCEPT TANOFABRICACAO.  
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.
       R6.
           MOVE 1 TO IND
           ACCEPT TSITUACAO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4.
       R6A.
           MOVE TBSITUACAO(IND) TO TXTSITUACAO
           IF TXTSITUACAO1 NOT = SITUACAO
              ADD 1 TO IND
              IF IND < 10
                 GO TO R6A
              ELSE
                 MOVE "*** TIPO SITUACAO INCORRETO***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R6
           ELSE
               DISPLAY TTXTSITUACAO2.

       R7.
           DISPLAY TELAMARCA
           ACCEPT TMARCA.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELA2
                   GO TO R6.
           IF MARCA = 0 
                 MOVE "*** DIGITE APENAS DE 1 ATE 9 ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R7.
       R7A.
           MOVE TBMARCA(MARCA) TO TXTMARCA
           DISPLAY TTXTMARCA.
           DISPLAY TELA2.
      * ------------- VERICAR SE E ALTERACAO -----------------
           IF W-SEL = 1 
                GO TO ALT-OPC.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R7.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGCARRO
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  MOVE "* CARRO JA EXISTE,DADOS NAO GRAVADOS *" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO R1
                ELSE
                      MOVE "ERRO NA GRAVACAO DO ARQUIVO DE PRODUTO"
                                                       TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.

      *
      *****************************************
      * ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO *
      *****************************************
      *
       ACE-001.
                DISPLAY (23, 12)
                     "N=NOVO REGISTRO   A=ALTERAR   E=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A" 
                    AND W-OPCAO NOT = "E" GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 12) MENS
                IF W-OPCAO = "N"
                   GO TO R1  
                ELSE
                   IF W-OPCAO = "A"
                      MOVE 1 TO W-SEL
                      GO TO R3.
      *
       EXC-OPC.
                DISPLAY (23, 40) "EXCLUIR   (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** REGISTRO NAO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "* DIGITE APENAS S=SIM  e  N=NAO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO EXC-OPC.
       EXC-DL1.
                DELETE CADCARRO RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO CARRO EXCLUIDO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO "   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *
       ALT-OPC.
                DISPLAY (23, 40) "ALTERAR  (S/N) : ".
                ACCEPT (23, 57) W-OPCAO
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R7.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCARRO
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO CARRO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *-----------------------------------------------------------------
       ROT-FIM.
           CLOSE CADCARRO.
           STOP RUN.

      *---------[ ROTINA DE MENSAGEM ]---------------------
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (23, 12) MENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.

      *    FILE STATUS
      *    00 = OPERACAO REALIZADO COM SUCESSO
      *    22 = REGISTRO JA CADASTRADO
      *    23 = REGISTRO NAO ENCONTRADO
      *    30 = ARQUIVO NAO ENCONTRADO