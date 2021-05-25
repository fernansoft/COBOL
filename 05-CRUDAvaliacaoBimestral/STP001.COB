       IDENTIFICATION DIVISION.
       PROGRAM-ID. STP001.
       AUTHOR. FERNANDO OLIVEIRA DA COSTA.
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT CADCEP ASSIGN TO DISK
                    ORGANIZATION         IS INDEXED
                    ACCESS MODE          IS DYNAMIC
                    RECORD KEY           IS CEP
                    ALTERNATE RECORD KEY IS LOGRADOURO WITH DUPLICATES
                    FILE STATUS          IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT".
       01 REGCEP.
          03 CEP               PIC 9(09).
          03 LOGRADOURO        PIC X(35).
          03 BAIRRO            PIC X(20).
          03 CIDADE            PIC X(20).
          03 UFAUX             PIC X(02).
          03 UFAUX2            PIC X(20).
          03 PONTOREF          PIC X(35).
          03 STATS             PIC 9(01).
          03 OBS               PIC X(40).
          03 DATA-INC-MAN.
               07 IGN          PIC 9(02).
               07 MES          PIC 9(02).
               07 DIA          PIC 9(02).
          03 ANO               PIC 9(04).
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
       01 TXTSTATS     PIC X(13) VALUE SPACES.
       01 IND          PIC 9(02) VALUE ZEROS.

       01 TABSTATS.
          03 TBSTATS    PIC X(13) OCCURS 4 TIMES.

       01 TABUFX.
           03 FILLER   PIC X(22) VALUE "ACAcre".
           03 FILLER   PIC X(22) VALUE "ALAlagoas".
           03 FILLER   PIC X(22) VALUE "APAmapa".
           03 FILLER   PIC X(22) VALUE "AMAmazonas".
           03 FILLER   PIC X(22) VALUE "BABahia".
           03 FILLER   PIC X(22) VALUE "CECeara".
           03 FILLER   PIC X(22) VALUE "ESEspirito Santo".
           03 FILLER   PIC X(22) VALUE "GOGoias".
           03 FILLER   PIC X(22) VALUE "MAMaranhao".
           03 FILLER   PIC X(22) VALUE "MTMato Grosso".
           03 FILLER   PIC X(22) VALUE "MSMato Grosso do Sul".
           03 FILLER   PIC X(22) VALUE "MGMinas Gerais".
           03 FILLER   PIC X(22) VALUE "PAPara".
           03 FILLER   PIC X(22) VALUE "PBParaiba".
           03 FILLER   PIC X(22) VALUE "PRParana".
           03 FILLER   PIC X(22) VALUE "PEPernambuco".
           03 FILLER   PIC X(22) VALUE "PIPiaui".
           03 FILLER   PIC X(22) VALUE "RJRio de Janeiro".
           03 FILLER   PIC X(22) VALUE "RNRio Grande do Norte".
           03 FILLER   PIC X(22) VALUE "RSRio Grande do Sul".
           03 FILLER   PIC X(22) VALUE "RORondonia".
           03 FILLER   PIC X(22) VALUE "RRRoraima".
           03 FILLER   PIC X(22) VALUE "SCSanta Catarina".
           03 FILLER   PIC X(22) VALUE "SPSao Paulo".
           03 FILLER   PIC X(22) VALUE "SESergipe".
           03 FILLER   PIC X(22) VALUE "TOTocantins".
           03 FILLER   PIC X(22) VALUE "DFDistrito Federal".
       01 TABUF REDEFINES TABUFX.
          03 TBUF   PIC X(22) OCCURS 27 TIMES.
       01 TXTUF.
          03 TXTUF1 PIC X(02) VALUE SPACES.
          03 TXTUF2 PIC X(20) VALUE SPACES. 

      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELA.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 29  VALUE  "*** CADASTRO DE CEP ***".
           05  LINE 04  COLUMN 04  VALUE  "CEP:".
           05  LINE 06  COLUMN 04  VALUE  "LOGRADOURO:".
           05  LINE 08  COLUMN 04  VALUE  "BAIRRO:".
           05  LINE 10  COLUMN 04  VALUE  "CIDADE:".
           05  LINE 12  COLUMN 04  VALUE  "UF:".
           05  LINE 14  COLUMN 04  VALUE  "PONTO REF.:".
           05  LINE 16  COLUMN 04  VALUE  "STATUS:".
           05  LINE 18  COLUMN 04  VALUE  "OBSERVACAO:".
           05  LINE 20  COLUMN 04  VALUE  "DATA INC.MAN:".
           05  LINE 23  COLUMN 01  VALUE  "MENSAGEM:".
           05  TCEP
               LINE 04  COLUMN 10  PIC 99999.999
               USING  CEP
               HIGHLIGHT.
           05  TLOGRADOURO
               LINE 06  COLUMN 17  PIC X(35)
               USING  LOGRADOURO
               HIGHLIGHT.
           05  TBAIRRO
               LINE 08  COLUMN 13  PIC X(20)
               USING  BAIRRO
               HIGHLIGHT.
           05  TCIDADE
               LINE 10  COLUMN 13  PIC X(20)
               USING  CIDADE
               HIGHLIGHT.
           05  TUF
               LINE 12  COLUMN 09  PIC X(02)
               USING  UFAUX
               HIGHLIGHT.
           05  TTXTUF2
               LINE 12  COLUMN 13  PIC X(20)
               USING  TXTUF2
               HIGHLIGHT.
           05  TPONTOREF
               LINE 14  COLUMN 17  PIC X(35)
               USING  PONTOREF
               HIGHLIGHT.
           05  TSTATS
               LINE 16  COLUMN 13  PIC X(01)
               USING  STATS
               HIGHLIGHT.
           05  TTXTSTATS
               LINE 16  COLUMN 16  PIC X(13)
               USING  TXTSTATS
               HIGHLIGHT.
           05  TOBS
               LINE 18  COLUMN 17  PIC X(40)
               USING  OBS
               HIGHLIGHT.
           05  TDIA
               LINE 20  COLUMN 19  PIC X(02)
               USING  DIA
               HIGHLIGHT.
           05  TMES
               LINE 20  COLUMN 22  PIC X(02)
               USING  MES
               HIGHLIGHT.
           05  TANO
               LINE 20  COLUMN 25  PIC X(04)
               USING  ANO
               HIGHLIGHT.
      *
       01  TELASTATS.
           05  LINE 16  COLUMN 50 VALUE  "1-ATIVO".
           05  LINE 17  COLUMN 50 VALUE  "2-INATIVO".
           05  LINE 18  COLUMN 50 VALUE  "3-CANCELADO".
           05  LINE 19  COLUMN 50 VALUE  "4-ESPECIFICO".
      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
           MOVE "ATIVO"        TO TBSTATS(1)
           MOVE "INATIVO"      TO TBSTATS(2)
           MOVE "CANCELADO"    TO TBSTATS(3)
           MOVE "ESPECIFICO"   TO TBSTATS(4)
       
       R0.
           OPEN I-O CADCEP
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADCEP
                 CLOSE CADCEP
                 MOVE "*** ARQUIVO CADCEP FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADCEP" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      * 
      *------------[ INICIALIZACAO DAS VARIAVEIS ]---------------------               
       R1.
           MOVE SPACES TO LOGRADOURO BAIRRO  CIDADE UFAUX TXTUF
           MOVE SPACES TO TXTSTATS PONTOREF STATS OBS UFAUX2
           MOVE ZEROS TO W-SEL CEP
           MOVE 2021 TO ANO
           ACCEPT DATA-INC-MAN FROM DATE

      *-------------[VISUALIZACAO DA TELA]------------------------------
           DISPLAY TELA.
       R2.
           ACCEPT TCEP
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
           IF CEP > 1000000
            MOVE "*OK FEITO*" TO MENS            
           ELSE
            MOVE "*CAMPO EM BRANCO*" TO MENS
            PERFORM ROT-MENS THRU ROT-MENS-FIM
            GO TO R2.
           
       LER-CADMCEP.
           READ CADCEP
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                PERFORM R5A
                PERFORM R7A
                DISPLAY TELA
                MOVE "*** CEP JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADCEP" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** CEP NAO CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.
       R3.
           ACCEPT TLOGRADOURO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.
           IF LOGRADOURO NOT = SPACE
            MOVE "*OK FEITO*" TO MENS
           ELSE
            MOVE "*CAMPO EM BRANCO*" TO MENS
            PERFORM ROT-MENS THRU ROT-MENS-FIM
            GO TO R3.
       R4.
           ACCEPT TBAIRRO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.
           IF BAIRRO NOT = SPACE
                 MOVE "*OK FEITO*" TO MENS
           ELSE
            MOVE "*CAMPO EM BRANCO*" TO MENS
            PERFORM ROT-MENS THRU ROT-MENS-FIM
            GO TO R4.
       R4A.
           ACCEPT TCIDADE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4.
           IF CIDADE NOT = SPACE
            MOVE "*OK FEITO*" TO MENS
           ELSE
            MOVE "*CAMPO EM BRANCO*" TO MENS
            PERFORM ROT-MENS THRU ROT-MENS-FIM
            GO TO R4A.
       R5.
           MOVE 1 TO IND
           ACCEPT TUF.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4A.
       R5A.
           MOVE TBUF(IND) TO TXTUF
           IF TXTUF1 NOT = UFAUX
              ADD 1 TO IND
              IF IND < 28
                 GO TO R5A
              ELSE
                 MOVE "*** TIPO UF INCORRETO***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R5
           ELSE
               MOVE TXTUF2 TO UFAUX2
               DISPLAY TTXTUF2.
       R6.
           ACCEPT TPONTOREF
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R5.
           IF CIDADE NOT = SPACE
            MOVE "*OK FEITO*" TO MENS
           ELSE
            MOVE "*CAMPO EM BRANCO*" TO MENS
            PERFORM ROT-MENS THRU ROT-MENS-FIM
            GO TO R6.
       R7.
           DISPLAY TELASTATS
           ACCEPT TSTATS.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELA
                   GO TO R6.
           IF STATS = 0
                 MOVE "*** DIGITE APENAS DE 1 ATE 4 ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R7.
       R7A.
           MOVE TBSTATS(STATS) TO TXTSTATS
           DISPLAY TTXTSTATS.
           DISPLAY TELA.
       R8.
           ACCEPT TOBS
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R7.
           IF OBS NOT = SPACE
            MOVE "*OK FEITO*" TO MENS
           ELSE
            MOVE "*CAMPO EM BRANCO*" TO MENS
            PERFORM ROT-MENS THRU ROT-MENS-FIM
            GO TO R8.
       R9. 
           ACCEPT TDIA
           ACCEPT TMES
           ACCEPT TANO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R8.

      * ------------- VERICAR SE E ALTERACAO -----------------
           IF W-SEL = 1 
                GO TO ALT-OPC.
       INC-OPC.
                MOVE "S" TO W-OPCAO
                DISPLAY (23, 40) "DADOS OK (S/N) : ".
                ACCEPT (23, 57) W-OPCAO WITH UPDATE
                ACCEPT W-ACT FROM ESCAPE KEY
                IF W-ACT = 01 GO TO R9.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
       INC-WR1.
                WRITE REGCEP
                IF ST-ERRO = "00" OR "02"
                      MOVE "*** DADOS GRAVADOS *** " TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO R1.
                IF ST-ERRO = "22"
                  MOVE "* CEP JA EXISTE,DADOS NAO GRAVADOS *" TO MENS
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
                DISPLAY (23, 11)
                     "N=NOVO REGISTRO   A=ALTERAR   E=EXCLUIR"
                ACCEPT (23, 55) W-OPCAO
                IF W-OPCAO NOT = "N" AND W-OPCAO NOT = "A" 
                    AND W-OPCAO NOT = "E" GO TO ACE-001.
                MOVE SPACES TO MENS
                DISPLAY (23, 11) MENS
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
                DELETE CADCEP RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO CEP EXCLUIDO ***" TO MENS
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
                IF W-ACT = 01 GO TO R9.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCEP
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO CEP"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *-----------------------------------------------------------------
       ROT-FIM.
           CLOSE CADCEP.
           STOP RUN.

      *---------[ ROTINA DE MENSAGEM ]---------------------
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 11) MENS.
       ROT-MENS2.
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   MOVE SPACES TO MENS
                   DISPLAY (23, 11) MENS.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.

      *    FILE STATUS
      *    00 = OPERACAO REALIZADO COM SUCESSO
      *    22 = REGISTRO JA CADASTRADO
      *    23 = REGISTRO NAO ENCONTRADO
      *    30 = ARQUIVO NAO ENCONTRADO