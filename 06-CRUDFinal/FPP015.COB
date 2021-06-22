       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP015.
       AUTHOR. FERNANDO OLIVEIRA DA COSTA
      ************************************
      * RELATORIO FOLHA DE PAGAMENTOS *
      ************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                     DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQFUNC ASSIGN TO DISK
                       ORGANIZATION IS INDEXED
                       ACCESS MODE  IS DYNAMIC
                       RECORD KEY   IS CODFUNC
                       ALTERNATE RECORD KEY IS NOMEFUNC WITH DUPLICATES
                       FILE STATUS  IS FUNC-ERRO.
           
           SELECT ARQCARG ASSIGN TO DISK
                       ORGANIZATION IS INDEXED
                       ACCESS MODE  IS DYNAMIC
                       RECORD KEY   IS CODCARGO
                       FILE STATUS  IS CARG-ERRO
                       ALTERNATE RECORD KEY IS NOMECARGO 
                       WITH DUPLICATES.

           SELECT ARQFUNCTX ASSIGN TO DISK
                  ORGANIZATION IS LINE SEQUENTIAL
                  FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD ARQFUNC
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQFUNC.DAT". 
       01 REGFUNC.
           03 CODFUNC PIC 9(6).
           03 NOMEFUNC PIC X(30).
           03 GENEROAUX PIC X(1).
           03 GENERO.
               05 GENERO1 PIC X(1).
               05 GENERO2 PIC X(26).
           03 STATUSAUX PIC X.
           03 STATUSF.
               05 STATUSF1 PIC X.
               05 STATUSF2 PIC X(15).
           03 DATANASC.
               05 ANO              PIC 9(4).
               05 MES              PIC 9(2).
               05 DIA              PIC 9(2).
           03 SEXO PIC X.
           03 SEXO2 PIC X(9).
           03 CARGOFUNC PIC 999.
           03 DEPFUNC PIC 999.
           03 NIVELS PIC 9(2).
           03 IMP PIC X.
           03 IMP2 PIC X(11).
           03 QTDDEP PIC 9.

       FD ARQCARG
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQCARG.DAT". 
       01 REGCARG.
           03 CODCARGO PIC 9(3).
           03 NOMECARGO PIC X(25).
           03 TIPOSALARIOAUX PIC X(1).
           03 TIPOSALARIO.
               05 TIPOSALARIO1 PIC X(1).
               05 TIPOSALARIO2 PIC X(14).
           03 STATUSCAUX PIC X.
           03 STATUSC.
               05 STATUSC1 PIC X.
               05 STATUSC2 PIC X(15).
           03 SALARIO PIC 9(06)V99.
      *
       FD ARQFUNCTX
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQFUNCRL.DOC".
       01 REGFUNCIONARIOTX    PIC X(111).
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       77 W-SEL      PIC 9(01) VALUE ZEROS.
       77 W-CONT     PIC 9(06) VALUE ZEROS.
       77 W-OPCAO    PIC X(01) VALUE SPACES.
       77 ST-ERRO    PIC X(02) VALUE "00". 
       77 FUNC-ERRO    PIC X(02) VALUE "00".
       77 CARG-ERRO    PIC X(02) VALUE "00".       
       77 W-ACT      PIC 9(02) VALUE ZEROS.
       77 MENS       PIC X(50) VALUE SPACES.
       77 LIMPA      PIC X(50) VALUE SPACES.
       01 CONLIN     PIC 9(03) VALUE ZEROS.
       01 CONLIN1    PIC 9(04) VALUE ZEROS.
       01 CONPAG     PIC 9(03) VALUE ZEROS.
       01 X       PIC 9(03) VALUE ZEROS.
       01 AUX         PIC 9(8).
       01 Y         PIC 9(01)V99 VALUE ZEROS.
       01 Z         PIC 9(01)V99 VALUE ZEROS.
       01 SALBRUTO      VALUE ZEROS  PIC 9(8).
       01 TOTBRUTO      VALUE ZEROS  PIC 9(8).
       01 INSS      VALUE ZEROS  PIC 9(8).
       01 TOTINSS      VALUE ZEROS  PIC 9(8).
       01 RENDA      VALUE ZEROS  PIC 9(8).
       01 TOTRENDA      VALUE ZEROS  PIC 9(8).
       01 LIQUIDO      VALUE ZEROS  PIC 9(8).
       01 TOTLIQUIDO      VALUE ZEROS  PIC 9(8).
      *

       01  CAB1.
           05  FILLER                 PIC X(057) VALUE
           "---------------------------------------------------------".
           05  FILLER                 PIC X(057) VALUE
           "---------------------------------------------------------".
           05  FILLER                 PIC X(057) VALUE
           "---------------------------------------------------------".
           05  FILLER                 PIC X(1) VALUE
           " ".

       01  CAB2.
           05  FILLER                 PIC X(32) VALUE
           "EMPRESA FATEC - ZL              ".
           05  FILLER                 PIC X(45) VALUE
           " *** FOLHA DE PAGAMENTO ***        *** PAGINA".
           05  FILLER                 PIC X(002) VALUE
           ": ".
           05  NUMPAG  VALUE ZEROS  PIC Z99.
           05  FILLER                 PIC X(20) VALUE
           "              DATA: ".
           05  DATAREL  VALUE ZEROS  PIC 99.99.9999.

       01  CAB3.
           05  FILLER                 PIC X(38) VALUE
           "CODIGO       NOME DO FUNCIONARIO      ".
           05  FILLER                 PIC X(40) VALUE
           "   CARGO                      SAL.BRUTO ".
           05  FILLER                 PIC X(34) VALUE
           "    INSS    IMP.RENDA SAL.LIQUIDO".

       01  CAB4.
           05  FILLER                 PIC X(057) VALUE
           "---------------------------------------------------------".
           05  FILLER                 PIC X(057) VALUE
           "---------------------------------------------------------".
           05  FILLER                 PIC X(057) VALUE
           "---------------------------------------------------------".
           05  FILLER                 PIC X(036) VALUE
           "999 XXXXXXXXXXXXXXXXXXXX".

       01  DET.
           05  CODFUNCTX       VALUE ZEROS  PIC 999999.
           05  FILLER          PIC X(1) VALUE  " ".
           05  NOMEFUNCTX      VALUE SPACES PIC X(30).
           05  FILLER          PIC X(1) VALUE  " ".
           05  CODCARGOTX    VALUE ZEROS  PIC 9(3).
           05  FILLER          PIC X(1) VALUE  " ".
           05  NOMECARGOTX    VALUE SPACES PIC X(25).
           05  FILLER          PIC X(1) VALUE  " ".
           05  SALBRUTOTX      VALUE ZEROS  PIC ZZZ.ZZ9,99.
           05  FILLER          PIC X(1) VALUE  " ".
           05  INSSTX          VALUE ZEROS  PIC ZZZ.ZZ9,99.
           05  FILLER          PIC X(1) VALUE  " ".
           05  IMPTX           VALUE ZEROS  PIC ZZZ.ZZ9,99.
           05  FILLER          PIC X(1) VALUE  " ".
           05  SALLIQUIDOTX    VALUE ZEROS  PIC ZZZ.ZZ9,99.

       01  LINHA-TOT1.
           05  FILLER                 PIC X(057) VALUE
           "TOTAL DA FOLHA DE PAGAMENTO.............................:".
           05  FILLER                 PIC X(11) VALUE
           "           ".
           05  TOTBRUTOTX  VALUE ZEROS  PIC ZZZ.ZZ9,99.
           05  FILLER                 PIC X(01) VALUE
           " ".
           05  TOTINSSTX  VALUE ZEROS  PIC ZZZ.ZZ9,99.
           05  FILLER                 PIC X(01) VALUE
           " ".
           05  TOTIMPTX  VALUE ZEROS  PIC ZZZ.ZZ9,99.
           05  FILLER                 PIC X(01) VALUE
           " ".
           05  TOTLIQUIDOTX  VALUE ZEROS  PIC ZZZ.ZZ9,99.

       01  LINHA-TOT2.
           05  FILLER                 PIC X(023) VALUE
           "TOTAL DE FUNCIONARIOS: ".
           05  TOTFUNC  VALUE ZEROS  PIC Z99.

       01  LINHABR                    PIC X(100) VALUE SPACES.
      *-----------------------------------------------------------------
       SCREEN SECTION.
       01  TELA.
           05  BLANK SCREEN.
           05  LINE 02  COLUMN 01 
               VALUE  "                *** RELATORIO DE FOLHA".
           05  LINE 02  COLUMN 39 
               VALUE  " DE PAGAMENTO ***".
           05  LINE 12  COLUMN 01 
               VALUE  "                         CONFIRMA IMPRES".
           05  LINE 12  COLUMN 41 
               VALUE  "SAO (S ou N ) :".
           05  LINE 24  COLUMN 01 
               VALUE  " MENSAGEM :".
           05  TW-OPCAO
               LINE 12  COLUMN 57  PIC X(01)
               USING  W-OPCAO
               HIGHLIGHT.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO. 
      *
       INC-001.
                MOVE SPACES TO NOMEFUNC .
                MOVE SPACES TO NOMECARGO. 
                MOVE ZEROS  TO CODFUNC CODCARGO.
                DISPLAY TELA.
       INC-OPC.
                ACCEPT TW-OPCAO
                IF W-OPCAO = "N" OR "n"
                   MOVE "* DADOS RECUSADOS PELO USUARIO *" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-FIM.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO INC-OPC.
      *
       INC-OP0.
           OPEN INPUT ARQFUNC
           IF FUNC-ERRO NOT = "00"
               IF FUNC-ERRO = "30"
                      MOVE "* ARQUIVO FUNCIONARIO NAO EXISTE *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
               ELSE
                  MOVE "ERRO NA ABERTURA DO ARQUIVO FUNCIONARIO" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
                    NEXT SENTENCE.
           
           OPEN INPUT ARQCARG
           IF CARG-ERRO NOT = "00"
               IF CARG-ERRO = "30"
                      MOVE "* ARQUIVO CARGO NAO EXISTE *" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
               ELSE
                      MOVE "ERRO NA ABERTURA DO ARQUIVO CARGO" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
           ELSE
                    NEXT SENTENCE.
      *
           OPEN OUTPUT ARQFUNCTX
           IF ST-ERRO NOT = "00"
                      MOVE "ERRO ABERTURA DO ARQUIVO ARQFUNCTX" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
       ROT-CABEC. 
           ADD 1 TO CONPAG
           MOVE CONPAG TO NUMPAG
           WRITE REGFUNCIONARIOTX FROM CAB1. 

           MOVE SPACES TO REGFUNCIONARIOTX    
           WRITE REGFUNCIONARIOTX.
           WRITE REGFUNCIONARIOTX FROM CAB2.
           WRITE REGFUNCIONARIOTX FROM CAB3.
           WRITE REGFUNCIONARIOTX FROM CAB4.
      *
       LER-FUNCIONARIO01.
                READ ARQFUNC NEXT
                MOVE CARGOFUNC TO CODCARGO.
                READ ARQCARG
                IF CARG-ERRO NOT = "00"
                  MOVE "ERRO NA LEITURA ARQUIVO CADASTRO CARGOS" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM.
                IF FUNC-ERRO NOT = "00"
                   IF FUNC-ERRO = "10"
                      MOVE CONLIN1 TO TOTFUNC
                      MOVE TOTBRUTO TO TOTBRUTOTX
                      MOVE TOTINSS TO TOTINSSTX
                      MOVE TOTRENDA TO TOTIMPTX
                      MOVE TOTLIQUIDO TO TOTLIQUIDOTX

                      MOVE SPACES TO REGFUNCIONARIOTX
                      WRITE REGFUNCIONARIOTX FROM LINHABR

                      WRITE REGFUNCIONARIOTX FROM LINHA-TOT1                                           
                      WRITE REGFUNCIONARIOTX FROM LINHA-TOT2
                      MOVE "** FIM DO ARQUIVO DE FUNCIONARIO **" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                   ELSE
                      MOVE "ERRO NA LEITURA ARQFUNC"   TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO ROT-FIM
                ELSE
                   NEXT SENTENCE.
       CALC1.
           
           IF  TIPOSALARIOAUX = "H"
               MOVE 220 TO X.
           IF  TIPOSALARIOAUX = "D"
               MOVE 30 TO X.
           IF  TIPOSALARIOAUX = "M"
               MOVE 1 TO X.        
           COMPUTE SALBRUTO = SALARIO * X
           COMPUTE TOTBRUTO = TOTBRUTO + SALBRUTO.
       CALC2.
           
           IF  SALBRUTO < 2300,01
               MOVE 0,08 TO Y.
           IF  SALBRUTO > 2300 AND SALBRUTO < 4500,01
               MOVE 0,09 TO Y.
           IF  SALBRUTO > 4500
               MOVE 0,10 TO Y.        
           COMPUTE INSS = SALBRUTO * Y
           COMPUTE AUX = SALBRUTO - INSS    
           COMPUTE TOTINSS = TOTINSS + INSS.

       CALC3.
           
           IF  AUX < 3400,01
               MOVE 0 TO Z.
           IF  AUX > 3400 AND AUX < 6000,01
               MOVE 0,12 TO Z.
           IF  AUX > 6000 AND AUX < 9500,01
               MOVE 0,15 TO Z.
           IF  AUX > 9500
               MOVE 0,20 TO Z.
           COMPUTE RENDA = AUX * Z
           COMPUTE TOTRENDA = TOTRENDA + RENDA.

       CALC4.               
           COMPUTE LIQUIDO = SALBRUTO - INSS - RENDA
           COMPUTE TOTLIQUIDO = TOTLIQUIDO + LIQUIDO.
             
       INC-003.
           MOVE CODFUNC TO CODFUNCTX.
           MOVE NOMEFUNC  TO NOMEFUNCTX.
           MOVE CODCARGO TO CODCARGOTX.
           MOVE NOMECARGO TO NOMECARGOTX.
           MOVE SALBRUTO TO SALBRUTOTX                      
           MOVE INSS TO INSSTX
           MOVE RENDA TO IMPTX.
           MOVE LIQUIDO TO SALLIQUIDOTX.

       INC-WR1.
           WRITE REGFUNCIONARIOTX FROM DET
           IF ST-ERRO NOT = "00"
                   MOVE "ERRO NA GRAVACAO DO ARQUIVO ARQFUNCTX"
                                                    TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-FIM.

           ADD 1 TO CONLIN CONLIN1
           IF CONLIN < 34
                 GO TO LER-FUNCIONARIO01
           ELSE
                 MOVE ZEROS TO CONLIN
                 MOVE SPACES TO REGFUNCIONARIOTX

                 WRITE REGFUNCIONARIOTX FROM LINHABR
                 WRITE REGFUNCIONARIOTX FROM LINHABR
                 WRITE REGFUNCIONARIOTX FROM LINHABR
                 WRITE REGFUNCIONARIOTX FROM LINHABR
                 WRITE REGFUNCIONARIOTX FROM LINHABR
                 WRITE REGFUNCIONARIOTX FROM LINHABR
                 GO TO ROT-CABEC.
      *
      **********************
      * ROTINA DE FIM      *
      **********************
       ROT-FIM.
                DISPLAY (01, 01) ERASE
                CLOSE ARQFUNC ARQFUNCTX ARQCARG.
       ROT-FIMP.
                EXIT PROGRAM.

       ROT-FIMS.
                STOP RUN.
      *
      **********************
      * ROTINA DE MENSAGEM *
      **********************
      *
       ROT-MENS.
                MOVE ZEROS TO W-CONT.
       ROT-MENS1.
               DISPLAY (23, 12) MENS.
       ROT-MENS2.              
                ADD 1 TO W-CONT
                IF W-CONT < 3000
                   GO TO ROT-MENS2
                ELSE
                   DISPLAY (23, 12) LIMPA.
       ROT-MENS-FIM.
                EXIT.
       FIM-ROT-TEMPO.
