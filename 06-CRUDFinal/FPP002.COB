       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP002.
       AUTHOR. FERNANDO OLIVEIRA DA COSTA.
      **************************************
      * ALTERACAO CADASTRO DE CARGOS*
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ARQCARG ASSIGN TO DISK
                   ORGANIZATION IS INDEXED
                   ACCESS MODE  IS DYNAMIC
                   RECORD KEY   IS CODECARGO
                   FILE STATUS  IS ST-ERRO
                   ALTERNATE RECORD KEY IS NOMECARGO WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD ARQCARG
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQCARG.DAT". 
       01 REGCARG.
           03 CODECARGO PIC 9(3).
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


       WORKING-STORAGE SECTION.
       
       01 W-CONT        PIC 9(06) VALUE ZEROS.
       01 W-OPCAO       PIC X(01) VALUE SPACES.
       01 OPCAO         PIC X(01) VALUE SPACES.
       01 W-ACT         PIC 9(02) VALUE ZEROS.
       01 MENS          PIC X(50) VALUE SPACES.
       01 LIMPA         PIC X(50) VALUE SPACES. 
       01 ST-ERRO       PIC X(02) VALUE "00".
       01 W-SEL         PIC 9(01) VALUE ZEROS.
       01 IND           PIC 9(2) VALUE ZEROS.
             
       01 TABSTATUSX.
           03 FILLER     PIC X(15) VALUE "A > ATIVA".
           03 FILLER     PIC X(15) VALUE "S > SUSPENSA".
           03 FILLER     PIC X(15) VALUE "D > DESATIVADA".

       01 TABTIPOSALARIO REDEFINES TABTIPOSALARIOX.
           03 TBTIPOSALARIO   PIC X(15) OCCURS 3 TIMES.
             
       01 TABSTATUS REDEFINES TABSTATUSX.
           03 TBSTATUS   PIC X(15) OCCURS 3 TIMES.

       01 TABTIPOSALARIOX.
           03 FILLER     PIC X(15) VALUE "H > HORISTA".
           03 FILLER     PIC X(15) VALUE "D > DIARISTA".
           03 FILLER     PIC X(15) VALUE "M > MENSALISTA".
      *
      *---------------------------------------------------------------- 

       SCREEN SECTION.
       
       01  TELA.
           05  LINE 02  COLUMN 16 VALUE  "********** CADASTRO DE C".
           05  LINE 02  COLUMN 41 VALUE  "ARGOS **********".
           05  LINE 05  COLUMN 09 VALUE  "CODIGO DO CARGO :".
           05  LINE 07  COLUMN 09 VALUE  "NOME DO CARGO   :".
           05  LINE 09  COLUMN 09 VALUE  "TIPO DE SALARIO :".
           05  LINE 11  COLUMN 09 VALUE  "SALARIO BASE    :".
           05  LINE 13  COLUMN 09 VALUE  "STATUS          :".
           05  TCODECARGO      LINE 05      COLUMN 28  PIC 9(03)
               USING  CODECARGO.
           05  TNOMECARGO      LINE 07      COLUMN 28  PIC X(25)
               USING  NOMECARGO.
           05  TTIPOSALARIO    LINE 09    COLUMN 28  PIC X(01)
               USING  TIPOSALARIOAUX.
           05  TTIPOSALARIO2   LINE 09   COLUMN 29  PIC X(15)
               USING  TIPOSALARIO2.
           05  TSALARIO        LINE 11        COLUMN 28  PIC 9(06)V99
               USING  SALARIO.
           05  TSTATUS         LINE 13         COLUMN 28  PIC X(01)
               USING  STATUSCAUX.
           05  TSTATUSC2       LINE 13       COLUMN 29  PIC X(15)
               USING  STATUSC2.

       
       01  TELATSALARIO.
           05  LINE 12  COLUMN 40 VALUE  " #####################".      
           05  LINE 13  COLUMN 40 VALUE  "# H > HORISTA         #".
           05  LINE 14  COLUMN 40 VALUE  "# D > DIARISTA        #".
           05  LINE 15  COLUMN 40 VALUE  "# M > MENSALISTA      #".
           05  LINE 16  COLUMN 40 VALUE  " #####################".
       
       01  TELASTATUS.
           05  LINE 12  COLUMN 40 VALUE  " #####################".      
           05  LINE 13  COLUMN 40 VALUE  "# A > ATIVA           #".
           05  LINE 14  COLUMN 40 VALUE  "# S > SUSPENSA        #".
           05  LINE 15  COLUMN 40 VALUE  "# D > DESATIVADA      #".
           05  LINE 16  COLUMN 40 VALUE  " #####################".

       
       01 TELAEMBRANCO.
           05  LINE 12  COLUMN 40 VALUE  "                          ".      
           05  LINE 13  COLUMN 40 VALUE  "                          ".
           05  LINE 14  COLUMN 40 VALUE  "                          ".
           05  LINE 15  COLUMN 40 VALUE  "                          ".
           05  LINE 16  COLUMN 40 VALUE  "                          ".
           
            
      *----------------------------------------------------------------  
      
       PROCEDURE DIVISION.
       
       R0.
           OPEN I-O ARQCARG
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT ARQCARG
                 CLOSE ARQCARG
                 MOVE "*** ARQUIVO ARQCARG FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO ARQCARG" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      *                
      *------------[ INICIALIZACAO DAS VARIAVEIS ]---------------------               
       R1.
           MOVE SPACES TO NOMECARGO TIPOSALARIO 
           MOVE SPACES TO TIPOSALARIOAUX STATUSC STATUSCAUX          
           MOVE ZEROS TO CODECARGO SALARIO. 
         
      *-------------[VISUALIZACAO DA TELA]------------------------------
           
       R2.         
           DISPLAY TELA
           ACCEPT TCODECARGO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
           IF CODECARGO = 0
                   MOVE "O CODIGO DEVE SER MAIOR QUE ZERO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R2. 
       LER-ARQCARG.
           READ ARQCARG
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                   DISPLAY TELA
                   MOVE "*** CARGO JA CADASTRAD0 ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ACE-001
               ELSE
                   MOVE "ERRO NA LEITURA ARQUIVO ARQCARG" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-FIM
               ELSE
                   MOVE "*** CARGO NAO CADASTRAD0 ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM.
       R3. 
           DISPLAY TELAEMBRANCO      
           ACCEPT TNOMECARGO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.                   
           IF NOMECARGO = SPACES
                   MOVE "O NOME NAO PODE FICAR EM BRANCO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R3.           
       
       R4.
           DISPLAY TELAEMBRANCO
           DISPLAY TELA
           MOVE 1 TO IND
           DISPLAY (23, 14) "                                        "
           DISPLAY (23, 30) "                                        "
           DISPLAY TELATSALARIO
           ACCEPT TTIPOSALARIO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.
       
       R4A.
           MOVE TBTIPOSALARIO(IND) TO TIPOSALARIO
           IF TIPOSALARIO1 NOT = TIPOSALARIOAUX
               ADD 1 TO IND
               IF IND < 4
                   GO TO R4A
               ELSE
                   DISPLAY (23, 14) "TIPO DE TIPOSALARIO INCORRETA"
                   STOP "  DIGITE ENTER PARA CONTINUAR"                  
                   GO TO R4
           ELSE
               DISPLAY TTIPOSALARIO2.
       
       R5.
           DISPLAY TELAEMBRANCO
           DISPLAY TELA
           ACCEPT TSALARIO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
           IF SALARIO = 0
                   MOVE "O SALARIO DEVE SER MAIOR QUE ZERO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R5. 

       R6.
           MOVE 1 TO IND
           DISPLAY (23, 14) "                                        "
           DISPLAY (23, 30) "                                        "
           DISPLAY TELASTATUS
           
           ACCEPT TSTATUS
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R5.
       
       R6A.
           MOVE TBSTATUS(IND) TO STATUSC
           IF STATUSC1 NOT = STATUSCAUX
               ADD 1 TO IND
               IF IND < 4
                   GO TO R6A
               ELSE
                  DISPLAY TELAEMBRANCO
                  DISPLAY (23, 14) "TIPO INCORRETO"
                   STOP "  DIGITE ENTER PARA CONTINUAR"                  
                   GO TO R6
           ELSE
               DISPLAY TSTATUSC2.
       
       R7.
           DISPLAY TELAEMBRANCO
           DISPLAY TELA   

       
      * ------------- VERICAR SE E ALTERACAO -----------------
           IF W-SEL = 1 
                GO TO ALT-OPC.
       INC-OPC.
           MOVE "S" TO W-OPCAO
           DISPLAY (23, 40) "DADOS OK (S/N) : ".
           ACCEPT (23, 57) W-OPCAO WITH UPDATE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01 GO TO R2.
           IF W-OPCAO = "N" OR "n"
              MOVE "*** DADOS RECUSADOS PELO OPERADOR ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO R1.
           IF W-OPCAO NOT = "S" AND "s"
              MOVE "*** DIGITE APENAS S=SIM e N=NAO ***" TO MENS
              PERFORM ROT-MENS THRU ROT-MENS-FIM
              GO TO INC-OPC.
       INC-WR1.
           WRITE REGCARG
           IF ST-ERRO = "00" OR "02"
                 MOVE "*** DADOS GRAVADOS *** " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R1.
           IF ST-ERRO = "22"
             MOVE "* CARGO JA EXISTE,DADOS NAO GRAVADOS *" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R1
           ELSE
                 MOVE "ERRO NA GRAVACAO DO ARQUIVO DE CARGO"
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
                DELETE ARQCARG RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO CARGO EXCLUIDO ***" TO MENS
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
                IF W-ACT = 01 GO TO R2.
                IF W-OPCAO = "N" OR "n"
                   MOVE "*** INFORMACOES NAO ALTERADAS *** " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                IF W-OPCAO NOT = "S" AND "s"
                   MOVE "*** DIGITE APENAS S=SIM  e  N=NAO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ALT-OPC.
       ALT-RW1.
                REWRITE REGCARG
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO CARGO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
       

       ROT-FIM.
           CLOSE ARQCARG.         
           EXIT PROGRAM.
       ROT-FIM1.
           DISPLAY (01, 01) ERASE.
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

      *    FILE TIPOSALARIO
      *    00 = OPERAÇÃO REALIZADO COM SUCESSO
      *    10 = FIM DE ARQUIVO ( QUANDO EXECUTADO LEITURA SEQUENCIAL ) 
      *    22 = REGISTRO JÁ CADASTRADO
      *    23 = REGISTRO NÃO ENCONTRADO
      *    30 = ARQUIVO NÃO ENCONTRADO
      *    95 = ISAM NÃO CARREGADO