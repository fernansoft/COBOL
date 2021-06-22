       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP001.
       AUTHOR. FERNANDO OLIVEIRA DA COSTA.
      **************************************
      * ALTERACAO CADASTRO DE DEPARTAMENTOS  *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ARQDEP ASSIGN TO DISK
                   ORGANIZATION IS INDEXED
                   ACCESS MODE  IS DYNAMIC
                   RECORD KEY   IS CODEDEP
                   FILE STATUS  IS ST-ERRO
                   ALTERNATE RECORD KEY IS NOMEDEP WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD ARQDEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQDEP.DAT". 
       01 REGDEP.
           03 CODEDEP PIC 9(3).
           03 NOMEDEP PIC X(25).
           03 STATUSDEAUX PIC X(1).
           03 STATUSDE.
               05 STATUSDE1 PIC X(1).
               05 STATUSDE2 PIC X(14).
           03 NIVELHAUX PIC 9(2).
           03 NIVELH.
               05 NIVELH1 PIC 9(2).
               05 NIVELH2 PIC X(28).


       WORKING-STORAGE SECTION.
       
       01 W-CONT        PIC 9(06) VALUE ZEROS.
       01 W-OPCAO       PIC X(01) VALUE SPACES.
       01 OPCAO       PIC X(01) VALUE SPACES.
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
         
       
       01 TABSTATUS REDEFINES TABSTATUSX.
           03 TBSTATUS   PIC X(15) OCCURS 3 TIMES.
    
       01 TABNIVELHX.
           03 FILLER     PIC X(30) VALUE "01 > CONSELHO CONSULTIVO".
           03 FILLER     PIC X(30) VALUE "02 > CONSELHO FISCAL".
           03 FILLER     PIC X(30) VALUE "03 > PRESIDENCIA".
           03 FILLER    PIC X(30) VALUE "04 > DIRETORIA ADMINISTRATIVA".
           03 FILLER     PIC X(30) VALUE "05 > DIRETORIA INDUSTRIAL".
           03 FILLER     PIC X(30) VALUE "06 > DIRETORIA COMERCIAL".
           03 FILLER     PIC X(30) VALUE "07 > DIRETORIA MARKETING".
   	       03 FILLER     PIC X(30) VALUE "08 > DIRETORIA JURIDICA".
           03 FILLER     PIC X(30) VALUE "09 > CONSULTORIA".          
      
       01 TABNIVELH REDEFINES TABNIVELHX.
           03 TBNIVELH  PIC X(30) OCCURS 9 TIMES.
      *
       01 TABNIVELH.
           03 TABNIVELH1 PIC X(02) .
           03 TABNIVELH2 PIC X(28) .
      *---------------------------------------------------------------- 

       SCREEN SECTION.
       
       01  TELA.
           05  LINE 03  COLUMN 15 VALUE  "********** CADASTRO DE DE".
           05  LINE 03  COLUMN 41 VALUE  "PARTAMENTO ***********".
           05  LINE 04  COLUMN 01 VALUE  "____________________________".
           05  LINE 04  COLUMN 29 VALUE  "____________________________".
           05  LINE 04  COLUMN 57 VALUE  "________________________".
           05  LINE 08  COLUMN 04 VALUE  "COD DEPARTAMENTO:".
           05  LINE 10  COLUMN 04 VALUE  "NOME DEPARTAMENTO:".
           05  LINE 12  COLUMN 04 VALUE  "NIVEL HIERARQUICO:".
           05  LINE 14  COLUMN 04 VALUE  "STATUS:".
           05  TCODEDEP LINE 08  COLUMN 29  PIC 9(03)
               USING  CODEDEP.
           05  TNOMEDEP LINE 10  COLUMN 27  PIC X(25)
               USING  NOMEDEP.
           05  TNIVELH LINE 12  COLUMN 24  PIC 9(02)
               USING  NIVELHAUX.
           05  TNIVELH2 LINE 12  COLUMN 26  PIC X(27)
               USING  NIVELH2.
           05  TSTATUS LINE 14  COLUMN 13  PIC X(01)
               USING  STATUSDEAUX.
           05  TSTATUSDE2 LINE 14  COLUMN 14  PIC X(11)
               USING  STATUSDE2.
       
       01  TELAHIERARQUIA.
           05  LINE 12 COLUMN 40 VALUE " ##########################".
           05  LINE 13 COLUMN 40 VALUE "# 01 > CONSELHO CONSULTIVO  #".
           05  LINE 14 COLUMN 40 VALUE "# 02 > CONSELHO FISCAL      #".
           05  LINE 15 COLUMN 40 VALUE "# 03 > PRESIDENCIA          #".
           05  LINE 16 COLUMN 40 VALUE "# 04 > DIRETORIA ADM.       #".
           05  LINE 17 COLUMN 40 VALUE "# 05 > DIRETORIA INDUSTRIAL #".
           05  LINE 18 COLUMN 40 VALUE "# 06 > DIRETORIA COMERCIAL  #".
           05  LINE 19 COLUMN 40 VALUE "# 07 > DIRETORIA MARKETING  #".
           05  LINE 20 COLUMN 40 VALUE "# 08 > DIRETORIA JURIDICA   #".
           05  LINE 21 COLUMN 40 VALUE "# 09 > CONSULTORIA          #".
           05  LINE 22 COLUMN 40 VALUE " ##########################".
       
       01  STATUSSCREEN.
           05  LINE 12  COLUMN 40 VALUE  " #####################".      
           05  LINE 13  COLUMN 40 VALUE  "# A > ATIVA           #".
           05  LINE 14  COLUMN 40 VALUE  "# S > SUSPENSA        #".
           05  LINE 15  COLUMN 40 VALUE  "# D > DESATIVADA      #".
           05  LINE 16  COLUMN 40 VALUE  " #####################".      

       
       01 TELAEMBRANCO.
           05  LINE 12  COLUMN 40 
           VALUE  "                                 ".
           05  LINE 13  COLUMN 40 
           VALUE  "                                 ".
           05  LINE 14  COLUMN 40 
           VALUE  "                                 ".
           05  LINE 15  COLUMN 40 
           VALUE  "                                 ".
           05  LINE 16  COLUMN 40 
           VALUE  "                                 ".
           05  LINE 17  COLUMN 40 
           VALUE  "                                 ".
           05  LINE 18  COLUMN 40 
           VALUE  "                                 ".
           05  LINE 19  COLUMN 40 
           VALUE  "                                 ".
           05  LINE 20  COLUMN 40 
           VALUE  "                                 ".
           05  LINE 21  COLUMN 40 
           VALUE  "                                 ".
           05  LINE 22  COLUMN 40 
           VALUE  "                                 ".
      *----------------------------------------------------------------  
      
       PROCEDURE DIVISION.
       
       R0.
           OPEN I-O ARQDEP
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT ARQDEP
                 CLOSE ARQDEP
                 MOVE "*** ARQUIVO ARQDEP FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO ARQDEP" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      *                
      *------------[ INICIALIZACAO DAS VARIAVEIS ]---------------------               
       R1.
           MOVE SPACES TO NOMEDEP STATUSDE1 STATUSDE STATUSDEAUX           
           MOVE ZEROS TO CODEDEP NIVELH1 NIVELHAUX. 
         
      *-------------[VISUALIZACAO DA TELA]------------------------------
           
       R2.         
           DISPLAY TELA
           ACCEPT TCODEDEP
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
           IF CODEDEP = 0
                   MOVE "O CODIGO DEVE SER MAIOR QUE ZERO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R2. 
       LER-ARQDEP.
           READ ARQDEP
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                   DISPLAY TELA
                   MOVE "*** DEPARTAMENTO JA CADASTRAD0 ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ACE-001
               ELSE
                   MOVE "ERRO NA LEITURA ARQUIVO ARQDEP" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-FIM
               ELSE
                   MOVE "*** DEPARTAMENTO NAO CADASTRAD0 ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM.
       R3. 
           DISPLAY TELAEMBRANCO      
           ACCEPT TNOMEDEP
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.                   
           IF NOMEDEP = SPACES
                   MOVE "O NOME NAO PODE FICAR EM BRANCO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R3.         
          
       R4.
           MOVE 1 TO IND
           DISPLAY TELAEMBRANCO
           DISPLAY (23, 14) "                                        "
           DISPLAY (23, 30) "                                        "
           DISPLAY TELAHIERARQUIA
           
           ACCEPT TNIVELH
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.
       
       R4A.
           MOVE TBNIVELH(IND) TO NIVELH
           IF NIVELH1 NOT = NIVELHAUX
               ADD 1 TO IND
               IF IND < 10
                   GO TO R4A
               ELSE
                  DISPLAY TELAEMBRANCO
                  DISPLAY (23, 14) "TIPO INCORRETO"
                   STOP "  DIGITE ENTER PARA CONTINUAR"                  
                   GO TO R4
           ELSE
               DISPLAY TNIVELH2.
       
       R5.
           DISPLAY TELAEMBRANCO
           DISPLAY TELA
           MOVE 1 TO IND
           DISPLAY (23, 14) "                                        "
           DISPLAY (23, 30) "                                        "
           DISPLAY STATUSSCREEN
           ACCEPT TSTATUS
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4.
       
       R5A.
           MOVE TBSTATUS(IND) TO STATUSDE
           IF STATUSDE1 NOT = STATUSDEAUX
               ADD 1 TO IND
               IF IND < 4
                   GO TO R5A
               ELSE
                   DISPLAY (23, 14) "TIPO DE STATUS INCORRETA"
                   STOP "  DIGITE ENTER PARA CONTINUAR"                  
                   GO TO R5
           ELSE
               DISPLAY TSTATUSDE2.
       
       R6.
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
           WRITE REGDEP
           IF ST-ERRO = "00" OR "02"
                 MOVE "*** DADOS GRAVADOS *** " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R1.
           IF ST-ERRO = "22"
            MOVE "* DEPARTAMENTO JA EXISTE,DADOS NAO GRAVADOS *" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R1
           ELSE
                 MOVE "ERRO NA GRAVACAO DO ARQUIVO DE DEPARTAMENTO"
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
                DELETE ARQDEP RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO DEPARTAMENTO EXCLUIDO ***" TO MENS
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
                REWRITE REGDEP
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
              MOVE "ERRO NA EXCLUSAO DO REGISTRO DEPARTAMENTO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
       

       ROT-FIM.
           CLOSE ARQDEP.         
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

      *    FILE STATUS
      *    00 = OPERAÇÃO REALIZADO COM SUCESSO
      *    10 = FIM DE ARQUIVO ( QUANDO EXECUTADO LEITURA SEQUENCIAL ) 
      *    22 = REGISTRO JÁ CADASTRADO
      *    23 = REGISTRO NÃO ENCONTRADO
      *    30 = ARQUIVO NÃO ENCONTRADO
      *    95 = ISAM NÃO CARREGADO