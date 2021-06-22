       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP003.
       AUTHOR. FERNANDO OLIVEIRA DA COSTA.
      **************************************
      * ALTERACAO CADASTRO DE DEPENDENTES*
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ARQDEPEN ASSIGN TO DISK
                   ORGANIZATION IS INDEXED
                   ACCESS MODE  IS DYNAMIC
                   RECORD KEY   IS CODDEPEN              
                   ALTERNATE RECORD KEY IS NOMEDEPEN WITH DUPLICATES
                   FILE STATUS  IS ST-ERRO.
    
           SELECT ARQFUNC ASSIGN TO DISK
                   ORGANIZATION IS INDEXED
                   ACCESS MODE  IS DYNAMIC
                   RECORD KEY   IS CODFUNC
                   ALTERNATE RECORD KEY IS NOMEFUNC WITH DUPLICATES
                   FILE STATUS  IS ST-ERRO.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD ARQDEPEN
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQDEPEN.DAT". 
       01 REGDEPEN.
           03 CODDEPEN.
               05 DEPENFUNC   PIC 9(6).
               05 SEQUENCIA        PIC 9(1).
           03 NOMEDEPEN            PIC X(30).
           03 GRAUPARENTESCOAUX    PIC 9.
           03 GRAUPARENTESCO.
               05 GRAUPARENTESCO1  PIC 9.
               05 GRAUPARENTESCO2  PIC X(20).
           03 DEPEN-DATANASC.          
               05 ANO              PIC 9(4).
               05 MES              PIC 9(2).
               05 DIA              PIC 9(2).
           03 DEPEN-SEXO                 PIC X.
           03 DEPEN-SEXO2                PIC X(9).
           03 INSS                 PIC X.
           03 INSS2                PIC X(11).
           03 DIMP                  PIC X.
           03 DIMP2                 PIC X(11).
           03 DEPEN-STATUSAUX      PIC X.
           03 DEPEN-STATUS.
               05 DEPEN-STATUS1    PIC X.
               05 DEPEN-STATUS2    PIC X(16).
       
       FD ARQFUNC
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQFUNC.DAT". 
       01 REGFUNC.
           03 CODFUNC PIC 9(6).
           03 NOMEFUNC PIC X(25).
           03 GENEROAUX PIC X(1).
           03 GENERO.
               05 GENERO1 PIC X(1).
               05 GENERO2 PIC X(26).
           03 STATUSAUX PIC X.
           03 STATUSF.
               05 STATUSF1 PIC X.
               05 STATUSF2 PIC X(15).
           03 DATANASC PIC 9(10).
           03 SEXO PIC X.
           03 SEXO2 PIC X(9).
           03 CARGOFUNC PIC 999.
           03 DEPFUNC PIC 999.
           03 NIVELS PIC 9(2).
           03 IMP PIC X.
           03 IMP2 PIC X(11).
           03 QTDDEP PIC 9.

       WORKING-STORAGE SECTION.
       
       01 W-CONT        PIC 9(06) VALUE ZEROS.
       01 W-OPCAO       PIC X(01) VALUE SPACES.
       01 OPCAO         PIC X(01) VALUE SPACES.
       01 W-ACT         PIC 9(02) VALUE ZEROS.
       01 MENS          PIC X(50) VALUE SPACES.
       01 LINSSA         PIC X(50) VALUE SPACES. 
       01 ST-ERRO       PIC X(02) VALUE "00".
       01 W-SEL         PIC 9(01) VALUE ZEROS.
       01 IND           PIC 9(2) VALUE ZEROS.
       

       01 TABGRAUPX.
           03 FILLER     PIC X(21) VALUE "0 > ESPOSA".
           03 FILLER     PIC X(21) VALUE "1 > FILHO".
           03 FILLER     PIC X(21) VALUE "2 > ENTEADO(A)".
           03 FILLER     PIC X(21) VALUE "3 > PAI".
           03 FILLER     PIC X(21) VALUE "4 > MAE".
           03 FILLER     PIC X(21) VALUE "5 > AVOS".
           03 FILLER     PIC X(21) VALUE "6 > NETO".
           03 FILLER     PIC X(21) VALUE "7 > AGREGADO".
           03 FILLER     PIC X(21) VALUE "8 > RESPONSAVEL LEGAL".
           03 FILLER     PIC X(21) VALUE "9 > OUTROS".         
             
       01 TABGRAUP REDEFINES TABGRAUPX.
           03 TBGRAUP  PIC X(21) OCCURS 10 TIMES.
    
       01 TABSTATUSX.
           03 FILLER     PIC X(17) VALUE "A > ATIVA".
           03 FILLER     PIC X(17) VALUE "S > SUSPENSA".
           03 FILLER     PIC X(17) VALUE "D > DESATIVADA".
           03 FILLER     PIC X(17) VALUE "X > EX DEPENDENTE".
             
       01 TABSTATUS REDEFINES TABSTATUSX.
           03 TBSTATUS   PIC X(17) OCCURS 4 TIMES.
      *
      *---------------------------------------------------------------- 

       SCREEN SECTION.
       
       01  TELA.
           05  LINE 03  COLUMN 22 VALUE  "**** CADASTRO DE D".
           05  LINE 03  COLUMN 41 VALUE  "EPENDENTES ****".
           05  LINE 06  COLUMN 03 VALUE  "CODIGO DO FUNCIONARIO:".
           05  LINE 06  COLUMN 41 VALUE  "SEQUENCIA DE DEPENDENTE:".
           05  LINE 08  COLUMN 03 VALUE  "NOME:".
           05  LINE 08  COLUMN 41 VALUE  "GRAU PARENTESCO:".
           05  LINE 10  COLUMN 03 VALUE  "DATA DE NASCIMENTO:".
           05  LINE 10  COLUMN 41 VALUE  "DEPEN-SEXO:".
           05  LINE 12  COLUMN 03 VALUE  "INSS:".
           05  LINE 12  COLUMN 41 VALUE  "IMPOSTO DE RENDA:".
           05  LINE 14  COLUMN 03 VALUE  "STATUS:".
           05  TCODFUNC LINE 06 COLUMN 27 PIC 9(06)
               USING  DEPENFUNC.
           05  TSEQUENCIA LINE 06 COLUMN 66 PIC 9(01)
               USING  SEQUENCIA.
           05  TNOME LINE 08 COLUMN 10 PIC X(30)
               USING  NOMEDEPEN.
           05  TGRAU LINE 08 COLUMN 58 PIC 9(01)
               USING  GRAUPARENTESCOAUX.
           05  TGRAU2 LINE 08 COLUMN 59 PIC X(18)
               USING  GRAUPARENTESCO2.
           05  TDEPEN-DATANASC LINE 10 COLUMN 24 PIC 9999/99/99
               USING  DEPEN-DATANASC.
           05  TDEPEN-SEXO LINE 10 COLUMN 53 PIC X(01)
               USING  DEPEN-SEXO.
           05  TDEPEN-SEXO2 LINE 10 COLUMN 55 PIC X(09)
               USING  DEPEN-SEXO2.
           05  TINSS LINE 12 COLUMN 10 PIC X(01)
               USING  INSS.
           05  TINSS2 LINE 12 COLUMN 11 PIC X(11)
               USING  INSS2.
           05  TIMP LINE 12 COLUMN 59 PIC X(01)
               USING  DIMP.
           05  TDIMP2 LINE 12 COLUMN 60 PIC X(11)
               USING  DIMP2.
           05  TSTATUS LINE 14 COLUMN 12 PIC X(01)
               USING  DEPEN-STATUSAUX.
           05  TSTATUS2 LINE 14 COLUMN 13 PIC X(14)
               USING  DEPEN-STATUS2.

       
       01  TELAPARENT.
           05  LINE 12  COLUMN 40 VALUE  " #######################".      
           05  LINE 13  COLUMN 40 VALUE  "# 0 > ESPOSA            #".
           05  LINE 14  COLUMN 40 VALUE  "# 1 > FILHO             #".
           05  LINE 15  COLUMN 40 VALUE  "# 2 > ENTEADO(A)        #".
           05  LINE 16  COLUMN 40 VALUE  "# 3 > PAI               #".
           05  LINE 17  COLUMN 40 VALUE  "# 4 > MAE               #".
           05  LINE 18  COLUMN 40 VALUE  "# 5 > AVOS              #".
           05  LINE 19  COLUMN 40 VALUE  "# 6 > NETO              #".
           05  LINE 20  COLUMN 40 VALUE  "# 7 > AGREGADO          #".
           05  LINE 21  COLUMN 40 VALUE  "# 8 > RESPONSAVEL LEGAL #".
           05  LINE 22  COLUMN 40 VALUE  "# 9 > OUTROS            #".
           05  LINE 23  COLUMN 40 VALUE  " #######################".
       
       01  TELASTATUS.
           05  LINE 12  COLUMN 40 VALUE  " #####################".      
           05  LINE 13  COLUMN 40 VALUE  "# A > ATIVA           #".
           05  LINE 14  COLUMN 40 VALUE  "# S > SUSPENSA        #".
           05  LINE 15  COLUMN 40 VALUE  "# D > DESATIVADA      #".
           05  LINE 16  COLUMN 40 VALUE  "# X > EX DEPENDENTE   #".
           05  LINE 17  COLUMN 40 VALUE  " #####################".

       
       01 TELAEMBRANCO.     
           05  LINE 12  COLUMN 40 VALUE  "                          ".      
           05  LINE 13  COLUMN 40 VALUE  "                          ".
           05  LINE 14  COLUMN 40 VALUE  "                          ".
           05  LINE 15  COLUMN 40 VALUE  "                          ".
           05  LINE 16  COLUMN 40 VALUE  "                          ".
           05  LINE 17  COLUMN 40 VALUE  "                          ".
           05  LINE 18  COLUMN 40 VALUE  "                          ".
           05  LINE 19  COLUMN 40 VALUE  "                          ".
           05  LINE 20  COLUMN 40 VALUE  "                          ".
           05  LINE 21  COLUMN 40 VALUE  "                          ".
           05  LINE 22  COLUMN 40 VALUE  "                          ".
           05  LINE 23  COLUMN 40 VALUE  "                          ".
           
            
      *----------------------------------------------------------------  
      
       PROCEDURE DIVISION.
       
       R0.
           OPEN I-O ARQDEPEN
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT ARQDEPEN
                 CLOSE ARQDEPEN
                 MOVE "*** ARQUIVO ARQDEPEN FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO ARQDEPEN" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
       R0A.
           OPEN INPUT ARQFUNC
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
             MOVE "* ARQUIVO DE FUNCIONARIO NAO ENCONTRADO *" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
             MOVE "ERRO NA ABERTURA DO ARQUIVO DE FUNCIONARIO " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      *                
      *------------[ INICIALIZACAO DAS VARIAVEIS ]---------------------               
       R1.
           MOVE SPACES TO NOMEDEPEN DEPEN-SEXO DEPEN-SEXO2 INSS INSS2 
           MOVE SPACES TO DEPEN-STATUS DEPEN-STATUSAUX NOMEFUNC GENERO
           MOVE SPACES TO GENEROAUX IMP IMP2 DIMP DIMP2 SEXO STATUSF
           MOVE SPACES TO STATUSAUX GRAUPARENTESCO2       
           MOVE ZEROS TO CODDEPEN GRAUPARENTESCOAUX 
           MOVE ZEROS TO CODFUNC DATANASC CARGOFUNC DEPFUNC NIVELS   
           MOVE ZEROS TO QTDDEP DEPEN-DATANASC.
      *-------------[VISUALIZACAO DA TELA]------------------------------
           
       R2.         
           DISPLAY TELA
           ACCEPT TCODFUNC
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
           IF DEPENFUNC = 0
                   MOVE "O CODIGO DEVE SER MAIOR QUE ZERO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R2.
           PERFORM R2A.
           IF ST-ERRO = "23"
             MOVE "*** FUNCIONARIO DIGITADO NAO ENCONTRADO ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R2
           DISPLAY TELA. 
      
       R3.  
           ACCEPT TSEQUENCIA
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.                   
           IF SEQUENCIA = 0
                   MOVE "A SEQUENCIA DEVE SER MAIOR QUE ZERO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R3.
       
       LER-ARQDEPEN.
           READ ARQDEPEN
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                   DISPLAY TELA
                   MOVE "*** DEPENDENTE JA CADASTRAD0 ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ACE-001
               ELSE
                   MOVE "ERRO NA LEITURA ARQUIVO ARQDEPEN" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R2
               ELSE
                   MOVE "*** DEPENDENTE NAO CADASTRAD0 ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM.
       R4. 
           DISPLAY TELAEMBRANCO
           DISPLAY TELA      
           ACCEPT TNOME
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.                   
           IF NOMEDEPEN = SPACES
                   MOVE "O NOME NAO PODE FICAR EM BRANCO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R3.           
       
       R5.
           DISPLAY TELAEMBRANCO
           DISPLAY TELA
           MOVE 1 TO IND
           DISPLAY (23, 14) "                                        "
           DISPLAY (23, 30) "                                        "
           DISPLAY TELAPARENT
           ACCEPT TGRAU
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4.
       
       R5A.
           MOVE TBGRAUP(IND) TO GRAUPARENTESCO
           IF GRAUPARENTESCO1 NOT = GRAUPARENTESCOAUX
               ADD 1 TO IND
               IF IND < 11
                   GO TO R5A
               ELSE
                   DISPLAY (23, 14) "GRAU DE PARENTESCO INCORRETO"
                   STOP "  DIGITE ENTER PARA CONTINUAR"                  
                   GO TO R5
           ELSE
               DISPLAY TGRAU2.
       
       R6.
           DISPLAY TELAEMBRANCO
           DISPLAY TELA
           ACCEPT TDEPEN-DATANASC
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R5.
           IF ANO = 0 OR ANO  < 1910
                   MOVE "A DATA DEVE SER VALIDA" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R6.                                      
           IF MES = 0 OR MES  > 12
                   MOVE "A DATA DEVE SER VALIDA" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R6.
           IF DIA = 0 OR DIA  > 31
                   MOVE "A DATA DEVE SER VALIDA" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R6.          
       
       R7.   
           DISPLAY TELAEMBRANCO
           DISPLAY TELA               
           ACCEPT TDEPEN-SEXO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R6.                   
           IF DEPEN-SEXO = "M" OR DEPEN-SEXO = "m"
                   MOVE "MASCULINO" TO DEPEN-SEXO2
                   DISPLAY TELA
                   GO TO R8. 
           IF DEPEN-SEXO = "F" OR DEPEN-SEXO = "f"
                   MOVE "FEMININO" TO DEPEN-SEXO2
                   DISPLAY TELA
                   GO TO R8
           ELSE
                   GO TO R7.                   
   
       R8.

           ACCEPT TINSS
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R8.
           IF INSS = "S" OR INSS = "s"
                   MOVE "IM" TO INSS2              
                   GO TO R9. 
           IF INSS = "N" OR INSS = "n"
                   MOVE "AO" TO INSS2
                   GO TO R9.
           IF INSS = "E" OR INSS = "e"
                   MOVE "SPECIAL" TO INSS2
                   GO TO R9        
           ELSE    
                   MOVE "S = SIM, N = NAO, E = ESPECIAL" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R8.
       
       R9.
      *    IMPOSTO DE RENDA
           DISPLAY TELAEMBRANCO
           DISPLAY TELA
           ACCEPT TIMP
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R8.
           IF DIMP = "S" OR DIMP = "s"
                   MOVE "IM" TO DIMP2
                   DISPLAY TELA
                   GO TO R10. 
           IF DIMP = "N" OR DIMP = "n"
                   MOVE "AO" TO DIMP2
                   DISPLAY TELA
                   GO TO R10.
           IF DIMP = "E" OR DIMP = "e"
                   MOVE "SPECIAL" TO DIMP2
                   DISPLAY TELA
                   GO TO R10        
           ELSE    
                   MOVE "S = SIM, N = NAO, E = ESPECIAL" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R9.

       R10.
           DISPLAY TELA
           MOVE 1 TO IND
           DISPLAY (23, 14) "                                        "
           DISPLAY (23, 30) "                                        "
           DISPLAY TELASTATUS
           
           ACCEPT TSTATUS
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R9.
       
       R10A.
           MOVE TBSTATUS(IND) TO DEPEN-STATUS
           IF DEPEN-STATUS1 NOT = DEPEN-STATUSAUX
               ADD 1 TO IND
               IF IND < 5
                   GO TO R10A
               ELSE
                  DISPLAY TELAEMBRANCO
                  DISPLAY (23, 14) "TIPO INCORRETO"
                   STOP "  DIGITE ENTER PARA CONTINUAR"                  
                   GO TO R10
           ELSE
               DISPLAY TSTATUS2.
           

       R11.
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
           WRITE REGDEPEN
           IF ST-ERRO = "00" OR "02"
                 MOVE "*** DADOS GRAVADOS *** " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R1.
           IF ST-ERRO = "22"
             MOVE "* DEPENDENTE JA EXISTE,DADOS NAO GRAVADOS *" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R1
           ELSE
                 MOVE "ERRO NA GRAVACAO DO ARQUIVO DE DEPENDENTE"
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
                DELETE ARQDEPEN RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO DEPENDENTE EXCLUIDO ***" TO MENS
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
                REWRITE REGDEPEN
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO DEPENDENTE"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
       
       R2A.
           MOVE DEPENFUNC TO CODFUNC.
           READ ARQFUNC.
       R2A-FIM.
           GO TO R2.

       ROT-FIM.
           CLOSE ARQDEPEN ARQFUNC.         
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
        
               