       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP004.
       AUTHOR. FERNANDO OLIVEIRA DA COSTA.
      **************************************
      * ALTERACAO CADASTRO DE FUNCIONARIOS*
      **************************************
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
                       FILE STATUS  IS ST-ERRO.
                       
           
           SELECT ARQDEP ASSIGN TO DISK
                       ORGANIZATION IS INDEXED
                       ACCESS MODE  IS DYNAMIC
                       RECORD KEY   IS CODDEP
                       FILE STATUS  IS ST-ERRO
                       ALTERNATE RECORD KEY IS NOMEDEP WITH DUPLICATES.
           
           SELECT ARQCARG ASSIGN TO DISK
                       ORGANIZATION IS INDEXED
                       ACCESS MODE  IS DYNAMIC
                       RECORD KEY   IS CODCARGO
                       FILE STATUS  IS ST-ERRO
                       ALTERNATE RECORD KEY IS NOMECARGO 
                       WITH DUPLICATES.
 
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
       01 REGCARGO.
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

       FD ARQDEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQDEP.DAT". 
       01 REGDEP.
           03 CODDEP PIC 9(3).
           03 NOMEDEP PIC X(25).
           03 STATUSDAUX PIC X(1).
           03 STATUSD.
               05 STATUSD1 PIC X(1).
               05 STATUSD2 PIC X(14).
           03 NIVELHAUX PIC 9(2).
           03 NIVELH.
               05 NIVELH1 PIC 9(2).
               05 NIVELH2 PIC X(28).     


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
       
       01 TABGENEROX.
           03 FILLER     PIC X(27) VALUE "H > HETEROSEXUAL".
           03 FILLER     PIC X(27) VALUE "M > HOMOSEXUAL".
           03 FILLER     PIC X(27) VALUE "B > BISEXUAL".
           03 FILLER     PIC X(27) VALUE "T > TRANSEXUAL".
           03 FILLER     PIC X(27) VALUE "P > PANSEXUAL".
           03 FILLER     PIC X(27) VALUE "O > OUTROS OU NAO INFORMADO".
             
       01 TABGENERO REDEFINES TABGENEROX.
           03 TBGENERO   PIC X(27) OCCURS 6 TIMES.
    
       01 TABSTATUSX.
           03 FILLER     PIC X(15) VALUE "A > ATIVA".
           03 FILLER     PIC X(15) VALUE "S > SUSPENSA".
           03 FILLER     PIC X(15) VALUE "D > DESATIVADA".
             
       01 TABSTATUS REDEFINES TABSTATUSX.
           03 TBSTATUS   PIC X(15) OCCURS 3 TIMES.
      *
      *---------------------------------------------------------------- 

       SCREEN SECTION.
       
       01  TELA.
           05  LINE 02  COLUMN 22 VALUE  "**** CADASTRO DE F".
           05  LINE 02  COLUMN 41 VALUE  "UNCIONARIOS ****".
           05  LINE 04  COLUMN 03 VALUE  "CODIGO DO FUNCIONARIO:".
           05  LINE 04  COLUMN 32 VALUE  "NOME:".
           05  LINE 06  COLUMN 03 VALUE  "DATA DE NASCIMENTO   :".
           05  LINE 06  COLUMN 35 VALUE  "SEXO:".
           05  LINE 08  COLUMN 03 VALUE  "GENERO:".
           05  LINE 08  COLUMN 41 VALUE  "NIVEL SALARIAL:".
           05  LINE 10  COLUMN 03 VALUE  "IMPOSTO DE RENDA:".
           05  LINE 10  COLUMN 35 VALUE  "QTD.".
           05  LINE 10  COLUMN 41 VALUE  "DEPENDENTES:   STATUS:".
           05  LINE 12  COLUMN 01 VALUE  "____________________________".
           05  LINE 12  COLUMN 29 VALUE  "____________________________".
           05  LINE 12  COLUMN 57 VALUE  "________________________".
           05  LINE 13  COLUMN 03 VALUE  "CODIGO DEPARTAMENTO:".
           05  LINE 13  COLUMN 35 VALUE  "NOME".
           05  LINE 13  COLUMN 41 VALUE  "DEP.:".
           05  LINE 15  COLUMN 03 VALUE  "NIVEL HIERARQUICO:".
           05  LINE 15  COLUMN 56 VALUE  "STATUS:".
           05  LINE 16  COLUMN 01 VALUE  "____________________________".
           05  LINE 16  COLUMN 29 VALUE  "____________________________".
           05  LINE 16  COLUMN 57 VALUE  "________________________".
           05  LINE 17  COLUMN 03 
           VALUE  "CODIGO DO CARGO    :            NOME".
           05  LINE 17  COLUMN 41 VALUE  "CARGO:".
           05  LINE 19  COLUMN 03 VALUE  "TIPO DE SALARIO    :".
           05  LINE 19  COLUMN 56 VALUE  "STATUS:".
           05  LINE 21  COLUMN 03 VALUE  "SALARIO BASE       :".
           05  TCODFUNC LINE 04  COLUMN 27  PIC 9(06)
               USING  CODFUNC.
           05  TNOMEFUNC LINE 04  COLUMN 42  PIC X(30)
               USING  NOMEFUNC.
           05  TDATANASC LINE 06  COLUMN 27  PIC 9999/99/99
               USING  DATANASC.
           05  TSEXO LINE 06  COLUMN 45  PIC X(01)
               USING  SEXO.        
           05  TSEXO2 LINE 06  COLUMN 47  PIC X(09)
               USING  SEXO2.
           05  TGENERO LINE 08  COLUMN 12  PIC X(01)
               USING  GENEROAUX.
           05  TGENERO2 LINE 08  COLUMN 13  PIC X(26)
               USING  GENERO2.
           05  TNIVELS LINE 08  COLUMN 57  PIC 9(02)
               USING  NIVELS.
           05  TIMP LINE 10  COLUMN 22  PIC X(01)
               USING  IMP.
           05  TIMP2 LINE 10  COLUMN 23  PIC X(11)
               USING  IMP2.
           05  TQTD LINE 10  COLUMN 54  PIC 9(01)
               USING  QTDDEP.
           05  TSTATUS LINE 10  COLUMN 64  PIC X(01)
               USING  STATUSAUX.
           05  TSTATUS2 LINE 10  COLUMN 65  PIC X(14)
               USING  STATUSF2.
           05  TCODDEP LINE 13  COLUMN 25  PIC 9(03)
               USING  DEPFUNC.
           05  TNOMEDEP LINE 13  COLUMN 47  PIC X(25)
               USING  NOMEDEP.
           05  TNIVELH LINE 15  COLUMN 25  PIC X(30)
               USING  NIVELH.
           05  TSTATUSD LINE 15  COLUMN 64  PIC X(15)
               USING  STATUSD.
           05  TCODCARGO LINE 17  COLUMN 25  PIC 9(03)
               USING  CARGOFUNC.
           05  TNOMECARGO LINE 17  COLUMN 48  PIC X(25)
               USING  NOMECARGO.
           05  TTIPOSALARIO LINE 19  COLUMN 25  PIC X(15)
               USING  TIPOSALARIO.
           05  TSTATUSC LINE 19  COLUMN 64  PIC X(15)
               USING  STATUSC.
           05  TSALARIO LINE 21  COLUMN 25  PIC ZZZ.ZZ9,99
               USING  SALARIO.

       
       01  TELAGENERO.
           05 LINE 12 COLUMN 40 VALUE " ###########################".
           05 LINE 13 COLUMN 40 VALUE "# H > HETEROSEXUAL            #".
           05 LINE 14 COLUMN 40 VALUE "# M > HOMOSEXUAL              #".
           05 LINE 15 COLUMN 40 VALUE "# B > BISEXUAL                #".
           05 LINE 16 COLUMN 40 VALUE "# T > TRANSSEXUAL             #".
           05 LINE 17 COLUMN 40 VALUE "# P > PANSEXUAL               #".
           05 LINE 18 COLUMN 40 VALUE "# O > OUTROS OU NAO INFORMADO #".
           05 LINE 19 COLUMN 40 VALUE " ###########################".
       
       01  TELASTATUS.
           05  LINE 12  COLUMN 40 VALUE  " #####################".      
           05  LINE 13  COLUMN 40 VALUE  "# A > ATIVA           #".
           05  LINE 14  COLUMN 40 VALUE  "# S > SUSPENSA        #".
           05  LINE 15  COLUMN 40 VALUE  "# D > DESATIVADA      #".
           05  LINE 16  COLUMN 40 VALUE  " #####################".

       
       01 TELAEMBRANCO.
           05  LINE 12  COLUMN 40 
               VALUE  "                               ".      
           05  LINE 13  COLUMN 40 
               VALUE  "                               ".
           05  LINE 14  COLUMN 40 
               VALUE  "                               ".
           05  LINE 15  COLUMN 40 
               VALUE  "                               ".
           05  LINE 16  COLUMN 40 
               VALUE  "                               ".
           05  LINE 17  COLUMN 40 
               VALUE  "                               ".
           05  LINE 18  COLUMN 40 
               VALUE  "                               ".
           05  LINE 19  COLUMN 40 
               VALUE  "                               ".
           
            
      *----------------------------------------------------------------  
      
       PROCEDURE DIVISION.
       
       R0.
           OPEN I-O ARQFUNC
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT ARQFUNC
                 CLOSE ARQFUNC
                 MOVE "*** ARQUIVO ARQFUNC FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO ARQFUNC" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
       R0A.
           OPEN INPUT ARQDEP
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
             MOVE "* ARQUIVO DE DEPARTAMENTO NAO ENCONTRADO *" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
             MOVE "ERRO NA ABERTURA DO ARQUIVO DE DEPARTAMENTO " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      * 
       R0B.
           OPEN INPUT ARQCARG
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO DE CARGO NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE CARGO " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      *                
      *------------[ INICIALIZACAO DAS VARIAVEIS ]---------------------               
       R1.
           MOVE SPACES TO NOMEFUNC GENERO GENEROAUX IMP IMP2 NOMECARGO
           MOVE SPACES TO STATUSD
           MOVE SPACES TO SEXO STATUSF STATUSAUX STATUSCAUX STATUSDAUX
           MOVE SPACES TO TIPOSALARIOAUX TIPOSALARIO NOMEDEP NIVELH   
           MOVE ZEROS TO CODFUNC DATANASC CARGOFUNC DEPFUNC NIVELS 
           MOVE ZEROS TO QTDDEP CODCARGO SALARIO CODDEP NIVELHAUX 
           
      *-------------[VISUALIZACAO DA TELA]------------------------------
           DISPLAY TELA.
       R2.                        
           ACCEPT TCODFUNC
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
           IF CODFUNC = 0
                   MOVE "O CODIGO DEVE SER MAIOR QUE ZERO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R2. 
       LER-ARQFUNC.
           READ ARQFUNC
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                   PERFORM R11A
                   PERFORM R12A
                   DISPLAY TELA
                   MOVE "*** FUNCIONARIO JA CADASTRAD0 ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ACE-001
               ELSE
                   MOVE "ERRO NA LEITURA ARQUIVO ARQFUNC" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-FIM
               ELSE
                   MOVE "*** FUNCIONARIO NAO CADASTRAD0 ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM.
       R3. 
           DISPLAY TELAEMBRANCO
           DISPLAY TELA      
           ACCEPT TNOMEFUNC
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.                   
           IF NOMEFUNC = SPACES
                   MOVE "O NOME NAO PODE FICAR EM BRANCO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R3.           
       
       R4.                  
           ACCEPT TDATANASC
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.                   
           IF ANO = 0 OR ANO  < 1910
                   MOVE "A DATA DEVE SER VALIDA" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R4.                                      
           IF MES = 0 OR MES  > 12
                   MOVE "A DATA DEVE SER VALIDA" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R4.
           IF DIA = 0 OR DIA  > 31
                   MOVE "A DATA DEVE SER VALIDA" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R4.  
       R5.   
      *     DISPLAY TELAEMBRANCO
           DISPLAY TELA               
           ACCEPT TSEXO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4.                   
           IF SEXO = "M" OR SEXO = "m"
                   MOVE "MASCULINO" TO SEXO2
                   DISPLAY TELA
                   GO TO R6. 
           IF SEXO = "F" OR SEXO = "f"
                   MOVE "FEMININO" TO SEXO2
                   DISPLAY TELA
                   GO TO R6
           ELSE
                   GO TO R5.
       R6.         
           MOVE 1 TO IND
           DISPLAY (23, 14) "                                        "
           DISPLAY (23, 30) "                                        "
           DISPLAY TELAGENERO
           ACCEPT TGENERO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R5.
       
       R6A.
           MOVE TBGENERO(IND) TO GENERO
           IF GENERO1 NOT = GENEROAUX
               ADD 1 TO IND
               IF IND < 7
                   GO TO R6A
               ELSE
                   DISPLAY (23, 14) "TIPO DE GENERO INCORRETA"
                   STOP "  DIGITE ENTER PARA CONTINUAR"                  
                   GO TO R6
           ELSE
               DISPLAY TGENERO2.
       
       R7.
           DISPLAY TELAEMBRANCO
           DISPLAY TELA
           ACCEPT TNIVELS
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R6.
           IF NIVELS > 20
                   MOVE "SOMENTE NIVEIS DE 0 A 20" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R7.

       R8.
           ACCEPT TIMP
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R7.
           IF IMP = "S" OR IMP = "s"
                   MOVE "IM" TO IMP2
                   DISPLAY TELA
                   GO TO R9. 
           IF IMP = "N" OR IMP = "n"
                   MOVE "AO" TO IMP2
                   DISPLAY TELA
                   GO TO R9.
           IF IMP = "E" OR IMP = "e"
                   MOVE "SPECIAL" TO IMP2
                   DISPLAY TELA
                   GO TO R9        
           ELSE    
                   MOVE "S = SIM, N = NAO, E = ESPECIAL" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R8.

       R9.
           DISPLAY TELA
           ACCEPT TQTD
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R8.

       R10.
           MOVE 1 TO IND
           DISPLAY (23, 14) "                                        "
           DISPLAY (23, 30) "                                        "
           DISPLAY TELASTATUS
           
           ACCEPT TSTATUS
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R9.
       
       R10A.
           MOVE TBSTATUS(IND) TO STATUSF
           IF STATUSF1 NOT = STATUSAUX
               ADD 1 TO IND
               IF IND < 4
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
           ACCEPT TCODDEP.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELA
                   GO TO R10.
           IF DEPFUNC = 0 
                 MOVE "*** DEPARTAMENTO NÃO INFORMADO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R11.
           PERFORM R11A.
           IF ST-ERRO = "23"
             MOVE "*** DEPARTAMENTO DIGITADO NAO ENCONTRADO ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R11
           DISPLAY TELA.
       
       R12.
           DISPLAY TELA   
           ACCEPT TCODCARGO.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELA
                   GO TO R11.
           IF CARGOFUNC = 0 
                 MOVE "*** CARGO NÃO INFORMADO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R12.
           PERFORM R12A.
           IF ST-ERRO = "23"
             MOVE "*** CARGO DIGITADO NAO ENCONTRADO ***" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R12
           DISPLAY TELA.

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
           WRITE REGFUNC
           IF ST-ERRO = "00" OR "02"
                 MOVE "*** DADOS GRAVADOS *** " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R1.
           IF ST-ERRO = "22"
             MOVE "* FUNCIONARIO JA EXISTE,DADOS NAO GRAVADOS *" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R1
           ELSE
                 MOVE "ERRO NA GRAVACAO DO ARQUIVO DE FUNCIONARIO"
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
                DELETE ARQFUNC RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO FUNCIONARIO EXCLUIDO ***" TO MENS
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
                REWRITE REGFUNC
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
               MOVE "ERRO NA EXCLUSAO DO REGISTRO FUNCIONARIO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *-------------------------------------------------------------------------------------------
       R11A.
           MOVE DEPFUNC TO CODDEP.
           READ ARQDEP
           IF ST-ERRO NOT = "00"
             MOVE "ERRO NA LEITURA ARQUIVO CADASTRO DEPARTANETO" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO ROT-FIM.
           DISPLAY TELA.
       R11A-FIM.
           GO TO R12.
       R12A.
           MOVE CARGOFUNC TO CODCARGO.
           READ ARQCARG
           IF ST-ERRO NOT = "00"
                  MOVE "ERRO NA LEITURA ARQUIVO CADASTRO CARGOS" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM.
           DISPLAY TELA.
       R12A-FIM.
           
       
      *-------------------------------------------------------------------------------------------------
       ROT-FIM.
           CLOSE ARQFUNC ARQCARG ARQDEP.
           EXIT PROGRAM.
           

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
        
               