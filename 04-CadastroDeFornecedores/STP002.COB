       IDENTIFICATION DIVISION.
       PROGRAM-ID. STP002.
      *AUTHOR. FERNANDO OLIVEIRA DA COSTA.
      **************************************
      * MANUTENCAO DO CADASTRO DE ENDERECOS  *
      **************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CADFORN ASSIGN TO DISK
                          ORGANIZATION IS INDEXED
                          ACCESS MODE  IS DYNAMIC
                          RECORD KEY   IS CNPJ
                          ALTERNATE RECORD KEY IS NOME WITH DUPLICATES
                          FILE STATUS  IS ST-ERRO.
    
           SELECT CADCEP ASSIGN TO DISK
                        ORGANIZATION IS INDEXED
                        ACCESS MODE  IS DYNAMIC
                        RECORD KEY   IS CODCEP
                        FILE STATUS  IS ST-ERRO
                        ALTERNATE RECORD KEY IS DATAMOD
                                       WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.
       FD CADFORN
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADFORN.DAT". 
       01 REGFORN.
          03 CNPJ          PIC 9(15).
          03 RAZAO         PIC X(40).
          03 NOME          PIC X(12).
          03 NUMERO        PIC X(04).
          03 COMPLEMENTO   PIC X(12).
          03 CEPFORN       PIC 9(9).
      *
      *-----------------------------------------------------------------
       FD CADCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT".
       01 REGCEP.
           03 CODCEP PIC 9(9).
           03 LOGRADOURO PIC X(35).
           03 BAIRRO PIC X(20).   
           03 CIDADE PIC X(20).
           03 STATUSAUX PIC X(1).
           03 STATUSC.
               05 STATUS1 PIC X(1).
               05 STATUS2 PIC X(14).
           03 UFAUX PIC X(2).
           03 UF.
               05 UF1 PIC X(2).
               05 UF2 PIC X(26).
           03 PONTOREFERENCIA PIC X(35).
           03 OBSERVACAO PIC X(35).
           03 DATAMOD PIC 9(4).

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
       
      *----------------------------------------------------------------- 

       SCREEN SECTION.
       01  TELA.
           05  LINE 01  COLUMN 27  VALUE  "CADASTRO DE FORNECEDORES".
           05  LINE 04  COLUMN 07  VALUE  "CNPJ:".
           05  LINE 06  COLUMN 07  VALUE  "RAZAO SOCIAL:".
           05  LINE 08  COLUMN 07  VALUE  "NOME FANTASIA:".
           05  LINE 10  COLUMN 07  VALUE  "CEP:".
           05  LINE 11  COLUMN 01  VALUE  "___________________________".
           05  LINE 11  COLUMN 28  VALUE  "___________________________".
           05  LINE 13  COLUMN 07  VALUE  "ENDERECO:".
           05  LINE 13  COLUMN 54  VALUE  "NUMERO:".
           05  LINE 15  COLUMN 07  VALUE  "BAIRRO:".
           05  LINE 15  COLUMN 54  VALUE  "COMPLEMENTO:".
           05  LINE 17  COLUMN 07  VALUE  "CIDADE:".
           05  LINE 19  COLUMN 07  VALUE  "UF:".
           05  TCNPJ   LINE 04 COLUMN 13 PIC 999.999.999.9999.99
               USING  CNPJ.
           05  TRAZAO  LINE 06 COLUMN 21 PIC X(40) USING  RAZAO.
           05  TNOME   LINE 08 COLUMN 22 PIC X(12) USING  NOME.
           05  TCEP    LINE 10 COLUMN 12 PIC 99999.999 USING CEPFORN.
           05  TLOGRA  LINE 13 COLUMN 17 PIC X(35) USING  LOGRADOURO.
           05  TNUMERO LINE 13 COLUMN 62 PIC 9(04) USING  NUMERO.
           05  TBAIRRO LINE 15 COLUMN 15 PIC X(20) USING  BAIRRO.
           05  TCOMP   LINE 15 COLUMN 67 PIC X(12) USING  COMPLEMENTO.
           05  TCIDADE LINE 17 COLUMN 15 PIC X(20) USING  CIDADE.
           05  TUF     LINE 19 COLUMN 11 PIC X(02) USING  UFAUX.
           05  TUF2    LINE 19 COLUMN 14 PIC X(26) USING  UF2.

      *-----------------------------------------------------------------  
      
       PROCEDURE DIVISION.
       
       R0.
           OPEN I-O CADFORN
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADFORN
                 CLOSE CADFORN
                 MOVE "*** ARQUIVO CADFORN FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADFORN" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      * 
       R0A.
           OPEN INPUT CADCEP
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "*** ARQUIVO DE CEP NAO ENCONTRADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE CEP " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      *-----------------[ INICIALIZACAO DAS VARIAVEIS ]-----------------
       R1.
           MOVE SPACES TO RAZAO NOME COMPLEMENTO LOGRADOURO BAIRRO
           MOVE SPACES TO CIDADE UFAUX UF2      
           MOVE ZEROS TO CNPJ CEPFORN NUMERO   
      *--------------------[ VISUALIZACAO DA TELA ]---------------------
           DISPLAY TELA.
       R2.
           ACCEPT TCNPJ
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
       LER-CADFORN.
           READ CADFORN
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                PERFORM R5A
                DISPLAY TELA
                MOVE "*** FORNECEDOR JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADFORN" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** FORNECEDOR NAO CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.
       R3.       
           ACCEPT TRAZAO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.
 
       R4.
           ACCEPT TNOME.  
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.
       R5.
           ACCEPT TCEP.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELA
                   GO TO R4.
           IF CEPFORN = 0 
                 MOVE "*** CEP NÃO INFORMADO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R5.
           PERFORM R5A.
           IF ST-ERRO = "23"
                   MOVE "*** CEP DIGITADO NAO ENCONTRADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R5
           DISPLAY TELA.
       R6.
           ACCEPT TNUMERO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELA
                   GO TO R5.
           IF NUMERO = 0 
                 MOVE "** NUMERO RESIDENCIA NÃO INFORMADO  **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R6.
      
       R7.
           ACCEPT TCOMP
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELA
                   GO TO R6.

      *-------------------[ VERICAR SE E ALTERACAO ]--------------------
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
           WRITE REGFORN
           IF ST-ERRO = "00" OR "02"
                 MOVE "*** DADOS GRAVADOS *** " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R1.
           IF ST-ERRO = "22"
             MOVE "* FORNECEDOR JA EXISTE,DADOS NAO GRAVADOS *" TO MENS
             PERFORM ROT-MENS THRU ROT-MENS-FIM
             GO TO R1
           ELSE
                 MOVE "ERRO NA GRAVACAO DO ARQUIVO DE PRODUTO"
                                                  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM.

      *
      ******************************************************************
      ************* ROTINA DE CONSULTA/ALTERACAO/EXCLUSAO **************
      ******************************************************************
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
                DELETE CADFORN RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO FORNECEDOR EXCLUIDO ***" TO MENS
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
                REWRITE REGFORN
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO FORNECEDOR"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *-----------------------------------------------------------------
       R5A.
           MOVE CEPFORN TO CODCEP.
           READ CADCEP
           IF ST-ERRO NOT = "00"
                  MOVE "ERRO NA LEITURA ARQUIVO CADASTRO CEP" TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM.
           DISPLAY TELA.
       R5A-FIM.
      *-----------------------------------------------------------------
       ROT-FIM.
           CLOSE CADFORN CADCEP.
           STOP RUN.

      *---------------------[ ROTINA DE MENSAGEM ]----------------------
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