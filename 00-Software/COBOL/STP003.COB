       IDENTIFICATION DIVISION.
       PROGRAM-ID. STP003.
       AUTHOR. FERNANDO OLIVEIRA DA COSTA.
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
             DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT CADPROD ASSIGN TO DISK
                       ORGANIZATION IS INDEXED
                       ACCESS MODE  IS DYNAMIC
                       RECORD KEY   IS CODPROD
                       ALTERNATE RECORD KEY IS CNPJFORN WITH DUPLICATES
                       FILE STATUS  IS ST-ERRO.
           
           SELECT CADFORN ASSIGN TO DISK
                       ORGANIZATION IS INDEXED
                       ACCESS MODE  IS DYNAMIC
                       RECORD KEY   IS CNPJ
                       ALTERNATE RECORD KEY IS NOME WITH DUPLICATES
                       FILE STATUS  IS ST-ERRO.
    
           SELECT CADCEP ASSIGN TO DISK
                       ORGANIZATION IS INDEXED
                       ACCESS MODE  IS DYNAMIC
                       RECORD KEY   IS CEP
                       FILE STATUS  IS ST-ERRO
                       ALTERNATE RECORD KEY IS DATA-INC-MAN
                                      WITH DUPLICATES.
      *
      *-----------------------------------------------------------------
       DATA DIVISION.
       FILE SECTION.

       FD CADPROD
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADPROD.DAT". 
       01 REGPROD.
           03 CODPROD       PIC 9(06).
           03 DESC          PIC X(30).
           03 UNIDADEAUX    PIC X(02).
           03 UNIDADE.
               05 UNIDADE1  PIC X(02).
               05 UNIDADE2  PIC X(12).
           03 APLICACAOAUX  PIC 9.
           03 APLICACAO.
               05 APLICACAO1 PIC 9(1).
               05 APLICACAO2 PIC X(19).
           03 QUANTIDADE    PIC 9(05)V9.
           03 PRECO         PIC 9(07)V99.
           03 CNPJFORN      PIC 9(15).

       FD CADFORN
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADFORN.DAT".
       01 REGFORN.
           03 CNPJ          PIC 9(15).
           03 RAZAO         PIC X(40).
           03 NOME          PIC X(12).
           03 NUMERO        PIC X(04).
           03 COMPLEMENTO   PIC X(12).
           03 CEPFORN       PIC 9(09).
      *
      *-----------------------------------------------------------------
       FD CADCEP
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "CADCEP.DAT".
       01 REGCEP.
           03 CEP          PIC 9(09).
           03 LOGRADOURO   PIC X(35).
           03 BAIRRO       PIC X(20).   
           03 CIDADE       PIC X(20).
           03 UFAUX        PIC X(02).
           03 UFAUX2       PIC X(20).
           03 PONTOREF     PIC X(35).
           03 STATS        PIC 9(01).
           03 OBS          PIC X(40).
           03 DATA-INC-MAN.
               07 IGN      PIC 9(02).
               07 MES      PIC 9(02).
               07 DIA      PIC 9(02).
           03 ANO          PIC 9(04).

       WORKING-STORAGE SECTION.
       
       01 W-CONT     PIC 9(06) VALUE ZEROS.
       01 W-OPCAO    PIC X(01) VALUE SPACES.
       01 OPCAO      PIC X(01) VALUE SPACES.
       01 W-ACT      PIC 9(02) VALUE ZEROS.
       01 MENS       PIC X(50) VALUE SPACES.
       01 LIMPA      PIC X(50) VALUE SPACES. 
       01 ST-ERRO    PIC X(02) VALUE "00".
       01 W-SEL      PIC 9(01) VALUE ZEROS.
       01 IND        PIC 9(2) VALUE ZEROS.

       01 TABAPLICACAOX.
           03 FILLER     PIC X(19) VALUE "1- SAUDE".
           03 FILLER     PIC X(19) VALUE "2- HIGIENE PESSOAL".
           03 FILLER     PIC X(19) VALUE "3- ALIMENTO".
           03 FILLER     PIC X(19) VALUE "4- LIMPESA".
           03 FILLER     PIC X(19) VALUE "5- SEGURANCA".
           03 FILLER     PIC X(19) VALUE "6- GESTAO".
           03 FILLER     PIC X(19) VALUE "7- VESTUARIO".
           03 FILLER     PIC X(19) VALUE "8- OUTRAS".
         
       
       01 TABAPLICACAO REDEFINES TABAPLICACAOX.
           03 TBAPLICACAO   PIC X(19) OCCURS 8 TIMES.
       
       01 TABAPLICACAO.
           03 TABAPLICACAO1 PIC X(02).
           03 TABAPLICACAO2 PIC X(17).

       01 TABUNIDADEX.
           03 FILLER     PIC X(14) VALUE "LT- LITRO".
           03 FILLER     PIC X(14) VALUE "PC- PECA".
           03 FILLER     PIC X(14) VALUE "KG- KILO".
           03 FILLER     PIC X(14) VALUE "TN- TONELADA".
           03 FILLER     PIC X(14) VALUE "AR- ARROBA".
           03 FILLER     PIC X(14) VALUE "GR- GRAMA".
           03 FILLER     PIC X(14) VALUE "MT- METRO".
           03 FILLER     PIC X(14) VALUE "CM- CENTIMETRO".
           03 FILLER     PIC X(14) VALUE "CJ- CONJUNTO".
           
      *
       01 TABUNIDADE REDEFINES TABUNIDADEX.
           03 TBUNIDADE    PIC X(14) OCCURS 9 TIMES.
      *
       01 TABUNIDADE.
           03 TABUNIDADE1  PIC X(02).
           03 TABUNIDADE2  PIC X(12).
       
      *---------------------------------------------------------------- 

       SCREEN SECTION.
        01  TELA.
           05  LINE 02  COLUMN 30 VALUE  "CADASTRO DE PRODUTOS".
           05  LINE 05  COLUMN 06 VALUE  "CODIGO:".
           05  LINE 05  COLUMN 34 VALUE  "DESCRICAO:".
           05  LINE 07  COLUMN 06 VALUE  "UNIDADE:".
           05  LINE 07  COLUMN 34 VALUE  "APLICACAO:".
           05  LINE 09  COLUMN 06 VALUE  "QUANTIDADE:".
           05  LINE 09  COLUMN 34 VALUE  "PRECO COMPRA:".
           05  LINE 10  COLUMN 03 VALUE  "----------------------------".
           05  LINE 10  COLUMN 31 VALUE  "----------------------------".
           05  LINE 10  COLUMN 60 VALUE  "--------------------".
           05  LINE 11  COLUMN 34 VALUE  "FORNECEDORES:".
           05  LINE 12  COLUMN 06 VALUE  "CNPJ:".
           05  LINE 14  COLUMN 06 VALUE  "NOME FANTASIA:".
           05  LINE 16  COLUMN 06 VALUE  "CEP:".
           05  LINE 16  COLUMN 33 VALUE  "ENDERECO:".
           05  LINE 18  COLUMN 06 VALUE  "NUMERO:".
           05  LINE 18  COLUMN 21 VALUE  "COMPLEMENTO:".
           05  LINE 18  COLUMN 48 VALUE  "BAIRRO:".
           05  LINE 20  COLUMN 06 VALUE  "CIDADE:".
           05  LINE 20  COLUMN 36 VALUE  "UF:".
           
           05  TCODIGO     LINE 05  COLUMN 14  PIC 999.999
               USING  CODPROD.
           05  TDESC       LINE 05  COLUMN 45  PIC X(30)
               USING  DESC.
           05  TUNIDADE    LINE 07  COLUMN 15  PIC X(02)
               USING  UNIDADEAUX.
           05  TUNIDADE2   LINE 07  COLUMN 18  PIC X(12)
               USING  UNIDADE2.
           05  TAPLICACAO  LINE 07  COLUMN 45  PIC 9(01)
               USING  APLICACAOAUX.
           05  TAPLICACAO2 LINE 07  COLUMN 47  PIC X(19)
               USING  APLICACAO2.
           05  TQUANTIDADE LINE 09  COLUMN 18  PIC ZZ.ZZ9,9
               USING  QUANTIDADE.
           05  TPRECO      LINE 09  COLUMN 48  PIC ZZZZ.ZZ9,99
               USING  PRECO.
           05  TCNPJ       LINE 12  COLUMN 12  PIC 999.999.999.9999.99
               USING  CNPJFORN.
           05  TNOME       LINE 14  COLUMN 21  PIC X(12)
               USING  NOME.
           05  TCEP        LINE 16  COLUMN 11  PIC 99999.999
               USING  CEP.
           05  TLOGRA      LINE 16  COLUMN 43  PIC X(35)
               USING  LOGRADOURO.
           05  TNUMERO     LINE 18  COLUMN 14  PIC 9(04)
               USING  NUMERO.
           05  TCOMP       LINE 18  COLUMN 34  PIC X(12)
               USING  COMPLEMENTO.
           05  TBAIRRO     LINE 18  COLUMN 56  PIC X(20)
               USING  BAIRRO.
           05  TCIDADE     LINE 20  COLUMN 14  PIC X(20)
               USING  CIDADE.
           05  TUF         LINE 20  COLUMN 40  PIC X(02)
               USING  UFAUX.
           05  TUF2        LINE 20  COLUMN 43  PIC X(26)
               USING  UFAUX2.
       
       01  TELAUNI.
           05  LINE 13  COLUMN 58 VALUE  " ----------------- ".      
           05  LINE 14  COLUMN 58 VALUE  "| LT - LITRO      |".
           05  LINE 15  COLUMN 58 VALUE  "| PC - PECA       |".
           05  LINE 16  COLUMN 58 VALUE  "| KG - KILO       |".
           05  LINE 17  COLUMN 58 VALUE  "| TN - TONELADA   |".
           05  LINE 18  COLUMN 58 VALUE  "| AR - ARROBA     |".
           05  LINE 19  COLUMN 58 VALUE  "| GR - GRAMA      |".
           05  LINE 20  COLUMN 58 VALUE  "| MT - METRO      |".
           05  LINE 21  COLUMN 58 VALUE  "| CM - CENTIMETRO |".
           05  LINE 22  COLUMN 58 VALUE  "| CJ - CONJUNTO   |".
           05  LINE 23  COLUMN 58 VALUE  " ----------------- ".
       
       01  TELAAP.
           05  LINE 13  COLUMN 58 VALUE  " --------------------- ".      
           05  LINE 14  COLUMN 58 VALUE  "| 1 - SAUDE           |".
           05  LINE 15  COLUMN 58 VALUE  "| 2 - HIGIENE PESSOAL |".
           05  LINE 16  COLUMN 58 VALUE  "| 3 - ALIMENTO        |".
           05  LINE 17  COLUMN 58 VALUE  "| 4 - LIMPESA         |".
           05  LINE 18  COLUMN 58 VALUE  "| 5 - SEGURANCA       |".
           05  LINE 19  COLUMN 58 VALUE  "| 6 - GESTAO          |".
           05  LINE 20  COLUMN 58 VALUE  "| 7 - VESTUARIO       |".
           05  LINE 21  COLUMN 58 VALUE  "| 8 - OUTRAS          |".
           05  LINE 22  COLUMN 58 VALUE  " --------------------- ".
       
       01 TELALIMPA.
           05  LINE 13  COLUMN 58 VALUE  "                       ".      
           05  LINE 14  COLUMN 58 VALUE  "                       ".
           05  LINE 15  COLUMN 58 VALUE  "                       ".
           05  LINE 16  COLUMN 58 VALUE  "                       ".
           05  LINE 17  COLUMN 58 VALUE  "                       ".
           05  LINE 18  COLUMN 58 VALUE  "                       ".
           05  LINE 19  COLUMN 58 VALUE  "                       ".
           05  LINE 20  COLUMN 58 VALUE  "                       ".
           05  LINE 21  COLUMN 58 VALUE  "                       ".
           05  LINE 22  COLUMN 58 VALUE  "                       ".
           05  LINE 23  COLUMN 58 VALUE  "                       ".

      *----------------------------------------------------------------  
      
       PROCEDURE DIVISION.
       
       R0.
           OPEN I-O CADPROD
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 OPEN OUTPUT CADPROD
                 CLOSE CADPROD
                 MOVE "*** ARQUIVO CADPROD FOI CRIADO **" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R0
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO CADPROD" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.

       R0A.
           OPEN INPUT CADFORN
           IF ST-ERRO NOT = "00"  
              IF ST-ERRO = "30"
                 MOVE "* ARQUIVO DE FORNECEDOR NAO ENCONTRADO *" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
              ELSE
                 MOVE "ERRO NA ABERTURA DO ARQUIVO DE CEP " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
                 NEXT SENTENCE.
      * 
       R0B.
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
      *------------[ INICIALIZACAO DAS VARIAVEIS ]---------------------               
       R1.
           MOVE SPACES TO RAZAO NOME COMPLEMENTO LOGRADOURO BAIRRO
           MOVE SPACES TO CIDADE UFAUX UFAUX2 DESC UNIDADEAUX UNIDADE2 
           MOVE SPACES TO APLICACAO2   
           MOVE ZEROS TO CNPJFORN CEP NUMERO CODPROD APLICACAOAUX
           MOVE ZEROS TO  QUANTIDADE PRECO 
      *-------------[VISUALIZACAO DA TELA]--------------------------------
           DISPLAY TELA.
       R2.
           ACCEPT TCODIGO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO ROT-FIM.
           IF CODPROD = 0
                   MOVE "O CODIGO DEVE SER MAIOR QUE ZERO" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R2. 
       LER-CADPROD.
           READ CADPROD
           IF ST-ERRO NOT = "23"
              IF ST-ERRO = "00"
                PERFORM R8A
                DISPLAY TELA
                MOVE "*** PRODUTO JA CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ACE-001
             ELSE
                MOVE "ERRO NA LEITURA ARQUIVO CADPROD" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM
           ELSE
                MOVE "*** PRODUTO NAO CADASTRAD0 ***" TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM.
       R3.       
           ACCEPT TDESC
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R2.

       R4.
           MOVE 1 TO IND
           DISPLAY TELALIMPA
           DISPLAY (23, 14) "                                        "
           DISPLAY (23, 30) "                                        "
           DISPLAY TELAUNI
           
           ACCEPT TUNIDADE
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R3.
       
       R4A.
           MOVE TBUNIDADE(IND) TO UNIDADE
           IF UNIDADE1 NOT = UNIDADEAUX
               ADD 1 TO IND
               IF IND < 28
                   GO TO R4A
               ELSE
                   DISPLAY (23, 14) "TIPO DE UNIDADE INCORRETA"
                   STOP "  DIGITE ENTER PARA CONTINUAR"                  
                   GO TO R4
           ELSE
               DISPLAY TUNIDADE2.
       
       R5.
           MOVE 1 TO IND
           DISPLAY (23, 14) "                                        "
           DISPLAY (23, 30) "                                        "
           DISPLAY TELAAP
           ACCEPT TAPLICACAO
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R4.
       
       R5A.
           MOVE TBAPLICACAO(IND) TO APLICACAO
           IF APLICACAO1 NOT = APLICACAOAUX
               ADD 1 TO IND
               IF IND < 28
                   GO TO R5A
               ELSE
                   DISPLAY (23, 14) "TIPO DE APLICACAO INCORRETA"
                   STOP "  DIGITE ENTER PARA CONTINUAR"                  
                   GO TO R5
           ELSE
               DISPLAY TAPLICACAO2.
       
       R6.
           DISPLAY TELALIMPA
           DISPLAY TELA
           ACCEPT TQUANTIDADE.  
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R5.
       
       R7.
           ACCEPT TPRECO.  
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   GO TO R6.

       R8.
           ACCEPT TCNPJ.
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 01
                   DISPLAY TELA
                   GO TO R7.
           IF CNPJFORN = 0 
                 MOVE "*** CNPJ NÃO INFORMADO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R8.
           PERFORM R8A.
           IF ST-ERRO = "23"
                   MOVE "*** CNPJ DIGITADO NAO ENCONTRADO ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R8
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
           WRITE REGPROD
           IF ST-ERRO = "00" OR "02"
                 MOVE "*** DADOS GRAVADOS *** " TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO R1.
           IF ST-ERRO = "22"
             MOVE "* PRODUTO JA EXISTE,DADOS NAO GRAVADOS *" TO MENS
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
                DELETE CADPROD RECORD
                IF ST-ERRO = "00"
                   MOVE "*** REGISTRO PRODUTO EXCLUIDO ***" TO MENS
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
                REWRITE REGPROD
                IF ST-ERRO = "00" OR "02"
                   MOVE "*** REGISTRO ALTERADO ***         " TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO R1.
                MOVE "ERRO NA EXCLUSAO DO REGISTRO PRODUTO"   TO MENS
                PERFORM ROT-MENS THRU ROT-MENS-FIM
                GO TO ROT-FIM.
      *-------------------------------------------------------------------------------------------
       R8A.
           MOVE CNPJFORN TO CNPJ.
           READ CADFORN
           IF ST-ERRO NOT = "00"
              MOVE "ERRO NA LEITURA ARQUIVO CADASTRO FORNECEDOR" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ROT-FIM.
           MOVE CEPFORN TO CEP
           READ CADCEP
           IF ST-ERRO NOT = "00"
              MOVE "ERRO NA LEITURA ARQUIVO CADASTRO CEP" TO MENS
               PERFORM ROT-MENS THRU ROT-MENS-FIM
               GO TO ROT-FIM.
           DISPLAY TELA.
       R8A-FIM.
      *-------------------------------------------------------------------------------------------------
       ROT-FIM.
           CLOSE CADPROD CADFORN CADCEP.
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