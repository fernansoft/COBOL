       IDENTIFICATION DIVISION.
       PROGRAM-ID. FPP013.
       AUTHOR. FERNANDO OLIVEIRA DA COSTA.
      **************************************************
      * CONSULTA DEPENDENTES*
      **************************************************
      *----------------------------------------------------------------
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
                    DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    
           SELECT ARQDEPE ASSIGN TO DISK
                   ORGANIZATION IS INDEXED
                   ACCESS MODE  IS DYNAMIC
                   RECORD KEY   IS CODDEPEN              
                   ALTERNATE RECORD KEY IS NOMEDEPEN WITH DUPLICATES
                   FILE STATUS  IS ST-ERRO.

           SELECT ARQDEP ASSIGN TO DISK
                       ORGANIZATION IS INDEXED
                       ACCESS MODE  IS DYNAMIC
                       RECORD KEY   IS CODDEP
                       FILE STATUS  IS ST-ERRO
                       ALTERNATE RECORD KEY IS NOMEDEP WITH DUPLICATES.

           SELECT ARQFUNC ASSIGN TO DISK
                   ORGANIZATION IS INDEXED
                   ACCESS MODE  IS DYNAMIC
                   RECORD KEY   IS CODFUNC
                   FILE STATUS  IS FUNC-ERRO
                   ALTERNATE RECORD KEY IS NOMEFUNC WITH DUPLICATES.
            
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
      *
       FD ARQDEPE
               LABEL RECORD IS STANDARD
               VALUE OF FILE-ID IS "ARQDEPE.DAT". 
       01 REGDEPE.
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
           03 NOMEFUNC PIC X(30).
           03 GENEROAUX PIC X(1).
           03 GENERO.
               05 GENERO1 PIC X(1).
               05 GENERO2 PIC X(26).
           03 STATUSAUX PIC X.
           03 STATUSF.
               05 STATUSF1 PIC X.
               05 STATUSF2 PIC X(15).
           03 DATANASC PIC 9(8).
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
      *
      *-----------------------------------------------------------------
       WORKING-STORAGE SECTION.
       01 W-SEL            PIC 9(01) VALUE ZEROS.
       01 W-CONT           PIC 9(04) VALUE ZEROS.
       01 W-OPCAO          PIC X(01) VALUE SPACES.
       01 ST-ERRO          PIC X(02) VALUE "00".
       01 FUNC-ERRO        PIC X(02) VALUE "00".
       01 W-ACT            PIC 9(02) VALUE ZEROS.
       01 MENS             PIC X(50) VALUE SPACES.
       01 LIMPA            PIC X(55) VALUE SPACES.
       01 SOLIC            PIC X(20) VALUE SPACES.
       01 CONLIN           PIC 9(03) VALUE 001.
       01 CODFUNCENTR          PIC 9(06) VALUE ZEROS.
       01 NOMEFUNCENTR         PIC X(30) VALUE SPACES.
       01 CODAUX         PIC 9(6) VALUE ZEROS.
      *-----------------------------------------------------------------
      *
      *************************
      * REPORT SECTION *
      *************************
      *
       01 DETALHE.
           03 DETSEQ            PIC 9.
           03 FILLER            PIC X(03) VALUE SPACES.
           03 DETNOME           PIC X(28) VALUE SPACES.
           03 FILLER            PIC X(01) VALUE SPACES.
           03 DETGRAU           PIC X(22) VALUE SPACES.
           03 DETDATANASC.
               05 DETDIA              PIC 9(2) VALUE ZEROS.
               05 FILLER           PIC X VALUE "/". 
               05 DETMES              PIC 9(2) VALUE ZEROS.
               05 FILLER           PIC X VALUE "/". 
               05 DETANO              PIC 9(4) VALUE ZEROS.
           03 FILLER            PIC X(01) VALUE SPACES.                                    
           03 DETSTATUS         PIC X(16) VALUE SPACES.
                 
      *
      ******************
      * ROTINA DE TELA *
      ******************
      *
       SCREEN SECTION.
      
       01  TELA.
           05  LINE 02  COLUMN 01 
               VALUE  "                        **** CONSULTA DE".
           05  LINE 02  COLUMN 41 
               VALUE  " DEPENDENTES ****".
           05  LINE 03  COLUMN 01 
               VALUE  "----------------------------------------".
           05  LINE 03  COLUMN 41 
               VALUE  "----------------------------------------".
           05  LINE 04  COLUMN 01 
               VALUE  " CODIGO DO FUNCIONARIO:           NOME:".
           05  LINE 06  COLUMN 01 
               VALUE  " DEPARTAMENTO:".
           05  LINE 06  COLUMN 41 
               VALUE  "     CARGO:".
           05  LINE 07  COLUMN 01 
               VALUE  "----------------------------------------".
           05  LINE 07  COLUMN 41 
               VALUE  "----------------------------------------".
           05  LINE 08  COLUMN 01 
               VALUE  "                            **** DEPENDE".
           05  LINE 08  COLUMN 41 
               VALUE  "NTES ****".
           05  LINE 09  COLUMN 01 
               VALUE  "SEQ     NOME DO DEPENDENTE         GRA".
           05  LINE 09  COLUMN 39 
               VALUE  "U PARENTESCO     DATA NASC     STATUS".
           05  LINE 10  COLUMN 01 
               VALUE  "--- ---------------------------- -----".
           05  LINE 10  COLUMN 39 
               VALUE  "---------------- ---------- -------------".
           05  LINE 11  COLUMN 01  
               VALUE "                                                ".
           05  LINE 11  COLUMN 40  
               VALUE "                                                ".               
           05  LINE 12  COLUMN 01  
               VALUE "                                                ".
           05  LINE 12  COLUMN 40  
               VALUE "                                                ".    
           05  LINE 13  COLUMN 01  
               VALUE "                                                ".
           05  LINE 13  COLUMN 40  
               VALUE "                                                ".
           05  LINE 14  COLUMN 01  
               VALUE "                                                ".
           05  LINE 14  COLUMN 40  
           VALUE "                                                ".
           05  LINE 15  COLUMN 01  
               VALUE "                                                ".
           05  LINE 15  COLUMN 40  
               VALUE "                                                ".
           05  LINE 16  COLUMN 01  
               VALUE "                                                ".
           05  LINE 16  COLUMN 40  
               VALUE "                                                ".
           05  LINE 17  COLUMN 01  
               VALUE "                                                ".
           05  LINE 17  COLUMN 40  
               VALUE "                                                ".
           05  LINE 18  COLUMN 01  
               VALUE "                                                ".
           05  LINE 18  COLUMN 40  
               VALUE "                                                ".               
           05  LINE 19  COLUMN 01  
               VALUE "                                                ".
           05  LINE 19  COLUMN 40  
               VALUE "                                                ".               
           05  LINE 20  COLUMN 01  
               VALUE "                                                ".
           05  LINE 20  COLUMN 40  
               VALUE "                                                ".
           05  LINE 21  COLUMN 01  
               VALUE "                                                ".
           05  LINE 21  COLUMN 40  
               VALUE "                                                ".           
           05  LINE 23  COLUMN 01 
               VALUE  "MENSAGEM :".
           05  LINE 23  COLUMN 41 
               VALUE  "                                 FPP013".
           
           05  TCODFUNCENTR
               LINE 04  COLUMN 25  PIC 9(06)
               USING  CODFUNCENTR.
           05  TNOMEFUNCENTR
               LINE 04  COLUMN 41  PIC X(30)
               USING  NOMEFUNCENTR.
           05  TCODDEP
               LINE 06  COLUMN 16  PIC 9(03)
               USING  CODDEP.
           05  TNOMEDEP
               LINE 06  COLUMN 20  PIC X(25)
               USING  NOMEDEP.
           05  TCODCARGO
               LINE 06  COLUMN 53  PIC 9(03)
               USING  CODCARGO.
           05  TNOMECARGO
               LINE 06  COLUMN 57  PIC X(24)
               USING  NOMECARGO.

      *-----------------------------------------------------------------
       PROCEDURE DIVISION.
       INICIO.
      *
       INC-OP1.
           OPEN INPUT  ARQFUNC
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE ARQFUNC NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                IF ST-ERRO = "95"
                    MOVE "ISAM NAO CARREGADO" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-FIM2
                ELSE
                  MOVE "ERRO ABERTURA DO ARQUIVO ARQFUNC"  TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM2.
      *
       INC-OP2.
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
       INC-OP3.
           OPEN INPUT  ARQDEP
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE ARQDEP NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                IF ST-ERRO = "95"
                    MOVE "ISAM NAO CARREGADO" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-FIM2
                ELSE
                  MOVE "ERRO ABERTURA DO ARQUIVO ARQDEP"  TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM2.
      *
       INC-OP4.           
           OPEN INPUT  ARQDEPE
           IF ST-ERRO NOT = "00"
              IF ST-ERRO = "30"
                 MOVE "ARQUIVO DE ARQDEP NAO EXISTE" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM2
              ELSE
                IF ST-ERRO = "95"
                    MOVE "ISAM NAO CARREGADO" TO MENS
                    PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO ROT-FIM2
                ELSE
                  MOVE "ERRO ABERTURA DO ARQUIVO ARQDEP"  TO MENS
                  PERFORM ROT-MENS THRU ROT-MENS-FIM
                  GO TO ROT-FIM2.                       
      *
       INC-001.
           MOVE ZEROS TO CODFUNCENTR CARGOFUNC DEPFUNC CODCARGO CODFUNC
           MOVE ZEROS TO CODDEP CODAUX
           MOVE SPACES TO NOMEFUNCENTR NOMEDEP NOMECARGO NOMEFUNC
           DISPLAY  TELA.
      *
       INC-001A.
           DISPLAY (23, 13)
            "SOLIC.=> F1=ENCERRA CONSULTA   F2=CONSULTA P/NOMEFUNC."
           ACCEPT TCODFUNCENTR
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE ARQFUNC
                      GO TO ROT-FIM.
           IF W-ACT = 03
                      GO TO INC-001B.
           IF W-ACT > 03
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
           MOVE CODFUNCENTR TO CODFUNC
           MOVE CODFUNC TO DEPENFUNC
           PERFORM MONTA-FUNCA
           MOVE 0 TO SEQUENCIA           
           START ARQDEPE KEY IS NOT LESS CODDEPEN INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001.
           GO TO INC-RD2.
      *
       INC-001B.
           DISPLAY (23, 13)
            "SOLIC.=> F1=ENCERRA CONSULTA    F2=CONSULTA P/CODIGOFUNC"
           ACCEPT TNOMEFUNCENTR
           ACCEPT W-ACT FROM ESCAPE KEY
           IF W-ACT = 02
                      CLOSE ARQFUNC
                      GO TO ROT-FIM.
           IF W-ACT = 03
                      GO TO INC-001A.
           IF W-ACT > 03
                      MOVE "*** FUNCAO NAO DEFINIDA ***" TO MENS
                      PERFORM ROT-MENS THRU ROT-MENS-FIM
                      GO TO INC-001A.
           MOVE NOMEFUNCENTR TO NOMEFUNC
           PERFORM MONTA-FUNCB      
           MOVE CODFUNC TO DEPENFUNC           
           MOVE 0 TO SEQUENCIA
           START ARQDEPE KEY IS NOT LESS CODDEPEN INVALID KEY
                 MOVE "*** CHAVE NAO ENCONTRADA ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO INC-001B.
      * 
       INC-RD2.
           DISPLAY (23, 13) LIMPA               
           READ ARQDEPE NEXT 
           
      * LISTAR POR CODIGO DE FUNCIONARIO E GARANTIR QUE SEJA MAIOR QUE 0
      * MESMO EM CASO DE BUSCAR POR NOME
     
           IF CODAUX = 0
               MOVE DEPENFUNC TO CODAUX.
           
           IF DEPENFUNC > CODAUX                
                   MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO NOV-SOL.

           IF DEPENFUNC > CODFUNCENTR                
               IF CODFUNCENTR NOT = 0
                   MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                   PERFORM ROT-MENS THRU ROT-MENS-FIM
                   GO TO NOV-SOL.
                        
           IF ST-ERRO NOT = "00" 
              IF ST-ERRO = "10"
                 MOVE "*** FIM DO ARQUIVO  ***" TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO NOV-SOL
              ELSE
                 MOVE "ERRO NA LEITURA DO ARQUIVO ARQFUNC"  TO MENS
                 PERFORM ROT-MENS THRU ROT-MENS-FIM
                 GO TO ROT-FIM
           ELSE
              NEXT SENTENCE.
      *        
       ROT-MONTAR.
           MOVE SEQUENCIA      TO DETSEQ
           MOVE NOMEDEPEN      TO DETNOME
           MOVE GRAUPARENTESCO TO DETGRAU
           MOVE DIA            TO DETDIA
           MOVE MES            TO DETMES
           MOVE ANO            TO DETANO
           MOVE DEPEN-STATUS   TO DETSTATUS            
           COMPUTE LIN = CONLIN + 10
           DISPLAY (LIN, 01) DETALHE
           ADD 1 TO CONLIN
           IF CONLIN < 12
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
       
       MONTA-FUNCA.
           READ ARQFUNC
           MOVE CARGOFUNC TO CODCARGO
           MOVE DEPFUNC TO CODDEP
           READ ARQCARG
           READ ARQDEP
           MOVE NOMEFUNC TO NOMEFUNCENTR
           DISPLAY TELA.
       
       MONTA-FUNCB.
           READ ARQFUNC KEY IS NOMEFUNC
           MOVE CARGOFUNC TO CODCARGO
           MOVE DEPFUNC TO CODDEP
           READ ARQCARG
           READ ARQDEP
           MOVE NOMEFUNC TO NOMEFUNCENTR
           DISPLAY TELA.           
      *
      **********************
      * ROTINA DE FIM      *
      **********************
      *
       ROT-FIM.
           DISPLAY (01 01) ERASE.
           CLOSE ARQCARG ARQDEP ARQDEPE ARQFUNC.
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