
       01  TELA.
           05  LINE 02  COLUMN 01 
               VALUE  "                             *** CADASTR".
           05  LINE 02  COLUMN 41 
               VALUE  "O DE CEP ***".
           05  LINE 04  COLUMN 01 
               VALUE  "    CEP:".
           05  LINE 06  COLUMN 01 
               VALUE  "    LOGRADOURO:".
           05  LINE 08  COLUMN 01 
               VALUE  "    BAIRRO:".
           05  LINE 10  COLUMN 01 
               VALUE  "    CIDADE:".
           05  LINE 12  COLUMN 01 
               VALUE  "    UF:".
           05  LINE 14  COLUMN 01 
               VALUE  "    PONTO REF.:".
           05  LINE 16  COLUMN 01 
               VALUE  "    STATUS:".
           05  LINE 18  COLUMN 01 
               VALUE  "    OBSERVACAO:".
           05  LINE 20  COLUMN 01 
               VALUE  "    DATA INC.MAN:".
           05  LINE 23  COLUMN 01 
               VALUE  "MENSAGEM:".
           05  TCEP
               LINE 04  COLUMN 10  PIC Z(08)
               USING  CEP
               HIGHLIGHT.
           05  TLOGRADOURO
               LINE 06  COLUMN 17  PIC Z(35)
               USING  LOGRADOURO
               HIGHLIGHT.
           05  TBAIRRO
               LINE 08  COLUMN 13  PIC Z(20)
               USING  BAIRRO
               HIGHLIGHT.
           05  TCIDADE
               LINE 10  COLUMN 13  PIC Z(20)
               USING  CIDADE
               HIGHLIGHT.
           05  TUF
               LINE 12  COLUMN 09  PIC Z(02)
               USING  UF
               HIGHLIGHT.
           05  TPONTOREF
               LINE 14  COLUMN 17  PIC Z(35)
               USING  PONTOREF
               HIGHLIGHT.
           05  TSTATUS
               LINE 16  COLUMN 13  PIC Z(01)
               USING  STATS
               HIGHLIGHT.
           05  TOBS
               LINE 18  COLUMN 17  PIC Z(40)
               USING  OBS
               HIGHLIGHT.
           05  TDATA-INC-MAN
               LINE 20  COLUMN 19  PIC Z(08)
               USING  DATA-INC-MAN
               HIGHLIGHT.
