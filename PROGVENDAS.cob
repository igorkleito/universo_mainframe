      ******************************************************************
      * Author: IGOR KLEITO
      * Date: 25.10.2023
      * Purpose: TRABALHO FINAL DO CURSO ALURA 201318
      *          O PROGR RECEBE VALORES DE VENDAS E OS ACUMULA POR MES.
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. PROGVENDAS.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES. DECIMAL-POINT IS COMMA.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       01 WS-VAR.
           02 WS-VENDAS PIC 9(006)V99 OCCURS 12 TIMES.
       77 WS-MESVENDA   PIC 9(002)    VALUE  0.
       77 WS-VALOR      PIC 9(006)V99 VALUE  0.

       PROCEDURE DIVISION.
       0000-PRINCIPAL.

           PERFORM 1000-INICIAR.
           PERFORM 2000-PROCESSAR     UNTIL WS-MESVENDA = 99.
           PERFORM 9000-CONTABILIZAR.
           STOP RUN.

       1000-INICIAR.

           DISPLAY 'OLA!'
           .
       2000-PROCESSAR.

           DISPLAY 'INFORME O MES DA VENDA: '
           ACCEPT     WS-MESVENDA.
           IF         WS-MESVENDA <> 99
               DISPLAY 'INFORME VALOR DA VENDA: '
               ACCEPT WS-VALOR
               ADD    WS-VALOR TO WS-VENDAS(WS-MESVENDA)
           END-IF
           .
       9000-CONTABILIZAR.

           PERFORM VARYING WS-MESVENDA FROM 1 BY 1
                                       UNTIL WS-MESVENDA > 12
           DISPLAY 'TOTAL DO MES ' WS-MESVENDA
                   ' = R$ '        WS-VENDAS(WS-MESVENDA)
           END-PERFORM.
