      ******************************************************************
      * Author: JOSE CELESTINO
      * Date: 20-01-2023
      * Purpose: ESTUDO DE COBOL
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. DESAFIOM2.
       AUTHOR. JOSE VITOR PESQUEIRA CELESTINO.
       DATE-WRITTEN. 20-01-2023.
       DATE-COMPILED. 20-01-2023.

      * ESTE É UM PROJETO QUE TEM COMO OBJETIVO Imputar e calcular dados
      * de notas, processar média e gerar display de saída.

       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77 WS-SIGNAL                PIC X   VALUE 'Y'.
       01 WS-INFO.
          03 WS-ALUNO              PIC X(40).
          03 WS-MATERIA            PIC X(20).
          03 WS-NOTA-1             PIC 9(02)V99.
          03 WS-NOTA-2             PIC 9(02)V99.   
          03 WS-NOTA-3             PIC 9(02)V99.   
          03 WS-NOTA-4             PIC 9(02)V99.   
          03 WS-MEDIA              PIC 9(02)V99.
       01 WS-STATUS                PIC X(9) VALUE "REPROVADO".
          88 WS-CONFIRM            VALUE "APROVADO" FALSE "REPROVADO".
          
       PROCEDURE DIVISION.
       P100-INICIAR.
            INITIALIZE WS-INFO.
            PERFORM P500-CALCULO UNTIL WS-SIGNAL <> 'Y'.
            PERFORM P999-FIM.
            EXIT PROGRAM.
       P500-CALCULO.
            DISPLAY "INSIRA O NOME DO ALUNO."
            ACCEPT WS-ALUNO.
            DISPLAY "INSIRA A MATERIA."
            ACCEPT WS-MATERIA.

            DISPLAY "INSIRA A NOTA DO PRIMEIRO QUADRIMESTRE."
            ACCEPT WS-NOTA-1.
            DISPLAY "INSIRA A NOTA DO SEGUNDO QUADRIMESTRE."
            ACCEPT WS-NOTA-2.
            DISPLAY "INSIRA A NOTA DO TERCEIRO QUADRIMESTRE."
            ACCEPT WS-NOTA-3.
            DISPLAY "INSIRA A NOTA DO QUARTO QUADRIMESTRE."
            ACCEPT WS-NOTA-4.
            
            IF (WS-NOTA-1 IS NOT NUMERIC) OR 
               (WS-NOTA-2 IS NOT NUMERIC) OR 
               (WS-NOTA-3 IS NOT NUMERIC) OR 
               (WS-NOTA-4 IS NOT NUMERIC)       PERFORM P150-ERRO.

            IF (WS-NOTA-1 IS NEGATIVE) OR 
               (WS-NOTA-2 IS NEGATIVE) OR 
               (WS-NOTA-3 IS NEGATIVE) OR 
               (WS-NOTA-4 IS NEGATIVE)          PERFORM P150-ERRO.

            IF (WS-NOTA-1 > 10) OR 
               (WS-NOTA-2 > 10) OR 
               (WS-NOTA-3 > 10) OR 
               (WS-NOTA-4 > 10)                 PERFORM P150-ERRO.

            DISPLAY "NOTA 1: " WS-NOTA-1
            DISPLAY "NOTA 2: " WS-NOTA-2
            DISPLAY "NOTA 3: " WS-NOTA-3
            DISPLAY "NOTA 4: " WS-NOTA-4

            COMPUTE WS-MEDIA = (WS-NOTA-1 + WS-NOTA-2 + 
                                WS-NOTA-3 + WS-NOTA-4) / 4
           
            IF WS-MEDIA >= 7
               SET WS-CONFIRM TO TRUE
            ELSE  
               SET WS-CONFIRM TO FALSE
            END-IF.

            DISPLAY "**           RESULTADO DO PROCESSO              **"
            DISPLAY " "
            DISPLAY "Nome do Aluno: " WS-ALUNO
            DISPLAY "Matéria:       " WS-MATERIA
            DISPLAY "Média:         " WS-MEDIA
            DISPLAY "Status:        " WS-STATUS
            DISPLAY "**************************************************"

            DISPLAY " "
            DISPLAY "GOSTARIA DE CONTINUAR? (Y/N)"
            ACCEPT WS-SIGNAL.
            
       P150-ERRO.
            DISPLAY "**************************************************"
            DISPLAY "*      NOTAS INVALIDAS - TENTE NOVAMENTE         *"
            DISPLAY "**************************************************"
            PERFORM P999-FIM
            .
       P999-FIM.
            STOP RUN.
       END PROGRAM DESAFIOM2.
