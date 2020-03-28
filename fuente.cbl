      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. "INICIO".
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT OPTIONAL CLIENTES ASSIGN TO "./clientes.dat"
                  ORGANIZATION INDEXED
                  ACCESS MODE DYNAMIC
                  RECORD KEY IS ID_CLIENTE
                  ALTERNATE KEY CLI_NOMBRE WITH DUPLICATES
                  ALTERNATE KEY CLI_ALT_2  WITH DUPLICATES
                  STATUS ST-FILE.

       DATA DIVISION.
       FILE SECTION.

       FD  CLIENTES.

       01  REG-CLIENTES.
           03  ID_CLIENTE.
               05  CLI_ID           PIC 9(7).
           03  CLI_SALDO            PIC S9(7)V9(3).
           03  CLI_NOMBRE           PIC X(70).
           03  CLI_DIRECCION        PIC X(80).
           03  CLI_CODPOST          PIC X(10).
           03  CLI_CATEGORIA        PIC X.
           03  CLI_ALT_2.
               05  CLI_CATEGORIA_2  PIC X.
               05  CLI_NOMBRE_2     PIC X(60).
           03  FILLER               PIC X(240).


       WORKING-STORAGE SECTION.

       01  ST-FILE   PIC XX.
       01  X         PIC X.

       01  MENSAJE   PIC X(70).

       01  FIN        PIC X     VALUES "N".
       01  EXISTE     PIC X.
       01  HUBO-ERROR PIC 9     VALUES 0.
       01  GUIONES    PIC X(80) VALUES ALL "-".
       01  OPCION     PIC 99.

       01  W-CLI-ID   PIC 9(07).
       01  W-CLI-ID-Z PIC Z(06)9.

       01  DATOS.
           02 W-CLI-NOMBRE     PIC X(70).
           02 W-CLI-NOMBRE-ANT PIC X(70).
           02 W-CLI-DIRECCION  PIC X(80).
           02 W-CLI-CODPOSTAL  PIC X(10).
           02 W-CLI-CATEGORIA  PIC X.

       PROCEDURE DIVISION.
       PABLO.
           PERFORM INICIALIZACION.
           PERFORM ABRO-ARCHIVO.
           PERFORM PROCESO THRU F-PROCESO UNTIL FIN = "S".
           PERFORM CIERRO-ARCHIVO.
           GO TO FINALIZAR.

       INICIALIZACION.
           MOVE "N" TO FIN.
           MOVE "S" TO EXISTE.

       ABRO-ARCHIVO.
           OPEN I-O CLIENTES.
           IF ST-FILE > "07"
              STRING "Error al abrir Clientes " ST-FILE
                 DELIMITED BY SIZE INTO MENSAJE
              DISPLAY MENSAJE LINE 10 COL 20
              MOVE "S" TO FIN.

       CIERRO-ARCHIVO.
           CLOSE CLIENTES.

       FINALIZAR.
           STOP RUN.

       PROCESO.
           PERFORM MUESTRO-PANTALLA.
           PERFORM INGRESO-ID THRU F-INGRESO-ID.
           IF FIN = "N"
              PERFORM LEO-CLIENTES THRU F-LEO-CLIENTES
              IF HUBO-ERROR = 1
                 MOVE "S" TO FIN
                 GO TO F-PROCESO
              END-IF
              IF EXISTE = "S"
                 PERFORM MUESTRO-DATOS
              ELSE
                 PERFORM CARGO-DATOS THRU F-CARGO-DATOS
              END-IF
              PERFORM OPCIONES.


       F-PROCESO.
           EXIT.

       MUESTRO-PANTALLA.
           DISPLAY " "                   LINE 1  COL 1 ERASE EOS
                   "A.B.M. Clientes"     LINE 3  COL 32
                   GUIONES               LINE 4  COL 1
                   "Id Cliente : "       LINE 07 COL 10
                   "01. Nombre     : "   LINE 10 COL 10
                   "02. Direccion  : "   LINE 12 COL 10
                   "03. Cod.Postal : "   LINE 14 COL 10
                   "04. Categoria  : "   LINE 16 COL 10
                   "Opcion [  ]"         LINE 20 COL 30
                   GUIONES               LINE 22 COL 1.


       INGRESO-ID.
           ACCEPT W-CLI-ID LINE 07 COL 23.
           MOVE   W-CLI-ID TO W-CLI-ID-Z.
           DISPLAY W-CLI-ID-Z LINE 07 COL 23.
           IF W-CLI-ID = 0 GO TO INGRESO-ID.

       F-INGRESO-ID.
           EXIT.

       LEO-CLIENTES.
           DISPLAY SPACES LINE 23 COL 1 SIZE 80.
           MOVE W-CLI-ID TO CLI_ID.
           READ CLIENTES INVALID KEY MOVE "N" TO EXISTE.
           IF ST-FILE = "99" GO TO LEO-CLIENTES.
           IF ST-FILE > "07" AND ST-FILE NOT = "23"
                 STRING "Error leyendo Clientes Status = " ST-FILE
                 DELIMITED BY SIZE INTO MENSAJE
                 DISPLAY MENSAJE LINE 23 COL 1
                 MOVE 1   TO HUBO-ERROR
                 MOVE "N" TO EXISTE.

       F-LEO-CLIENTES.
           EXIT.

       MUESTRO-DATOS.

           IF EXISTE = "S"
              MOVE CLI_NOMBRE    TO W-CLI-NOMBRE
              MOVE CLI_DIRECCION TO W-CLI-DIRECCION
              MOVE CLI_CODPOST   TO W-CLI-CODPOSTAL
              MOVE CLI_CATEGORIA TO W-CLI-CATEGORIA.
           DISPLAY CLI_NOMBRE    LINE 10 COL 36
                   CLI_DIRECCION LINE 12 COL 36
                   CLI_CODPOST   LINE 14 COL 36
                   CLI_CATEGORIA LINE 16 COL 36.

       CARGO-DATOS.
           INITIALIZE DATOS.

       INGRESO-NOMBRE.
           MOVE W-CLI-NOMBRE TO W-CLI-NOMBRE-ANT.
           ACCEPT W-CLI-NOMBRE LINE 10 COL 36.
           IF W-CLI-NOMBRE = SPACES
              MOVE W-CLI-NOMBRE-ANT TO W-CLI-NOMBRE
              GO TO INGRESO-NOMBRE.
           DISPLAY W-CLI-NOMBRE LINE 10 COL 36.

       INGRESO-DIRECCION.
           ACCEPT W-CLI-DIRECCION LINE 12 COL 36.
           IF W-CLI-DIRECCION = SPACES
              GO TO INGRESO-DIRECCION.
           DISPLAY W-CLI-DIRECCION LINE 12 COL 36.

       INGRESO-CODPOSTAL.
           ACCEPT W-CLI-CODPOSTAL LINE 14 COL 36.
           IF W-CLI-CODPOSTAL = SPACES
              GO TO INGRESO-CODPOSTAL.
           DISPLAY W-CLI-CODPOSTAL LINE 14 COL 36.

       INGRESO-CATEGORIA.
           ACCEPT W-CLI-CATEGORIA LINE 16 COL 36.
           IF W-CLI-CATEGORIA = SPACES
              GO TO INGRESO-CATEGORIA.
           DISPLAY W-CLI-CATEGORIA LINE 16 COL 36.

       F-CARGO-DATOS.
           EXIT.

       OPCIONES.
           DISPLAY "[00 - ENTER] GRABAR" LINE 23 COL 1
                   "[88] - BORRAR"       LINE 24 COL 1
                   "[77] - SALIR"        LINE 23 COL 20.
           ACCEPT OPCION LINE 20 COL 38.
           IF FIN = "N"
              EVALUATE OPCION
                WHEN 1
                   PERFORM INGRESO-NOMBRE
                WHEN 2
                   PERFORM INGRESO-DIRECCION
                WHEN 3
                   PERFORM INGRESO-CODPOSTAL
                WHEN 4
                   PERFORM INGRESO-CATEGORIA
                WHEN 0
                   PERFORM GRABAR THRU F-GRABAR
                WHEN 88
                   PERFORM BORRAR
                WHEN 77
                   MOVE "S" TO FIN
                WHEN OTHER
                   GO TO OPCIONES
              END-EVALUATE
              IF OPCION > 0 AND OPCION < 77 GO TO OPCIONES.

       GRABAR.
           MOVE W-CLI-NOMBRE    TO CLI_NOMBRE CLI_NOMBRE_2.
           MOVE W-CLI-DIRECCION TO CLI_DIRECCION.
           MOVE W-CLI-CODPOSTAL TO CLI_CODPOST.
           MOVE W-CLI-CATEGORIA TO CLI_CATEGORIA CLI_CATEGORIA_2.

       GRABO.
           IF EXISTE = "S" GO TO REGRABO.
           WRITE REG-CLIENTES.
           IF ST-FILE = "99" GO TO GRABO.
           IF ST-FILE > "07"
              STRING "Error al GRABAR Clientes " ST-FILE
                 DELIMITED BY SIZE INTO MENSAJE
              DISPLAY MENSAJE LINE 24 COL 40
              ACCEPT X LINE 23 COL 77.
           GO TO F-GRABAR.

       REGRABO.
           REWRITE REG-CLIENTES.
           IF ST-FILE = "99" GO TO REGRABO.
           IF ST-FILE > "07"
              STRING "Error al reGRABAR Clientes " ST-FILE
                 DELIMITED BY SIZE INTO MENSAJE
              DISPLAY MENSAJE LINE 24 COL 40
              ACCEPT X LINE 23 COL 77.

       F-GRABAR.
           EXIT.

       BORRAR.
           DELETE CLIENTES.
           IF ST-FILE = "99" GO TO BORRAR.
           IF ST-FILE > "07"
              STRING "Error al borrar Clientes " ST-FILE
                 DELIMITED BY SIZE INTO MENSAJE
              DISPLAY MENSAJE LINE 24 COL 40.


       END PROGRAM "INICIO".
