       IDENTIFICATION DIVISION.
       PROGRAM-ID. train.
       AUTHOR. lucas & léo.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *on associe le FD TRAIN au fichier train.dat
           SELECT TRAIN ASSIGN TO "train.dat"
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       
       FILE SECTION.
      *la structure de TRAIN est défini dans train-record.cpy
           FD TRAIN.
              COPY train-record.

       WORKING-STORAGE SECTION.
       01 WS-TRAIN-TO-WRITE.
      *46 TIMES car il y a 46 ligne dans le fichier
           05 WS-LIGNE-TRAIN OCCURS 46 TIMES.
              10 WS-TYPE-TRAIN PIC X(3).
              10 WS-GARE-DEPART PIC X(18). 
              10 WS-HEURE-DEPART PIC X(4).
              10 WS-DUREE-TRAJET PIC X(2).
              10 WS-NOMBRE-TRAJET PIC X(10).

      *l'index pour parcourir le tableau
       01 WS-INDEX-TABLEAU PIC 9(2).

      *la taille du tableau
       01 WS-TAILLE-TABLEAU PIC 9(2) VALUE 46.

      *un output pour l'affichage 
       01 WS-SORTIE PIC X(100).
       

       PROCEDURE DIVISION.



           STOP RUN.

