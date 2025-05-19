       IDENTIFICATION DIVISION.
       PROGRAM-ID. train.
       AUTHOR. lucas & Leocrabe225.

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
       COPY traincpy.

       WORKING-STORAGE SECTION.
       01 WS-TRAIN-TO-WRITE        PIC 9(03).

       01 WS-TBL-TRAIN.
      *46 TIMES car il y a 46 ligne dans le fichier
           05 WS-LIGNE-TRAIN OCCURS 46 TIMES.
              10 WS-TYPE-TRAIN PIC X(3).
                  88 WS-TGV     VALUE 'TGV'.
                  88 WS-CORAIL  VALUE 'COR'.
                  88 WS-TER     VALUE 'TER'.
              10 WS-GARE-DEPART PIC X(18). 
              10 WS-HEURE-DEPART.
                  15 WS-HEURE-DEPART-HH PIC 9(2).
                  15 WS-HEURE-DEPART-MM PIC 9(2).
              10 WS-DUREE-TRAJET PIC 9(2).
              10 WS-NOMBRE-TRAJET PIC X(10).
                  88 TRAIN-STOPS-HERE VALUE 'H'.
                  88 TRAIN-SERVICE    VALUE 'S'.
                  88 TRAIN-FRETE      VALUE 'F'.
              10 WS-TRAIN-STOPS       PIC 9(2).
              10 WS-HEURE-ARRIVEE.
                  15 WS-TRAIN-END-TIME-HH PIC 9(2).
                  15 WS-TRAIN-END-TIME-MM PIC 9(2).

      *l'index pour parcourir le tableau
       01 WS-IDX PIC 9(2).

      *la taille du tableau
       01 WS-TBL-SIZE PIC 9(2) VALUE 46.

      *un output pour l'affichage 
       01 WS-SORTIE.
           05 FILLER           PIC X(12) VALUE "Train Type: ".
           05 WS-OUT-TYPE      PIC X(26).
           05 FILLER           PIC X(22) VALUE " | Departure Station: ".
           05 WS-OUT-STATION-DEPART PIC X(18).
           05 FILLER           PIC X(15) VALUE " | Train Time: ".
           05 WS-OUT-TRAIN-TIME.
               10 WS-OUT-TRAIN-TIME-HH PIC 9(02).
               10 FILLER               PIC X(01) VALUE ":".
               10 WS-OUT-TRAIN-TIME-MM PIC 9(02).
               10 FILLER               PIC X(01) VALUE "h".
           05 FILLER                   PIC X(13) VALUE " | Duration: ".
           05 WS-OUT-TRAIN-NMBR-HEURES PIC 9(02).
           05 FILLER                   PIC X(01) VALUE "h".
           05 FILLER                   PIC X(10) VALUE " | Stops: ".
           05 OUT-TRAIN-STOPS          PIC 9(02).
           05 FILLER                   PIC X(17) VALUE 
              " | Arrival Time: ".
           05 WS-OUT-TRAIN-END-TIME.
               10 OUT-TRAIN-END-TIME-HH PIC 9(02).
               10 FILLER                PIC X(01) VALUE ":".
               10 OUT-TRAIN-END-TIME-MM PIC 9(02).
               10 FILLER                PIC X(01) VALUE "h".


       01 WS-EOF               PIC 9(01).
           88 WS-EOF-TRUE                VALUE 1.
           88 WS-EOF-FALSE               VALUE 0.
       PROCEDURE DIVISION.
           MOVE 0 TO WS-IDX.
           SET WS-EOF-FALSE TO TRUE.
           OPEN INPUT TRAIN.
           PERFORM UNTIL WS-EOF-TRUE
               READ TRAIN
                   AT END
                       SET WS-EOF-TRUE TO TRUE
                   NOT AT END
                       ADD 1 TO WS-IDX
                       MOVE TRAIN-PLANNING TO WS-LIGNE-TRAIN(WS-IDX)
               END-READ
           END-PERFORM.
           CLOSE TRAIN.
           MOVE WS-IDX TO WS-TBL-SIZE.


           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > WS-TBL-SIZE
               MOVE WS-IDX TO WS-TRAIN-TO-WRITE
               PERFORM 0100-MOVE-TO-OUTPUT-BEGIN
                  THRU 0100-MOVE-TO-OUTPUT-END
               DISPLAY WS-SORTIE
           END-PERFORM.
           STOP RUN.

       0100-MOVE-TO-OUTPUT-BEGIN.
           EVALUATE TRUE
               WHEN WS-TGV(WS-TRAIN-TO-WRITE)
                   MOVE "Train a Grande Vitesse"     TO WS-OUT-TYPE
               WHEN WS-CORAIL(WS-TRAIN-TO-WRITE)
                   MOVE "Corail Intercite"           TO WS-OUT-TYPE
               WHEN WS-TER(WS-TRAIN-TO-WRITE)
                   MOVE "Transport express regional" TO WS-OUT-TYPE
           END-EVALUATE.
           MOVE WS-GARE-DEPART(WS-TRAIN-TO-WRITE) 
               TO WS-OUT-STATION-DEPART.
           MOVE WS-HEURE-DEPART-HH(WS-TRAIN-TO-WRITE)
               TO WS-OUT-TRAIN-TIME-HH.
           MOVE WS-HEURE-DEPART-MM(WS-TRAIN-TO-WRITE)
               TO WS-OUT-TRAIN-TIME-MM.
           MOVE WS-DUREE-TRAJET(WS-TRAIN-TO-WRITE)
               TO WS-OUT-TRAIN-NMBR-HEURES.
           MOVE WS-TRAIN-STOPS(WS-TRAIN-TO-WRITE)
               TO OUT-TRAIN-STOPS.
           MOVE WS-TRAIN-END-TIME-HH(WS-TRAIN-TO-WRITE)
               TO OUT-TRAIN-END-TIME-HH.
           MOVE WS-TRAIN-END-TIME-MM(WS-TRAIN-TO-WRITE)
               TO OUT-TRAIN-END-TIME-MM.
       0100-MOVE-TO-OUTPUT-END.
