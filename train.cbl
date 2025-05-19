       01 WS-EOF               PIC 9(01).
           88 WS-EOF-TRUE                VALUE 1.
           88 WS-EOF-FALSE               VALUE 0.
       PROCEDURE DIVISION.
           MOVE 0 TO IDX.
           SET WS-EOF-FALSE TO TRUE.
           OPEN INPUT TRAIN-INPUT.
           PERFORM UNTIL WS-EOF-TRUE
               READ TRAIN-INPUT
                   AT END
                       SET WS-EOF-TRUE TO TRUE
                   NOT AT END
                       ADD 1 TO IDX
                       MOVE TRAIN-PLANNING TO TBL-TRAIN(IDX)
               END-READ
           END-PERFORM.
           MOVE IDX TO TBL-SIZE.

           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > TBL-SIZE
               MOVE IDX TO TRAIN-TO-WRITE
               PERFORM 0100-MOVE-TO-OUTPUT-BEGIN
                  THRU 0100-MOVE-TO-OUTPUT-END
               DISPLAY OUT-LINE
           END-PERFORM.
           STOP RUN.

       0100-MOVE-TO-OUTPUT-BEGIN.
           EVALUATE TBL-TYPE(TRAIN-TO-WRITE)
               WHEN TGV
                   MOVE "Train a Grande Vitesse"     TO OUT-TYPE
               WHEN CORAIL
                   MOVE "Corail Intercite"           TO OUT-TYPE
               WHEN TER
                   MOVE "Transport express regional" TO OUT-TYPE
           END-EVALUATE.
           MOVE TBL-STATION-DEPART(TRAIN-TO-WRITE) 
               TO OUT-STATION-DEPART.
           MOVE TBL-TRAIN-TIME-HH(TRAIN-TO-WRITE) TO OUT-TRAIN-TIME-HH.
           MOVE TBL-TRAIN-TIME-MM(TRAIN-TO-WRITE) TO OUT-TRAIN-TIME-MM.
           MOVE TBL-TRAIN-NBRE-HEURES(TRAIN-TO-WRITE)
               TO OUT-TRAIN-NMBR-HEURES.
      *    MOVE TBL-TRAIN-STOPS(TRAIN-TO-WRITE) TO OUT-TRAIN-STOPS.
      *    MOVE TBL-TRAIN-END-TIME-HH(TRAIN-TO-WRITE)
      *        TO OUT-TRAIN-END-TIME-HH
      *    MOVE TBL-TRAIN-END-TIME-MM(TRAIN-TO-WRITE)
      *        TO OUT-TRAIN-END-TIME-MM
       0100-MOVE-TO-OUTPUT-END.