       IDENTIFICATION DIVISION.
       PROGRAM-ID. train.
       AUTHOR. lucas & Leocrabe225.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *on associe le FD TRAIN au fichier train.dat
           SELECT TRAIN ASSIGN TO "train.dat"
              ORGANIZATION IS LINE SEQUENTIAL.
              
           SELECT TRAIN-UNIQUE-OUTPUT
               ASSIGN TO "train-unique.dat"
               ORGANIZATION IS LINE SEQUENTIAL.

           SELECT TRAIN2 ASSIGN TO "train2.dat"
              ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       
       FILE SECTION.
      *la structure de TRAIN est défini dans train-record.cpy
       FD TRAIN.
       COPY traincpy.


       FD TRAIN-UNIQUE-OUTPUT.
       01 TRAIN-UNI-OUT-RECORD.
           05 TRAIN-UNI-OUT-LINE   PIC X(150).
      *la structure de sortie du fichier train2.dat
       FD TRAIN2.
       01 LIGNE-TRAIN2.
           05 LIGNE-TRAIN2-CONTENU PIC X(150).


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
              10 WS-NOMBRE-TRAJET PIC X OCCURS 10 TIMES.
                  88 TRAIN-STOPS-HERE VALUE 'H'.
                  88 TRAIN-SERVICE    VALUE 'S'.
                  88 TRAIN-FRETE      VALUE 'F'.
              10 WS-TRAIN-STOPS       PIC 9(2).
              10 WS-HEURE-ARRIVEE.
                  15 WS-TRAIN-END-TIME-HH PIC 9(2).
                  15 WS-TRAIN-END-TIME-MM PIC 9(2).

      *l'index pour parcourir le tableau
       01 WS-IDX PIC 9(2).
       01 WS-IDX-2 PIC 9(2).

      *la taille du tableau
       01 WS-TBL-SIZE PIC 9(2) VALUE 46.

      *variable pour calculer l'heure d'arrivée
       01 WS-HEURE-ARRIVE-CALCUL PIC 9(2).

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

       01 WS-USER-INPUT        PIC 9(03).
       PROCEDURE DIVISION.
      *initialisation de l'index et du flag de fin de lecture
           MOVE 0 TO WS-IDX.
           SET WS-EOF-FALSE TO TRUE.
      *on ouvre le fichier
           OPEN INPUT TRAIN.
      *on lit le fichier
           PERFORM UNTIL WS-EOF-TRUE
               READ TRAIN
      *si on a fini de lire le fichier, on sort de la boucle
                   AT END
                       SET WS-EOF-TRUE TO TRUE
      *si on n'a pas fini de lire le fichier
                   NOT AT END
                       ADD 1 TO WS-IDX
                       MOVE TRAIN-PLANNING TO WS-LIGNE-TRAIN(WS-IDX)
                       MOVE 1 TO WS-IDX-2
                       PERFORM UNTIL WS-IDX-2 > 10 
                       OR WS-NOMBRE-TRAJET(WS-IDX,WS-IDX-2) EQUAL SPACE
                           ADD 1 TO WS-IDX-2
                       END-PERFORM
                       SUBTRACT 1 FROM WS-IDX-2
                       MOVE WS-IDX-2 TO WS-TRAIN-STOPS(WS-IDX)
      *on calcule l'heure d'arrivée et on l'enregistre dans le tableau
      *on remet à 0 la valeur de WS-HEURE-ARRIVE-CALCUL pour éviter 
      *erreur de calcul
                       MOVE 0 TO WS-HEURE-ARRIVE-CALCUL
      *les minutes d'arrivé et de départ sont les mêmes
                       MOVE WS-HEURE-DEPART-MM(WS-IDX) 
                       TO WS-TRAIN-END-TIME-MM(WS-IDX)
      *on récupère l'heure de départ et la durée
                       ADD WS-HEURE-DEPART-HH(WS-IDX) 
                       WS-DUREE-TRAJET(WS-IDX) 
                       TO WS-HEURE-ARRIVE-CALCUL
                       COMPUTE WS-HEURE-ARRIVE-CALCUL = FUNCTION MOD(
                        WS-HEURE-ARRIVE-CALCUL 24)
      *on vient de calculer l'heure d'arrivé, on l'enregistre
                       MOVE WS-HEURE-ARRIVE-CALCUL 
                       TO WS-TRAIN-END-TIME-HH(WS-IDX) 
               END-READ
           END-PERFORM.
      *on ferme le fichier puisqu'on a fini de le lire
           CLOSE TRAIN.
      *on enregistre la taille du tableau dans une variable prévu à
      * cet effet
           MOVE WS-IDX TO WS-TBL-SIZE.

      *on ouvre le fichier train2.dat
           OPEN OUTPUT TRAIN2.
      
           PERFORM VARYING WS-IDX FROM 1 BY 1 UNTIL WS-IDX > WS-TBL-SIZE
               MOVE WS-IDX TO WS-TRAIN-TO-WRITE
               PERFORM 0100-MOVE-TO-OUTPUT-BEGIN
                  THRU 0100-MOVE-TO-OUTPUT-END
      *on affiche toutes les informations sur les trains
               DISPLAY WS-IDX SPACE WITH NO ADVANCING
               DISPLAY WS-SORTIE
      *on pense à écrire également dans la sortie train2.dat
               MOVE WS-SORTIE TO LIGNE-TRAIN2
               WRITE LIGNE-TRAIN2
           END-PERFORM.

      *après avoir écrit toutes les information concernant les trains,
      *il faut également écrire le nombre de ligne traité
           MOVE FUNCTION CONCATENATE("NOMBRE de ligne traitée = ",
           WS-TBL-SIZE) TO LIGNE-TRAIN2. 
           WRITE LIGNE-TRAIN2.

      *on ferme le fichier train2.dat
           CLOSE TRAIN2.
      
      *on appelle le paragraphe du bonus
           PERFORM 0400-INDEX-SEARCH-BONUS-BEGIN
              THRU 0400-INDEX-SEARCH-BONUS-END.


      *on arrête le programme
           STOP RUN.

      *paragraphe pour affichage dans le termnal
       0100-MOVE-TO-OUTPUT-BEGIN.
           EVALUATE TRUE
      *on commence par enregistrer le type de train
               WHEN WS-TGV(WS-TRAIN-TO-WRITE)
                   MOVE "Train a Grande Vitesse"     TO WS-OUT-TYPE
               WHEN WS-CORAIL(WS-TRAIN-TO-WRITE)
                   MOVE "Corail Intercite"           TO WS-OUT-TYPE
               WHEN WS-TER(WS-TRAIN-TO-WRITE)
                   MOVE "Transport express regional" TO WS-OUT-TYPE
           END-EVALUATE.
      *on enregistre la gare de départ
           MOVE WS-GARE-DEPART(WS-TRAIN-TO-WRITE) 
               TO WS-OUT-STATION-DEPART.
      *on enregistre l'heure de départ(heure et minute) 
           MOVE WS-HEURE-DEPART-HH(WS-TRAIN-TO-WRITE)
               TO WS-OUT-TRAIN-TIME-HH.
           MOVE WS-HEURE-DEPART-MM(WS-TRAIN-TO-WRITE)
               TO WS-OUT-TRAIN-TIME-MM.
      *on enregistre la durée du trajet
           MOVE WS-DUREE-TRAJET(WS-TRAIN-TO-WRITE)
               TO WS-OUT-TRAIN-NMBR-HEURES.
      *on enregistre le nombre de stop
           MOVE WS-TRAIN-STOPS(WS-TRAIN-TO-WRITE)
               TO OUT-TRAIN-STOPS.
      *on enregistre l'heure d'arrivée(heure et minute)
           MOVE WS-TRAIN-END-TIME-HH(WS-TRAIN-TO-WRITE)
               TO OUT-TRAIN-END-TIME-HH.
           MOVE WS-TRAIN-END-TIME-MM(WS-TRAIN-TO-WRITE)
               TO OUT-TRAIN-END-TIME-MM.
       0100-MOVE-TO-OUTPUT-END.

      *paragraphe pour le bonus, qui demande à l'utilisateur l'index
      *d'un record avant de l'écrire dans un fichier
       0400-INDEX-SEARCH-BONUS-BEGIN.
      *on demande l'entrée utilisateur avec un message
           DISPLAY                  "Which train do you want to write to 
      -            " train-unique.dat (1-" WS-TBL-SIZE ")? ".
      *tant que l'utilisateur ne rentre pas une valeur valid , on lui
      *redemande
           PERFORM UNTIL WS-USER-INPUT > 0
                   AND WS-USER-INPUT <= WS-TBL-SIZE
               ACCEPT WS-USER-INPUT
               IF WS-USER-INPUT = 0 OR WS-USER-INPUT > WS-TBL-SIZE
                   DISPLAY WS-USER-INPUT 
                       " is invalid. It should be between 1 and "
                       WS-TBL-SIZE "."
               END-IF
           END-PERFORM.
      *on appelle le paragrape d'écriture avec la valeur de
      *l'utilisateur
           MOVE WS-USER-INPUT TO WS-TRAIN-TO-WRITE.
           PERFORM 0100-MOVE-TO-OUTPUT-BEGIN
              THRU 0100-MOVE-TO-OUTPUT-END.
      *on ouvre le fichier de sortie
           OPEN OUTPUT TRAIN-UNIQUE-OUTPUT.
      *on s'assure qu'il n'y a rien dans la ligne d'écriture du fichier
           INITIALIZE TRAIN-UNI-OUT-RECORD.
      *avec un string, on con(cat)ene un message avec le numéro du train
           STRING "The user asked for train " WS-USER-INPUT " to be "
               "written in this file." DELIMITED BY SIZE INTO 
               TRAIN-UNI-OUT-LINE
           END-STRING.
      *on écrit le message dans le fichier
           WRITE TRAIN-UNI-OUT-RECORD.
      *on bouge la variable de sortie dans la ligne d'écriture
           MOVE WS-SORTIE TO TRAIN-UNI-OUT-LINE.
      *et on écrit
           WRITE TRAIN-UNI-OUT-RECORD.
      *avant de fermer le fichier
           CLOSE TRAIN-UNIQUE-OUTPUT.
           DISPLAY "The train record " WS-USER-INPUT " was successfully"
               " written to the file.".
       0400-INDEX-SEARCH-BONUS-END.
