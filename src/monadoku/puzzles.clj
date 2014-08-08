(ns monadoku.puzzles)

(def easy [7, 0, 0, 0, 8, 9, 0, 0, 0,
           4, 0, 0, 7, 0, 0, 6, 0, 8,
           0, 0, 8, 0, 2, 0, 7, 3, 0,
           0, 0, 1, 2, 6, 0, 0, 9, 3,
           0, 0, 2, 1, 0, 5, 8, 0, 0,
           6, 3, 0, 0, 9, 7, 4, 0, 0,
           0, 5, 7, 0, 4, 0, 2, 0, 0,
           2, 0, 9, 0, 0, 8, 0, 0, 1,
           0, 0, 0, 6, 1, 0, 0, 0, 5]
  )

(def mild [0, 7, 0, 0, 4, 0, 0, 0, 0,
           0, 1, 9, 0, 2, 0, 7, 0, 0,
           0, 0, 0, 0, 0, 7, 9, 0, 5,
           0, 0, 5, 0, 1, 9, 0, 3, 4,
           0, 0, 2, 0, 0, 0, 6, 0, 0,
           9, 4, 0, 8, 3, 0, 5, 0, 0,
           2, 0, 1, 3, 0, 0, 0, 0, 0,
           0, 0, 3, 0, 6, 0, 8, 9, 0,
           0, 0, 0, 0, 5, 0, 0, 1, 0]
  )

(def difficult1 [6, 0, 0, 9, 5, 0, 0, 0, 1,
                 0, 2, 0, 0, 0, 0, 0, 0, 0,
                 0, 0, 0, 4, 0, 0, 8, 2, 0,
                 2, 0, 0, 0, 6, 0, 1, 0, 0,
                 9, 0, 3, 5, 0, 4, 2, 0, 7,
                 0, 0, 8, 0, 3, 0, 0, 0, 4,
                 0, 4, 5, 0, 0, 6, 0, 0, 0,
                 0, 0, 0, 0, 0, 0, 0, 7, 0,
                 3, 0, 0, 0, 2, 5, 0, 0, 8]
  )

(def difficult25 [0, 1, 0, 0, 8, 6, 0, 0, 0,
                  0, 6, 9, 5, 1, 0, 7, 0, 0,
                  0, 2, 0, 0, 7, 0, 0, 0, 0,
                  0, 9, 4, 7, 0, 0, 0, 0, 5,
                  0, 0, 0, 0, 0, 0, 0, 0, 0,
                  8, 0, 0, 0, 0, 5, 9, 1, 0,
                  0, 0, 0, 0, 6, 0, 0, 2, 0,
                  0, 0, 3, 0, 2, 9, 4, 8, 0,
                  0, 0, 0, 3, 5, 0, 0, 9, 0]
  )

(def fiendish [0, 0, 1, 4, 6, 8, 0, 0, 0,
               0, 6, 0, 3, 0, 0, 0, 0, 0,
               0, 9, 0, 0, 0, 0, 0, 0, 2,
               3, 0, 0, 0, 0, 0, 0, 8, 0,
               0, 0, 5, 0, 7, 0, 9, 0, 0,
               0, 1, 0, 0, 0, 0, 0, 0, 6,
               8, 0, 0, 0, 0, 0, 0, 9, 0,
               0, 0, 0, 0, 0, 4, 0, 7, 0,
               0, 0, 0, 5, 2, 3, 4, 0, 0]
  )

(def hardest [8, 0, 0, 0, 0, 0, 0, 0, 0,
              0, 0, 3, 6, 0, 0, 0, 0, 0,
              0, 7, 0, 0, 9, 0, 2, 0, 0,
              0, 5, 0, 0, 0, 7, 0, 0, 0,
              0, 0, 0, 0, 4, 5, 7, 0, 0,
              0, 0, 0, 1, 0, 0, 0, 3, 0,
              0, 0, 1, 0, 0, 0, 0, 6, 8,
              0, 0, 8, 5, 0, 0, 0, 1, 0,
              0, 9, 0, 0, 0, 0, 4, 0, 0])
