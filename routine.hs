-- Could use System.Random.newStdGen,
-- rather than bumping the seed below.

randomSeed = 33
edo = 34
edoDivisor = 17
:s .ghci

quizEdoComplements  edo            randomSeed
quizEdoPartitions   edo edoDivisor randomSeed
quizEdoSums         edo            randomSeed
quizEdoTriads       edo            randomSeed
quiz_changes                       randomSeed
quiz_changes2                      randomSeed -- better
quiz_changes3                      randomSeed -- even better
quiz_chord_changes  edo            randomSeed

-- "Manual": Generate a note at random,
-- and find how it relates to each of the harmonics.
