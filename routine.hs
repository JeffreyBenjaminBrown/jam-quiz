-- Could use System.Random.newStdGen,
-- rather than bumping the seed below.

randomSeed = 17
edo = 46
edoDivisor = 23
:s .ghci

quizEdoComplements  edo            randomSeed
quizEdoPartitions   edo edoDivisor randomSeed
quizEdoSums         edo            randomSeed
quizEdoTriads       edo            randomSeed
quiz_changes                       randomSeed
quiz_changes2                      randomSeed -- better
quiz_changes3                      randomSeed -- even better
