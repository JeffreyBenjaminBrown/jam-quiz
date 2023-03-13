-- Could use System.Random.newStdGen,
-- rather than bumping the seed below.

edo = 46
edoDivisor = 23
randomSeed = 4

quizEdoPartitions   edo edoDivisor randomSeed
quizEdoComplements  edo            randomSeed
quizEdoSums         edo            randomSeed
quizEdoTriads       edo            randomSeed
