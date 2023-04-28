-- Could use System.Random.newStdGen,
-- rather than bumping the seed below.

randomSeed = 13
edo = 46
edoDivisor = 23
import Changes -- TODO : Why is this necessary?

quizEdoComplements  edo            randomSeed
quizEdoPartitions   edo edoDivisor randomSeed
quizEdoSums         edo            randomSeed
quizEdoTriads       edo            randomSeed
quiz_gestalt                       randomSeed
