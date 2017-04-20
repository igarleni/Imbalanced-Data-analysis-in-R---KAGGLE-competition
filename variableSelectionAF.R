#

library(FSelector)
library(mlbench)

###############################################
## Variable Selection - Aproximation filters ##
###############################################

#All-variables algorithms
weightchiSquared <- FSelector::chi.squared(PV1MATH~.,trainData)
#weightRelief <- relief(PV1MATH~.,trainData, neighbours.count = 5, sample.size = 20)

#Discrete-variables algorithms
weightentropInfo <- FSelector::information.gain(PV1MATH~.,trainData)
weightentropInfoRatio <- FSelector::gain.ratio(PV1MATH~.,trainData)
weightentropSymm <- FSelector::symmetrical.uncertainty(PV1MATH~.,trainData)
weightOneR <- FSelector::oneR(PV1MATH~.,trainData)

#Continuous-variables algorithms
#weightCorrelation <- FSelector::rank.correlation(PV1MATH~.,trainData)


#Normalize results
weightchiSquared <- (weightchiSquared - min(weightchiSquared)) / (max(weightchiSquared) - min(weightchiSquared))
weightentropInfo <- (weightentropInfo - min(weightentropInfo)) / (max(weightentropInfo) - min(weightentropInfo))
weightentropInfoRatio <- (weightentropInfoRatio - min(weightentropInfoRatio)) / (max(weightentropInfoRatio) - min(weightentropInfoRatio))
weightentropSymm <- (weightentropSymm - min(weightentropSymm)) / (max(weightentropSymm) - min(weightentropSymm))
weightOneR <- (weightOneR - min(weightOneR)) / (max(weightOneR) - min(weightOneR))

final.weight <- (weightchiSquared + weightentropInfo + weightentropInfoRatio + weightentropSymm + weightOneR)/5

final.weight.ordered <- cbind(final.weight, rownames(final.weight))
final.weight.ordered <- final.weight.ordered[order(final.weight, decreasing = T),]
