#

library(FSelector)
library(mlbench)

###############################################
## Variable Selection - Aproximation filters ##
###############################################

#All-variables algorithms
weightchiSquared <- FSelector::chi.squared(PV1MATH~.,balancedData)
#weightRelief <- relief(PV1MATH~.,balancedData, neighbours.count = 5, sample.size = 20)

#Discrete-variables algorithms
weightentropInfo <- FSelector::information.gain(PV1MATH~.,balancedData)
weightentropInfoRatio <- FSelector::gain.ratio(PV1MATH~.,balancedData)
weightentropSymm <- FSelector::symmetrical.uncertainty(PV1MATH~.,balancedData)
weightOneR <- FSelector::oneR(PV1MATH~.,balancedData)

#Continuous-variables algorithms
#weightCorrelation <- FSelector::rank.correlation(PV1MATH~.,balancedData)


#Normalize results
weightchiSquared <- (weightchiSquared - min(weightchiSquared)) / (max(weightchiSquared) - min(weightchiSquared))
weightentropInfo <- (weightentropInfo - min(weightentropInfo)) / (max(weightentropInfo) - min(weightentropInfo))
weightentropInfoRatio <- (weightentropInfoRatio - min(weightentropInfoRatio)) / (max(weightentropInfoRatio) - min(weightentropInfoRatio))
weightentropSymm <- (weightentropSymm - min(weightentropSymm)) / (max(weightentropSymm) - min(weightentropSymm))
weightOneR <- (weightOneR - min(weightOneR)) / (max(weightOneR) - min(weightOneR))

final.weight <- (weightchiSquared + weightentropInfo + weightentropInfoRatio + weightentropSymm + weightOneR)/5

final.weight.ordered <- cbind(final.weight, rownames(final.weight))
final.weight.ordered <- final.weight.ordered[order(final.weight, decreasing = T),]
