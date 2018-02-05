library('depmixS4')
library('quantmod')
set.seed(1)

# Obtain QQQ data from 2004 onwards and
# create the returns stream from this
getSymbols( "QQQ", from="2004-01-01" )
qqqRets = diff( log( Cl( QQQ ) ) )
returns = as.numeric(qqqRets)

# Fit a Hidden Markov Model with two states
# to the QQQ returns stream
hmm <- depmix(returns ~ 1, family = gaussian(), nstates = 2, data=data.frame(returns=returns))
hmmfit <- fit(hmm, verbose = FALSE)
post_probs2 <- posterior(hmmfit)

# Plot the returns stream and the posterior
# probabilities of the separate regimes
layout(1:2)
plot(returns, type='l', main='Regime Detection', xlab='', ylab='Returns')
matplot(post_probs[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
legend(x='topright', c('Regime #1','Regime #2'), fill=1:2, bty='n')

# Fit a Hidden Markov Model with three states
# to the QQQ returns stream
hmm <- depmix(returns ~ 1, family = gaussian(), nstates = 3, data=data.frame(returns=returns))
hmmfit <- fit(hmm, verbose = FALSE)
post_probs_df <- posterior(hmmfit)

# Plot the returns stream and the posterior
# probabilities of the separate regimes
layout(1:2)
plot(returns, type='l', main='Regime Detection', xlab='', ylab='Returns')
matplot(post_probs_df[,-1], type='l', main='Regime Posterior Probabilities', ylab='Probability')
legend(x='bottomleft', c('Regime #1','Regime #2', 'Regime #3'), fill=1:3, bty='n')
#Create a dataframe that includes, posterior probabilities of the states, and price of the QQQ
post_probs_df['Date']<-index(qqqRets)
post_probs_df['Price']<-QQQ$QQQ.Adjusted

# Create a csv file for usage in python 
write.csv(post_probs_df,file='HMM_QQQasof2_2.csv')

