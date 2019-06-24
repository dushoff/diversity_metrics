set.seed(2152)
muReps<-2e3

l <- 1
dat <- usersguide
sampleSize <- 200

truemun<-truemu(usersguide, size=sampleSize, reps=muReps, l=l)
print(truemun)

p <- usersguide/sum(usersguide)
p <- p[p>0]

print(sampDiv(p, sampleSize, muReps))
print(expRich(p, sampleSize))

q <- 1-l
sam<-subsam(dat, sampleSize)
print(sum(sam>0))
p_obs <- sam/sum(sam)
print(expRich(p_obs, sampleSize))
print(Chat.Ind(sam))

quit()

print(Bt_prob_abu_fiddle(sam))

pro = replicate(bootSamps, {
	aProb <- Bt_prob_abu(sam)
	aProb[is.na(aProb)] <- 0
	data.bt = rmultinom(1,sampleSize,aProb)
	return(Chao_Hill_abu(data.bt,q))
})
pro<-pro-mean(pro)+obs
chaotile<-sum(pro<=truemun)/(bootSamps/100)

quit()

