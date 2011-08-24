age = function(demography=list(), T=1) {
  # direct effect (ageing):
  # print(demography)
  for (i in (length(demography$population)-1):1) {
    demography$population[i+1] = demography$population[i] # population ages
    demography$in_workforce[i+1] = demography$in_workforce[i] # workforce ages
    demography$out_workforce[i+1] = demography$out_workforce[i] # people out of workforce age
    demography$unemployed[i+1] = demography$unemployed[i] # unemployed age
    demography$employed[i+1] = demography$employed[i] # employed age
  }
  demography$population[1] = 0
  # workforce effects
  demography$in_workforce[1:15] = 0 # 15 years is the youngest you can enter labor market? (MAKE THIS PURELY STOCHASTIC???)
  demography$in_workforce[16] = demography$population[16] # 15 year olds are added to workforce --> what to do about university and further schooling? Does everybody enter labor market? NO!!
  demography$in_workforce[66:150] = 0 # Pension at the age of 65?
  demography$out_workforce[1:15] = demography$population[1:15] # everybody of that age is out of workforce
  demography$out_workforce[16] = 0 # everybody starts in the workforce??? this might have to be changed
  demography$out_workforce[66:150] = demography$population[66:150] # Pension at the age of 65?

  # employment effects
  demography$employed[1:15] = 0 # cannot be in workforce cannot be employed
  demography$employed[66:150] = 0 # Pension at the age of 65?
  demography$unemployed[1:15] = 0 # cannot be in workforce cannot be unemployed
  pr_employed = sum(demography$employed)/sum(demography$in_workforce)
  demography$employed[16] = round(pr_employed * demography$in_workforce[16])
  demography$unemployed[16] = demography$in_workforce[16] - demography$employed[16] # ...
  demography$results$out_of_workforce_unemployed[66,T+1] = demography$unemployed[66]
  demography$results$out_of_unemployment[66,T+1] = demography$results$out_of_unemployment[66,T+1] + demography$unemployed[66] # all unemployed people get counted
  demography$unemployed[66:150] = 0 # Pension at the age of 65?
  
# the following is only needed if I don't use a matching model
  #unemployed_65 = sum(rbinom(demography$population[64], 1, demography$unemployed[64]/demography$population[64])) # each of the people who starts pension was either employed or unemployed. Since we don't have data on each person we use percentage of unemployed people as a proxy.
  #demography$unemployed = demography$unemployed - unemployed_65 # formerly unemployed are now in pension and thus not unemployed anymore
  #demography$employed = demography$employed - (demography$population[65] - unemployed_65) # some employed people will go in to pension --> employment shrinks...
  
  return (demography)
}


# Shall I simulate births and deaths with random variables here or in the definition of fertility and mortality?
births = function(demography = list(), T=1) {
  births = round(sum(demography$population * demography$fertility)) # taking age specific fertility rate for now... THIS IS NOT RANDOM...
  demography$population[1] = births
  demography$out_workforce[1] = demography$population[1] # newborn children are to young to work
  return (demography)
}

deaths = function(demography=list(), T=1) {
  deaths = rep(0, 150)
  deaths_in_workforce = rep(0, 150)
  
  for (i in 1:150) {
    deaths[i] = rbinom(1, demography$population[i], demography$mortality[i]) # taking age specific mortality rate for now... 
    pr_to_die = deaths[i]/demography$population[i]
    
    if (demography$population[i] == 0) # catch NaNs
      pr_to_die = 0
    deaths_in_workforce[i] = rbinom(1, demography$in_workforce[i], pr_to_die) # This assumes that dying is equally as likely when your employed as when your unemployed
    
    if (deaths[i] < deaths_in_workforce[i])
      deaths_in_workforce[i] = deaths[i] # because its impossible. Need to think about whether this distorts probability CHANGE then
    if (demography$out_workforce[i] - (deaths[i] - deaths_in_workforce[i]) < 0) # same with out_workforce as with in_workforce below
      deaths_in_workforce[i] = deaths[i] - demography$out_workforce[i] # because its impossible. Need to think about whether this distorts probability CHANGE then
    if (demography$in_workforce[i] < deaths_in_workforce[i])
      deaths_in_workforce[i] = demography$in_workforce[i] # because its impossible. Need to think about whether this distorts probability CHANGE then
  }
  
  # For now people are taken out of workforce according to the percentage of people being in and out of workforce (i.e. being in workforce is independent of death rate)
    
  change = list(move_out_workforce=deaths_in_workforce, move_in_workforce=0) # change$move_out_workforce is people that were in the workforce of those that have died
  demography = change_employment_on_workforce_change(demography, change, T=T)
  demography$in_workforce = demography$in_workforce - deaths_in_workforce
  demography$out_workforce = demography$out_workforce - (deaths - deaths_in_workforce) # deaths - those that where in workforce
  demography$population = demography$population - deaths
  
  return (demography)
}

migrate = function(demography=list(), T=1) {
  rnd_migration = round(runif(abs(demography$migration[T]), 1, 150)) # assumes migration is evenly distributed among age groups (mighty wrong! change?)
  ages = c()
  for (i in 1:150) {
    ages[i] = length(which(rnd_migration==i))
    
    if (demography$migration[T] < 0 && ages[i] > demography$in_workforce[i]) # says in_workforce because only people form thr workforce migrate CHANGE!!
      ages[i] = demography$in_workforce[i] # stupid because that way the older demographics can not emigrate
  }
  if (demography$migration[T] < 0) {
    # SIMPLIFICATION MIGHT NEED CHANGE:
    demography$in_workforce[16:65] = demography$in_workforce[16:65] - ages[16:65] # assumption: every emmigrant will be in workforce maybe should be changed to probability to be in workforce?
    demography$out_workforce[c(1:15, 66:150)] = demography$out_workforce[c(1:15, 66:150)] + ages[c(1:15, 66:150)] 
    change_employment_on_workforce_change(demography, list(move_out_workforce=ages,move_in_workforce=rep(0, 150)), T)
    demography$population = demography$population - ages # need to take care of workforce and employment still
  }
  else {
    # SIMPLIFICATION MIGHT NEED CHANGE:
    demography$in_workforce[16:65] = demography$in_workforce[16:65] + ages[16:65] # assumption: every immigrant will be in workforce maybe should be changed to probability to be in workforce?
    demography$out_workforce[c(1:15, 66:150)] = demography$out_workforce[c(1:15, 66:150)] + ages[c(1:15, 66:150)] 
    change_employment_on_workforce_change(demography, list(move_out_workforce=rep(0, 150),move_in_workforce=ages), T=T)
    demography$population = demography$population + ages # need to take care of workforce and employment still
  }
  
  return (demography)
}
move_workforce = function(demography=list(), T=1) {
  move_out_workforce = rep(0,150)
  move_in_workforce = rep(0,150)
  for (i in 16:150) { # before 16 you cant be in the workforce
    move_out_workforce[i] = round(rbinom(1, demography$in_workforce[i], demography$out_of_workforce[T]))
    move_in_workforce[i] = round(rbinom(1, demography$out_workforce[i], demography$in_to_workforce[T]))
  }
  return (list(move_out_workforce=move_out_workforce,move_in_workforce=move_in_workforce)) # returns the difference of people movin into workforce and people moving out
}

change_employment = function(demography=list(), T=1) {
 # not needed anymore... 
  #demography$open_jobs = demography$total_jobs - sum(demography$employed)
  #if (demography$open_jobs < 0)
  #  demography$open_jobs = 0
  #employ = demography$unemployed * demography$in_to_employment
  #new_employ = min(demography$open_jobs, round(sum(demography$unemployed) * demography$in_to_employment[T])) # Enforce maximum jobs?? Matching??
  new_employ = round(sum(demography$unemployed) * demography$in_to_employment[T])
  print(paste('new jobs:', new_employ, ' that is:', new_employ/sum(demography$unemployed)))
  #how should employ be distributed among age? Evenly!
  ages = c()
  if (new_employ > 0) {
    pr_unemployed = demography$unemployed/demography$in_workforce
    pr_unemployed[demography$in_workforce==0] = 0
    
    possible=c()
    unemployed = demography$unemployed
    
    ages = match_employment(demography, new_employ)
  }
  employ = ages
  print(paste('of this finally employed:', sum(employ), 'which is:', sum(employ)/sum(demography$unemployed)))
  unemploy = rep(0,150)
  for (i in 1:150) {
    if (demography$employed[i] > 0)
      unemploy[i] = rbinom(1, demography$employed[i], demography$out_of_employment[T]) # everybody who is employed faces probability same to be unemployed
    if (unemploy[i] > demography$employed[i]) # can't be higher than people who are employed
      unemploy[i] = demography$employed[i]
  }
  if (length(employ) == 0)
    employ = 0
  if (length(unemploy) == 0)
    unemploy = 0
  print(paste('finally unemployed:', sum(unemploy), 'which is:', sum(unemploy)/sum(demography$employed)))
  
  #alt = demography$employed*demography$out_of_employment[T]
  #print(paste('Alternative: ', sum(alt)))

return (list(employ=employ,unemploy=unemploy))
}


change_employment_on_workforce_change = function(demography=list(), change=list(), T=1) {
  # people who move out are more complicated!
  # If people move out of workforce we calculate likelihood of them being employed or unemployed and reduce respectively
  pr_employed = demography$employed/demography$in_workforce
  pr_employed[demography$in_workforce==0] = 0 # remove NaNs
  pr_unemployed = 1 - pr_employed
 # people who move in workforce start unemployed for now:  
  demography$unemployed = demography$unemployed + change$move_in_workforce 
 # The following is unrealistic: (assumption would be that people who enter workforce find jobs at the same rate as the employment rate
  #ch_unemployed = round(change$move_in_workforce*pr_unemployed)
  #demography$unemployed = demography$unemployed + ch_unemployed
  #demography$employed = demography$employed + (change$move_in_workforce - ch_unemployed)
  
  for (i in 1:150) {
    if (change$move_out_workforce[i] > 0)
      rnd_employed = rbinom(1, change$move_out_workforce[i], pr_employed[i]) # draw randomly how many per age cohort were employed... the rest were unemployed
    else
      rnd_employed = 0
    
    if (rnd_employed > demography$employed[i]){ # if more are randomly chosen than are actually employed we need to set to actual number to not get negative employments
      rnd_employed = demography$employed[i] # need to think if this distorts likelihood (if yes change it!) maybe not because appears to only happen when employed==0
    }
 
    if ((change$move_out_workforce[i] - rnd_employed) > demography$unemployed[i]) { # if more are randomly chosen to be unemployed than are actually unemployed we need to set to actual number to not get negative unemployments
      rnd_employed = change$move_out_workforce[i] - demography$unemployed[i] # need to think if this distorts likelihood (if yes change it!)
    }
    
    if (length(rnd_employed) == 0)
      print('length(null)');
    
    demography$employed[i] = demography$employed[i] - rnd_employed
    demography$unemployed[i] = demography$unemployed[i] - (change$move_out_workforce[i] - rnd_employed)
    
    demography$results$out_of_workforce_unemployed[i,T+1] = demography$results$out_of_workforce_unemployed[i,T+1] + (change$move_out_workforce[i] - rnd_employed)
    demography$results$out_of_unemployment[i,T+1] = demography$results$out_of_unemployment[i,T+1] + (change$move_out_workforce[i] - rnd_employed)
  }
  
  return (demography)
}

match_employment = function(demography, new_employ, T=1) {
  # First idea: take global probability that people get employed, apply that to each age cohort and simply enforce upper limit of unemployment (does this distort probability, though? If yes it needs change)
  employ = rep(0, 150)
  probability = new_employ / sum(demography$unemployed)
  for (i in 1:150) {
    employ[i] = rbinom(1, demography$unemployed[i], probability)
    if (employ[i] > demography$unemployed[i])
      # I could increase probability after that. Its possible: 
      #cut_off[i] = employ - demography$unemployed
      #probability = (new_employ-sum(cut_off)) / sum(demography$unemployed - sum(employ)) # I think this should work as intended to countervene drops in probability
      employ[i] = demography$unemployed[i]
    if (sum(employ) >= new_employ) {# we have employed to many people.
      # since this has started in this age cohort we can fire some people from this cohort again
      employ[i] = employ[i] - (sum(employ)-new_employ)
      break; # break so we dont employ more people --> This assumes that young cohorts get employed first while old cohorts might not get employed at all
    }
  }
  return (employ)
}