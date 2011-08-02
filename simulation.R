get.data = function(demography=list(), T=2004-1975, data=list()) {
  # demographic stuff
  # maybe use leslie??
  population_data = read.csv2('Data/used for simulation/population_1975_smoothed.csv', stringsAsFactors=F)
  
  yearly_data = read.csv2('Data/used for simulation/birthrate_mortality.csv', stringsAsFactors=F)
  
  demography$population = round(as.numeric(population_data$Population)) # round is necessary because we have comma values as population is smoothed

  demography$in_workforce = rep(0, 150)
  probability = yearly_data$labor.force..oecd.[yearly_data$Year==1975]/sum(demography$population[16:66]) # probability that someone who is in working age (15 to 65...) is in the labor force
  for (i in 16:66)
    demography$in_workforce[i] = rbinom(1, demography$population[i], probability) # this assumes that labor force is evenly distributed among the population
  demography$out_workforce = demography$population - demography$in_workforce

  demography$employed = rep(0, 150)
  probability = yearly_data$employment..oecd./sum(demography$in_workforce) # probability that a random workforce participant is employed  
  for (i in 16:66)
    demography$employed[i] = rbinom(1, demography$in_workforce[i], probability) # this assumes that employed is evenly distributed among the workforce
  demography$unemployed = demography$in_workforce - demography$employed

  demography$in_to_employment = as.numeric(data$pr_out_of_AL.grund) # vector of length 30, 1 for each year
  demography$out_of_employment = as.numeric(data$pr_in_to_AL.grund) # vector of length 30
  demography$out_of_workforce = as.numeric(data$pr_out_of_workforce) # vector of length 30
  demography$in_to_workforce = demography$out_of_workforce - runif(length(demography$out_of_workforce),0,demography$out_of_workforce) # dunno... Rente ist klar... Vlt. nehm ich tatsächliche workforce irgendwoher und berechne mittels einer anderen simulation wie die Wahrscheinlichkeit sein muss?
  
  demography$total_jobs = 40000000 # dunno!
# I need long-run values for the following: (even if I find year to year data from 1975 to 2004 because of the projection from 2004 to 2009)
  demography$fertility[1:150] = as.numeric(population_data$birth.rate) # FUCK I DONT HAVE THEM BY AGE, I only have one global value... that's bullshit
  demography$mortality[1:150] = as.numeric(population_data$death.rate) # FUCKING SHIT

  # demography$fertility = matrix(rep(c(rep(0,16), rep(0.10, 14), rep(0.075, 10), rep(0,110)), T), ncol=T) # dunno
  # demography$mortality = matrix(rep(c(rep(0.03,5), rep(0.005, 11), rep(0.0075, 14), rep(0.0075,10),rep(0.05,10),rep(0.15,10),rep(0.2,10),rep(0.4,10),rep(0.5,70)), T), ncol=T) # dunno
  demography$mortality[150] = 1 # at that age everybody has to die
  
  demography$migration = as.numeric(yearly_data$migration..OECD.)
  
  return (demography)
}

set.data = function(T = 2004-1975, data=list()) {
  set.seed(1)
  
  
  # the following variables are stock variables per age cohort. They can be changed to include time. For example by defining them as a matrix with columns representing time points
  # population = rep(0,150) # total population divided by age cohorts. 150 cohorts (1 per year of age)
  # employed = rep(0,150) # number of employed. 150 cohorts (1 per year of age)
  # unemployed = rep(0,150) # number of unemployed. 150 cohorts (1 per year of age)
  # out_workforce = rep(0,150) # number of people not in the workforce. 150 cohorts (1 per year of age) 
  # in_workforce = rep(0,150) # number of people in the workforce. 150 cohorts (1 per year of age)
  # open_jobs = rep(0,150) # (will be ignored for now... maybe use later for more complicated simulation) 150 cohorts (1 per year of age)
  
  # the following variables are flow variables per age cohort (see above) 
  #crude_birth_rate = matrix(rep(0,150*T), 150) # each Kohort has a different cbr and it changes potentially with t
  # fertility = matrix(rep(0,150*T), 150) # each Kohort has a different fertility and it changes potentially with t
  # mortality = matrix(rep(0,150*T), 150) # each Kohort has a different mortality and it changes potentially with t
  # out_of_workforce = matrix(rep(0,150*T), 150) # each Kohort has a different behaviour and it changes potentially with t
  # in_to_workforce = matrix(rep(0,150*T), 150) # each Kohort has a different behaviour and it changes potentially with t
  # net_migration = matrix(rep(0,150*T), 150) # net_migration per age cohort and time t
  # in_to_employment = matrix(rep(0,150*T), 150) # each Kohort has a different behaviour and it changes potentially with t
  # out_of_employment = matrix(rep(0,150*T), 150) # each Kohort has a different behaviour and it changes potentially with t
  #population_growth = matrix(rep(0,150*T), 150) # population increases through migration and births...
  
# initializion:
  demography = get.data(T=T, data=data)
  return (demography)
}

do.simulation = function() {
  data = read.csv2('Data/extracted from iab/csv/probabilities.csv', stringsAsFactors=F)
  
  start = min(as.numeric(data$Time))
  #end = max(as.numeric(data$Time))
  end = 2010
  data = set.data(length(data$Time), data) # initialize everything
  
  data$initial_data = data
  for (t in start:end) {
    T = t-(start-1) # year counter, starts from 1
    if (t>2004) 
      T=2004-(start-1)
    # population stuff

    data = deaths(data)
    #print('data after deaths');print(data$unemployed)
    data = age(data) # let population age...
    #print('data after age');print(data$unemployed)
    data = births(data) # put births...
    #print('data after births');print(data$unemployed)	
    data = migrate(data, T=T) # put births...
  # workforce stuff
   
    change = move_workforce(data, T=T) 
    data = change_employment_on_workforce_change(data, change, T=T)
    data$out_workforce = data$out_workforce + change$move_out_workforce - change$move_in_workforce
    data$in_workforce = data$in_workforce + change$move_in_workforce - change$move_out_workforce
    #print('data after move_workforce');print(data$unemployed) 
    
 # employment effect of workforce movement:   
  # employment stuff
    change = change_employment(data)
    data$unemployed = data$unemployed + change$unemploy - change$employ
    data$employed = data$employed - change$unemploy + change$employ
    
    
    #print(data$in_workforce)
    #print(data$employed)
    #print(data$unemployed)
    
    
    print(paste('t: ',t));
    print(paste('population: ', sum(data$population)))
    print(paste('in workforce: ', sum(data$in_workforce)))
    print(paste('employed: ', sum(data$employed)));
    print(paste('unemployed: ', sum(data$unemployed)));
    print(paste('unemployment rate: ', sum(data$unemployed)/sum(data$in_workforce))); 
  }

  return (data)
}