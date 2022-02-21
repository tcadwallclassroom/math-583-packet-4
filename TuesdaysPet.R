rm(list=ls())
petsim<-function(N=100,print_cases=TRUE)
{
  weekday<-c('M','Tu','W','Th','F','Sa','Su') #initialize the days of the week
  pet_type<-c('dog','cat') #initialize the pet type
  has_dog <- 0 #keep track of cases with at least one dog
  has_2_dogs <- 0 #keep track of cases with two dogs
  has_dog_on_tu <- 0 #keep track of cases with at least one dog adopted on a Tuesday
  has_2_dogs_and_dog_on_tu <- 0 #keep track of cases with at least one dog adopted on a Tuesday and has 2 dogs
  
  for(i in 1:N)
  {
    pet_1_day <- sample(weekday,1) #choose pet 1's adoption day
    pet_1_type <- sample(pet_type,1) #choose pet 1's type
    pet_2_day <- sample(weekday,1) #choose pet 2's adoption day
    pet_2_type <- sample(pet_type,1) #choose pet 1's type
    if(pet_1_type=='dog' | pet_2_type=='dog')
    {
      has_dog <- has_dog+1
    }
      if(pet_1_type=='dog' & pet_2_type=='dog')
    {
        has_2_dogs <- has_2_dogs+1
    }
      if((pet_1_day=='Tu' & pet_1_type=='dog')|(pet_2_day=='Tu' & pet_2_type=='dog'))
    {
        has_dog_on_tu <- has_dog_on_tu+1
      }
    if((pet_1_day=='Tu' & pet_1_type=='dog' & pet_2_type=='dog')|(pet_2_day=='Tu' & pet_1_type=='dog' & pet_2_type=='dog'))
    {
      has_2_dogs_and_dog_on_tu <- has_2_dogs_and_dog_on_tu+1
    }
    
    
    if(print_cases)
      cat(paste('pet 1: ',pet_1_type,' adopted on ',pet_1_day,
                '\npet 2: ',pet_2_type,' adopted on ',pet_2_day,
                '\n\n',sep=''))
  }
  cat(paste('Of a total of ',N,' cases,',
            '\n there are ',has_dog,' cases that have at least one dog ',
            '\n and of those ',has_2_dogs,' have 2 dogs,',
            '\n which is ',has_2_dogs/has_dog*100,'% of the cases with at least one dog.',
            '\n',
            '\n There are ',has_dog_on_tu,' cases that have at least one dog adopted on a Tuesday',
            '\n and of those ',has_2_dogs_and_dog_on_tu,' have 2 dogs,',
            '\n which is ',has_2_dogs_and_dog_on_tu/has_dog_on_tu*100,'% of the cases with at least one dog adopted on a Tuesday.',
            '\n\n',sep=''))
}
