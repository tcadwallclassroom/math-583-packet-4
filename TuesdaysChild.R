rm(list=ls())
childsim<-function(N=100,print_families=TRUE)
{
  weekday<-c('M','Tu','W','Th','F','Sa','Su') #initialize the days of the week the child can be born on
  sex<-c('Boy','Girl') #initialize the sex of the child
  has_girl <- 0 #keep track of families with at least one girl
  has_2_girls <- 0 #keep track of families with two girls
  has_girl_on_tu <- 0 #keep track of families with at least one girl born on a tuesday
  has_2_girls_and_girl_on_tu <- 0 #keep track of families with at least one girl born on a tuesday and 2 girls
  
  for(i in 1:N)
  {
    child_1_day <- sample(weekday,1) #choose child 1's birthday
    child_1_sex <- sample(sex,1) #choose child 1's sex
    child_2_day <- sample(weekday,1) #choose child 2's birthday
    child_2_sex <- sample(sex,1) #choose child 1's sex
    if(child_1_sex=='Girl' | child_2_sex=='Girl')
    {
      has_girl <- has_girl+1
    }
      if(child_1_sex=='Girl' & child_2_sex=='Girl')
    {
        has_2_girls <- has_2_girls+1
    }
      if((child_1_day=='Tu' & child_1_sex=='Girl')|(child_2_day=='Tu' & child_2_sex=='Girl'))
    {
        has_girl_on_tu <- has_girl_on_tu+1
      }
    if((child_1_day=='Tu' & child_1_sex=='Girl' & child_2_sex=='Girl')|(child_2_day=='Tu' & child_1_sex=='Girl' & child_2_sex=='Girl'))
    {
      has_2_girls_and_girl_on_tu <- has_2_girls_and_girl_on_tu+1
    }
    
    
    if(print_families)
      cat(paste('Child 1: ',child_1_sex,' born on ',child_1_day,
                '\nChild 2: ',child_2_sex,' born on ',child_2_day,
                '\n\n',sep=''))
  }
  cat(paste('Of a total of ',N,' families,',
            '\n there are ',has_girl,' families that have at least one girl ',
            '\n and of those ',has_2_girls,' have 2 girls.',
            '\n which represents ',has_2_girls/has_girl*100,'%.',
            '\n',
            '\n There are ',has_girl_on_tu,' families that have at least one girl born on a Tuesday',
            '\n and of those ',has_2_girls_and_girl_on_tu,' have 2 girls.',
            '\n which represents ',has_2_girls_and_girl_on_tu/has_girl_on_tu*100,'%.',
            '\n\n',sep=''))
}
