rm(list=ls())
dicerolls<-function(N=100,print_results=TRUE)
{
  die<-1:6
  is_even <- 0 #keep track of A
  is_at_least_ten <- 0 #keep track of B
  red_is_five <- 0 #keep track of C
  red_is_five_and_is_even <- 0 #Keep track of A and C
  red_is_five_and_sum_is_at_least_ten <- 0 #Keep track of B and C
  is_even_and_is_at_least_ten <- 0 #Keep track of A and B
  
  for(i in 1:N)
  {
    red <- sample(die,1) #roll red
    blue <- sample(die,1) #roll blue
    sum_dice <- sum(red,blue) #calculate the sum of the dice
    if((sum_dice %% 2) == 0)
    {
      is_even <- is_even+1
    }
    if(sum_dice>=10)
    {
      is_at_least_ten <- is_at_least_ten+1
    }
    if(red==5)
    {
      red_is_five <- red_is_five+1
    }
    if((red==5)&(sum_dice>=10))
    {
      red_is_five_and_sum_is_at_least_ten <- red_is_five_and_sum_is_at_least_ten+1
    }
    if((red==5)&((sum_dice %% 2) == 0))
    {
      red_is_five_and_is_even <- red_is_five_and_is_even+1
    }
    if(((sum_dice %% 2) == 0)&sum_dice>=10)
    {
      is_even_and_is_at_least_ten <- is_even_and_is_at_least_ten+1
    }
    
    if(print_results)
      cat(paste('Red: ',red,' Blue: ',blue,
                '\nSum: ',sum_dice,
                '\n\n',sep=''))
  }
  cat(paste('Of a total of ',N,' rolls,',
            '\n there are ',is_even,' rolls that are even ',
            '\n which represents ',is_even/N*100,'%.',
            '\n',
            '\n there are ',is_at_least_ten,' rolls that are at least 10 ',
            '\n which represents ',is_at_least_ten/N*100,'%.',
            '\n',
            '\n there are ',red_is_five,' rolls with a red die of 5 ',
            '\n which represents ',red_is_five/N*100,'%.',
            '\n',
            '\n there are ',red_is_five,' rolls with a red die of 5 ',
            '\n and of those ',red_is_five_and_is_even,' are also even,',
            '\n which represents ',red_is_five_and_is_even/red_is_five*100,'%.',
            '\n',
            '\n there are ',red_is_five,' rolls with a red die of 5 ',
            '\n and of those ',red_is_five_and_sum_is_at_least_ten,' are also at least 10,',
            '\n which represents ',red_is_five_and_sum_is_at_least_ten/red_is_five*100,'%.',
            '\n',
            '\n there are ',is_at_least_ten,' rolls with a sum of at least 10 ',
            '\n and of those ',is_even_and_is_at_least_ten,' are also even,',
            '\n which represents ',is_even_and_is_at_least_ten/is_at_least_ten*100,'%.',
            '\n\n',sep=''))
}

