
## WARNING: This clears all objects out of your memory.
## This can be a good thing.
rm(list=ls())

monty<-function(strat='stay',N=1000,print_games=TRUE)
{
  doors<-1:3 #initialize the doors behind one of which is a good prize
  win<-0 #to keep track of number of wins
  for(i in 1:N)
  {
    prize <- sample(doors,1) #randomize which door has the good prize
    guess <- sample(doors,1) #guess a door at random
    ## Reveal one of the doors you didn't pick which has a bum prize
    if(prize != guess)
      reveal <- doors[-c(prize,guess)]
    else
      reveal<-sample(doors[-c(prize,guess)],1)
    ## Stay with your initial guess or switch
    if(strat=='switch')
      select<-doors[-c(reveal,guess)]
    if(strat=='stay')
      select<-guess
    if(strat=='random')
      select<-sample(doors[-reveal],1)
    ## Count up your wins
    if(select==prize)
    {
      win<-win+1
      outcome<-'Winner!'
    }else
      outcome<-'Loser!'
    if(print_games)
      cat(paste('Guess: ',guess,
                '\nRevealed: ',reveal,
                '\nSelection: ',select,
                '\nPrize door: ',prize,
                '\n',outcome,'\n\n',sep=''))
  }
  cat(paste('Using the ',strat,' strategy, your win percentage was ',win/N*100,'%\n',sep='')) #Print the win percentage of your strategy
}

## Code from: https://www.r-bloggers.com/2012/02/monty-hall-by-simulation-in-r/

## Examples:
## monty(strat="stay",N=10)
## monty(strat="switch",N=10)
## monty(strat="stay",1000,FALSE)