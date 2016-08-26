getIt <- function(score,terms)
{
  dict = ""
  for(x in 1:length(score))
  {
    if(!is.na(score[x]))
    {
      dict <- paste(dict,terms[score[x]])
    }
    
  }
  return(dict)
}