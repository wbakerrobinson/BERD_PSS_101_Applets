# Function to get the test statistic depending on whther the specified hypothesis is one or two sided
calc_test_stat <- function(df, alpha = 0.05, two_sided = TRUE)
{
  test_stat <- 0
  if(two_sided)
    test_stat <- qt(1-alpha/2, df)
  else
    test_stat <- qt(1-alpha, df)
  return(test_stat)
}