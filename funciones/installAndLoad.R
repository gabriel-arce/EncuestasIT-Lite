installAndLoad <- function(requiredPackages){
  remainingPackages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])];
  
  if(length(remainingPackages)){
    install.packages(remainingPackages);
    
  }
  
  for(package_name in requiredPackages){
    library(package_name, character.only = TRUE, quietly = TRUE);
    
  }
}