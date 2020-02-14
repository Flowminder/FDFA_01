format_n=function(x){
  formatted_n=round(x)
  if(x>10000){
    formatted_n=format(round(x),big.mark = "'",round=-2)
    
  }
  if(x>100000){
    formatted_n=paste(format(round(x/ 1e3),big.mark = "'",trim=T),"k")
  }
  if(x>1000000){
    formatted_n=paste(format(round(x/ 1e6,2),big.mark = "'",trim=T),"M")
  }
  return(formatted_n)
}