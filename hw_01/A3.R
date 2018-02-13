# first
library(combinat)
find = TRUE
x = permutations(2, 16, c("T","L"), repeats.allowed = TRUE)
table = vector("character", 18)
for(i in 1:(length(x)/16)){
  table[1] = x[i,16]
  table[18] = x[i,1]
  for(j in 2:17){
    table[j] = x[i, j-1]
  }
  for(z in 2:17){
    if (table[z] == "L"){
      if(table[z-1] == "L" && table[z+1] == "L"){
        find = FALSE
        break()
      }
    }
    else {
      if(table[z-1] == "T" && table[z+1] == "T"){
        find = FALSE
        break()
      }
    }
  }
  if(!find){
    find = TRUE
  }
  else{
    print(x[i,])
    break()
  }
}

# second
find = TRUE
x = permutations(2, 12, c("T","L"), repeats.allowed = TRUE)
table = vector("character", 14)
for(i in 1:(length(x)/12)){
  table[1] = x[i,12]
  table[14] = x[i,1]
  for(j in 2:13){
    table[j] = x[i, j-1]
  }
  for(z in 2:13){
    if((table[z] == "L" && table[z-1] == "L" && table[z+1] == "T") || (table[z] == "L" && table[z-1] == "T" && table[z+1] == "L") || (table[z] == "T" && table[z-1] == "T" && table[z+1] == "T") || (table[z] == "T" && table[z-1] == "L" && table[z+1] == "L")){
      is_valid = FALSE
      break()
    }
  }
  if(!find){
    find = TRUE
  }
  else{
    print(x[i,])
    break()
  }
}

# third
find = TRUE
lt = 0
ll = 0
x = permutations(2, 8, c("T","L"), repeats.allowed = TRUE)
table = vector("character", 10)
for(i in 1:(length(x)/8)){
  table[1] = x[i,8]
  table[10] = x[i,1]
  for(j in 2:9){
    table[j] = x[i, j-1]
  }
  for(z in 2:9){
    if((table[z] == "L" && table[z-1] == "L" && table[z+1] == "L") || (table == "T" && table[z-1] == "L" && table[z+1] == "T") || (table[z] == "T" && table[z-1] == "T" && table[z+1] == "L")){
      lt = lt + 1
      if (lt > 4){
        find = FALSE
        break()
      }
    }
    else if((table[z] == "T" && table[z-1] == "L" && table[z+1] == "L") || (table[z] == "L" && table[z-1] == "T" && table[z+1] == "L") || (table[z] == "L" && table[z-1] == "L" && table[z+1] == "T")){
      ll = ll + 1
      if (ll > 4){
        find = FALSE
        break()
      }
    }
    else if(table[z] == "L" && table[z-1] == "T" && table[z+1] == "L"){
      if(ll > lt){
        lt = lt + 1
      }
      else {
        ll = ll + 1
      }
    }
    else if(table[z] == "T" && table[z-1] == "T" && table[z+1] == "T"){
      find = FALSE
      break()
    }
  }
  if(!find){
    find = TRUE
    ll = 0
    lt = 0
  }
  else{
    print(x[i,])
    break()
  }
}
