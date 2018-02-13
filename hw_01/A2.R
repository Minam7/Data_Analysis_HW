library(gtools)
library(combinat)

a_perms = permutations(n=13, r=5, v=1:13, repeats.allowed = FALSE)

seen_all_perms = length(a_perms)/5
a1_to_a5 = matrix(ncol = 5)

for(perm_index in 1:seen_all_perms) {
  
  choosen_perm = a_perms[perm_index,]
  if((choosen_perm[1] < choosen_perm[2] + choosen_perm[3]) & (choosen_perm[1] < choosen_perm[3] + choosen_perm[4] + choosen_perm[5]) & (choosen_perm[2] < choosen_perm[4] + choosen_perm[5]) & (choosen_perm[2] + choosen_perm[3])^2 == choosen_perm[1]*(choosen_perm[3] + choosen_perm[4] + choosen_perm[5])){
    a1_to_a5 = rbind(a1_to_a5, choosen_perm)
  }
  perm_index = perm_index + 1
}
a1_to_a5 = a1_to_a5[-1,]
# found answer for a1 to a5

seen_all_perms = length(a1_to_a5)/5
a1_to_a13 = vector(mode = 'numeric', length = 13)
for(perm_index in 1:seen_all_perms){
  a_perms = permn(setdiff(1:13,a1_to_a5[perm_index,]))
  a_choosen_perm = a1_to_a5[perm_index, 5]
  find_flag = logical(1)
  
  for(index in 1:length(a_perms)){
    choosen_perm = a_perms[[index]]
    if((choosen_perm[6] < choosen_perm[8]) & (choosen_perm[7] < choosen_perm[5]) & (choosen_perm[1] < choosen_perm[3] + choosen_perm[4]) & (choosen_perm[2] + choosen_perm[3] < a_choosen_perm + choosen_perm[5]) & ((choosen_perm[1] + choosen_perm[2])^2 == choosen_perm[1]*(choosen_perm[2] + choosen_perm[3] + choosen_perm[4])) & ((choosen_perm[2]+choosen_perm[3] + choosen_perm[4])^2 == (choosen_perm[1] + choosen_perm[2])*(a_choosen_perm + choosen_perm[4] + choosen_perm[5])) & ((choosen_perm[7] + choosen_perm[8])^2 == (choosen_perm[6] + choosen_perm[7])*(choosen_perm[8] + choosen_perm[5]))){
      find_flag = TRUE
      print("a1 to a5:")
      print(a1_to_a5[perm_index,])
      print("\na6 to a13:")
      print(choosen_perm)
    }
    if(find_flag){
      break
    }
    
    index = index + 1
  }
}