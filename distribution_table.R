# dist_table is a matrix with the values of the botton and top of the range in the 1st and 2nd columns

pdist_to_dist_table = function(pdist, tab_size_exp=2) {
  tab_size = 10^tab_size_exp
  fmt = paste("%0", tab_size_exp, "d", sep="")
  last_value = 0
  next_i = 1
  num_rows = NA
  retval = matrix(NA, nrow=tab_size, ncol=3)
  for (i in 1:tab_size) {
    value = round(pdist(i) * tab_size)
    if (value > last_value) {
      retval[next_i, 1] = last_value + 1
      retval[next_i, 2] = value
      num_rows = next_i
      next_i = next_i + 1
    }
    last_value = value
  }
  retval[1:num_rows, 1:2]
}

print_dist_table = function(dist_table, tab_size_exp=2) {
  tab_size = 10^tab_size_exp
  fmt = paste("%0", tab_size_exp, "d", sep="")
  for (i in 1:nrow(dist_table)) {
    bottom = dist_table[i, 1]
    top = dist_table[i, 2]
    line = ""
    if (bottom == top) {
      line = paste(sprintf(fmt, bottom %% tab_size),
                   "\t",
                   i,
                   "\n",
                   sep="")
    } else {
      line = paste(sprintf(fmt, bottom %% tab_size),
                   "-",
                   sprintf(fmt, top %% tab_size),
                   "\t",
                   i,
                   "\n",
                   sep="")
    }
    cat(line)
  }
}

tab = pdist_to_dist_table(function(x) { pexp(x, rate=0.4)})
print_dist_table(tab)
barplot(tab[,2] - tab[,1] + 1)