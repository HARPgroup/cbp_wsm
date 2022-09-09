library("hydrotools")
library("rapportools") # note this is needed because hydrotools does not load this lib correctly
library("stringr") # note this is needed because hydrotools does not load this lib correctly

argst <- commandArgs(trailingOnly=T)
print(argst)
all_segs <- str_split(as.character(argst[1]), " ")[[1]]
print(paste("all segs:",all_segs))

fn_stream_order <- function (seg, AllSegList) {
  itribs <- fn_upstream(seg,AllSegList)
  if (itribs[1] == "NA") {
    stream_order = 1
  } else {
    trib_order = c()
    for (t in itribs) {
      trib_order[t] = fn_stream_order(t, AllSegList)
    }
     stream_order = 1 + max(trib_order)
  }
  return(stream_order)
}

for (k in all_segs) {
  korder <- fn_stream_order(k,all_segs)
  print(paste(k, korder))
}
