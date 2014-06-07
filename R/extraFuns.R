gneFurlani = function(instrs, repo = GRANRepo$repo)
{
    ##[[1]] because strsplit always returns a list with one element per vector element of the input
    modules = strsplit(instrs, split =  "[[:space:]]*;[[:space:]]*")[[1]]
    
    cmd = paste(paste("module load", modules), collapse = " ; ")
    res = system_w_init(cmd, repo)

    res==0
}
