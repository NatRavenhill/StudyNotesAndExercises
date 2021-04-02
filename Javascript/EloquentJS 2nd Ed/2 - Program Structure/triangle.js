/*A  loop that makes seven calls to console.log to output the following triangle:
#
##
###
####
#####
######
#######*/

var hash = "#";

for(var i = 0; i < 7; i++){
    console.log(hash);
    hash += "#";
}
