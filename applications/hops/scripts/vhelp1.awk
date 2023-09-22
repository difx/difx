#{sub(/.SYNOPSIS:/,"\t");print "vhelp:  " $0}
{split($0,x,/:/);sub(/.doc/,"",x[1]);\
       printf "vhelp:  %-11.11s %s\n",x[1],x[3]}
