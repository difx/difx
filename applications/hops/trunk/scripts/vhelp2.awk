BEGIN {IGNORECASE=1;here=0; ok=0;}
match($1,section)      { here=1; }
match($0,/^[A-Z]/)&&ok { here=0; }
here == 1          {print $0;ok++}
