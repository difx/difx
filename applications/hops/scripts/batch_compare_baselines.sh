alist1=$1
alist2=$2

awks='NR>4{a=substr($15,1,1);b=substr($15,2,1)}a!=b{print a b}'
bl=`awk "$awks" $alist1 | sort | uniq | tr \\\\012 ' '`

echo
echo "Comparing $alist1 to $alist2"
echo "Only considering scans with snr>7"
echo "Amplitude ratios >1 mean that alist2 is improved compared to alist1"
echo

for b in $bl ;
  do echo -n "$b " && compare-baselines-v6.pl -a $alist1 -b $alist2 -f -m 7 -x $b | grep Median ; done
