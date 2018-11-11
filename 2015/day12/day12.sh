# part 1
cat input | sed -r -e 's/[^-0-9]/ /g;s/\s+/\n/g' | awk '{s+=$1} END {print s}'

#cat input | sed -r -e 's/\{.*"red".*\}//g;s/[^-0-9]/ /g;s/\s+/\n/g' | awk '{s+=$1} END {print s}'
