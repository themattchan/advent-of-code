#! /bin/sh

part1() {
    sed -r -e 's/[^-0-9]/ /g;s/\s+/\n/g' | awk '{s+=$1} END {print s}'
}

cat input | part1

part2() {
    jq -f remove_red.jq | part1
}

cat input | part2

## PART 2 TESTS

# testN=1
# test2() {
#     if [ $(echo $1 | part2) -eq $2 ]; then
#         echo "Part 2 Test #$testN PASS"
#     else
#         echo "Part 2 Test #$testN FAIL"
#     fi
#     ((testN++))
# }
# test2 '[1,2,3]' 6
# test2 '[1,{"c":"red","b":2},3]' 4
# test2 '{"d":"red","e":[1,2,3,4],"f":5}' 0
# test2 '[1,"red",5]' 6
