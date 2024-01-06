# get the first argument
day=$1

# Create input directory and files
mkdir ./inputs/day$day
touch ./inputs/day$day/test.txt
touch ./inputs/day$day/input.txt

# Create the source file

touch ./lib/day$day.ml

# populate the source file
echo "let input_test = \"./inputs/day$day/test.txt\"" >> ./lib/day$day.ml
echo "let input_file = \"./inputs/day$day/input.txt\"" >> ./lib/day$day.ml
echo "let part1 = 0" >> ./lib/day$day.ml
echo "let part2 = 0" >> ./lib/day$day.ml

# open the source file
nvim ./lib/day$day.ml
