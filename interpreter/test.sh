echo "-------------------------- RUNNING GOOD EXAMPLES --------------------------"

for f in `ls good/*`
do
  echo "---------- Running good: $f ----------"
  ./interpreter $f
done

echo "-------------------------- RUNNING BAD EXAMPLES --------------------------"

for f in `ls bad/*`
do
  echo "---------- Running bad: $f ----------"
  ./interpreter $f
done