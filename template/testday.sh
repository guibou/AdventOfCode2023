dayNumber=$(printf "%02d" $1)
echo $dayNumber

cp template/DayXSpec.hs tests/Day$dayNumberSpec.hs
sed -i "s/DayX/Day$dayNumber/" tests//Day$dayNumberSpec.hs

git add tests/Day$dayNumberSpec.hs
