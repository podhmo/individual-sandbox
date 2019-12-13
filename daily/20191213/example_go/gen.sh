set -e

for i in $(seq 4 5); do
src=$(echo $(printf "%02d" $i)*)
dst=$(echo $src | sed 's/^0/1/')
cp -r $src $dst
sed -i 's/findcaller/findcaller2/g' $dst/main.go
done
