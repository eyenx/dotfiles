RESULT=$(ping -w 4 -c 1 8.8.8.8)

if [[ $? -gt 0 ]]
then
    LAT="offline"
else
    LAT=$(echo $RESULT | sed -n 's/.*time\=\([0-9.]\+ ms\).*/\1/p')
fi

echo $LAT
