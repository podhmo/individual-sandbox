# bash こういうの楽にやりたい

```bash
STAGE=0
GOAL="$1"

if [ -z ${GOAL} ]; then
    GOAL=10000
fi

echo "start", ${STAGE}

if [[ ${STAGE} -lt ${GOAL} ]]; then
    echo "tick", ${STAGE}
    STAGE=`expr ${STAGE} + 1`
fi
if [[ ${STAGE} -lt ${GOAL} ]]; then
    echo "tick", ${STAGE}
    STAGE=`expr ${STAGE} + 1`
fi
if [[ ${STAGE} -lt ${GOAL} ]]; then
    echo "tick", ${STAGE}
    STAGE=`expr ${STAGE} + 1`
fi
if [[ ${STAGE} -lt ${GOAL} ]]; then
    echo "tick", ${STAGE}
    STAGE=`expr ${STAGE} + 1`
fi
if [[ ${STAGE} -lt ${GOAL} ]]; then
    echo "tick", ${STAGE}
    STAGE=`expr ${STAGE} + 1`
fi
if [[ ${STAGE} -lt ${GOAL} ]]; then
    echo "tick", ${STAGE}
    STAGE=`expr ${STAGE} + 1`
fi
if [[ ${STAGE} -lt ${GOAL} ]]; then
    echo "tick", ${STAGE}
    STAGE=`expr ${STAGE} + 1`
fi
if [[ ${STAGE} -lt ${GOAL} ]]; then
    echo "tick", ${STAGE}
    STAGE=`expr ${STAGE} + 1`
fi
echo "end", ${STAGE}
```

```
$ bash /tmp/a.sh
start, 0
tick, 0
tick, 1
tick, 2
tick, 3
tick, 4
tick, 5
tick, 6
tick, 7
end, 8

$ bash /tmp/a.sh 3
start, 0
tick, 0
tick, 1
tick, 2
end, 3
```
