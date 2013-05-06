#!/usr/bin/env bash
NC="nc localhost 2222"
DATE=`date +"%m%d-%H:%M:%S"`
CORRECT_DEVICE_TOKEN=130ab12bc1ef517bc574cb1199051a88057f6a9371028005f5e780cdb1588d49
ERROR_DEVICE_TOKEN=130ab12bc1ef517bc574cb1199051a88057f6a9371028005f5e780cdb1588d48

for i in {41001..41005}
do
    echo "apnsm PushTestDev4 ${CORRECT_DEVICE_TOKEN} ${DATE}-hello${i} ${i} chime 86400 {\"key\":${i}}" | ${NC}
done

for i in {31000..31005}
do
    echo "apnsm PushTestDev3 ${CORRECT_DEVICE_TOKEN} ${DATE}-hello${i} ${i} chime 86400 {\"key\":${i}}" | ${NC}
done

for i in {42001..42001}
do
	echo "apnsm PushTestDev4 ${ERROR_DEVICE_TOKEN} ${DATE}-hello${i} ${i} chime 86400 {\"key\":${i}}" | ${NC}
done

for i in {32001..32001}
do
	echo "apnsm PushTestDev3 ${ERROR_DEVICE_TOKEN} ${DATE}-hello${i} ${i} chime 86400 {\"key\":${i}}" | ${NC}
done

for i in {43001..43005}
do
	echo "apnsm PushTestDev4 ${CORRECT_DEVICE_TOKEN} ${DATE}-hello${i} ${i} chime 86400 {\"key\":${i}}" | ${NC}
done

for i in {33001..33005}
do
	echo "apnsm PushTestDev3 ${CORRECT_DEVICE_TOKEN} ${DATE}-hello${i} ${i} chime 86400 {\"key\":${i}}" | ${NC}
done
