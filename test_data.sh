#!/usr/bin/env bash
NC="nc $1 2222"
SID="$2"
DATE=`date +"%m%d-%H:%M:%S"`
CORRECT_DEVICE_TOKEN=130ab12bc1ef517bc574cb1199051a88057f6a9371028005f5e780cdb1588d49
ERROR_DEVICE_TOKEN=130ab12bc1ef517bc574cb1199051a88057f6a9371028005f5e780cdb1588d48

APNSCMD=""
for i in {41001..41010}
do
	APNSCMD=${APNSCMD}"apnsm PushTestDev4 ${CORRECT_DEVICE_TOKEN} ${DATE}-hello-${SID}-${i} ${i} chime 86400 {\"key\":${i}}\n"
done
echo -e ${APNSCMD} | ${NC}

APNSCMD=""
for i in {31001..31010}
do
    APNSCMD=${APNSCMD}"apnsm PushTestDev3 ${CORRECT_DEVICE_TOKEN} ${DATE}-hello-${SID}-${i} ${i} chime 86400 {\"key\":${i}}\n"
done
echo -e ${APNSCMD} | ${NC}

for i in {42001..42001}
do
	echo "apnsm PushTestDev4 ${ERROR_DEVICE_TOKEN} ${DATE}-hello-${SID}-${i} ${i} chime 86400 {\"key\":${i}}" | ${NC}
done

for i in {32001..32001}
do
	echo "apnsm PushTestDev3 ${ERROR_DEVICE_TOKEN} ${DATE}-hello-${SID}-${i} ${i} chime 86400 {\"key\":${i}}" | ${NC}
done

APNSCMD=""
for i in {43001..43010}
do
	APNSCMD=${APNSCMD}"apnsm PushTestDev4 ${CORRECT_DEVICE_TOKEN} ${DATE}-hello-${SID}-${i} ${i} chime 86400 {\"key\":${i}}\n"
done
echo -e ${APNSCMD} | ${NC}

APNSCMD=""
for i in {33001..33010}
do
	APNSCMD=${APNSCMD}"apnsm PushTestDev3 ${CORRECT_DEVICE_TOKEN} ${DATE}-hello-${SID}-${i} ${i} chime 86400 {\"key\":${i}}\n"
done
echo -e ${APNSCMD} | ${NC}
