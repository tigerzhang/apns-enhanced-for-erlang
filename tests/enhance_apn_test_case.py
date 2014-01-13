#!/home/push/python/bin/python
# -*- Encoding:GB2312 -*-

import string,urllib,sys
import struct
import socket
import StringIO
import sys
import struct
import StringIO
import socket

import MySQLdb
import json
import time

DB_USER='push_root'
DB_PASSWD= 'KKpush987'
DB_HOST= '192.168.252.27'
DB_NAME='kkpush'

ECMD_VERSION = 2

SVR_IP = "127.0.0.1"
SVR_PORT = 2222

CON_LEN =  10
SVRADD = (SVR_IP, SVR_PORT)

DBHEADRES="00000000000000000000000000"
toRes="aaaaa"
fromRes="i    "

    #C->S:
#    sContent = dwUin+dwType+wLen+K(dwUin+strmd5(passwd)+ dwStatus + strReserved)
#S->C:
#    sContent = cResult + stOther

def getappkeybyuid(uid):
    conn = MySQLdb.connect(host=DB_HOST, user=DB_USER, passwd=DB_PASSWD, db=DB_NAME, charset="utf8")
    cursor = conn.cursor()
    sql = "select appkey from t_dev_app_uid where uid = %d" % (int(uid))
    #print sql

    count = cursor.execute(sql)
    if count <= 0:
        cursor.close()
        conn.close()
        return ""
    results = cursor.fetchall()
    for result in results:
        cursor.close()
        conn.close()
        return result[0]

def usage() :
    print sys.argv[0] + " flatform(1-dev)\n"

if __name__ == "__main__":
    argc = len(sys.argv)
                #uid,
    # infoDev = [(94875430,getappkeybyuid(94875430)), (99872315,getappkeybyuid(99872315))]
    # infoDis = [(99835406,getappkeybyuid(99835406)), (99787510,getappkeybyuid(99787510))]

    infoDev = [(94875430, "00112233445566778899"), (99872315, "11112233445566778899")]
    infoDis = [(99835406, "22112233445566778899"), (99787510, "33112233445566778899")]

    if argc==2:
        if int(sys.argv[1]) == 1:
            toUid = infoDev[0][0]
            errToUid = infoDis[0][0]
            appKey = infoDev[0][1]

            toUid2 = infoDev[1][0]
            errToUid2 = infoDis[1][0]
            appKey2 = infoDev[1][1]

            platform = 1 # distribution
        else:
            toUid = infoDis[0][0]
            errToUid = infoDev[0][0]
            appKey = infoDis[0][1]

            toUid2 = infoDis[1][0]
            errToUid2 = infoDev[1][0]
            appKey2 = infoDis[1][1]

            platform = 10001 # development
    else:
        usage()
        sys.exit(-1)

    print toUid, appKey

    #send udp pkg
    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    #to get host socket addr to get return status
    s.connect(SVRADD)
    RETURN_ADDR = s.getsockname()
    RETURN_N_IP = struct.unpack('!I',socket.inet_aton(RETURN_ADDR[0]))[0]
    RETURN_N_PORT = RETURN_ADDR[1]
    socket.timeout(1)

    #test case 1
    testcnt = 1
    print "test case 1: send succ for %d times" % (testcnt,)
    i = 1
    while i<=testcnt:
        #make up udp pkg
        msg = '{"aps":{"sound":"default","badge":%d, "alert":"case 1 : %d"}}' % (i,i)
        fmt = '!H%dsIIH24s' % len(msg)
        badge = 1
        withsound = 1
        PKG_BODY = struct.pack(fmt, len(msg), msg, badge, withsound, 24 , str(appKey) )

        ALL_len = CON_LEN + len(PKG_BODY)
        PKG_HEAD = struct.pack("!HBBHI", ALL_len , ECMD_VERSION, 0, 1234, 0)
        PKG_DB_HEAD = struct.pack("!IHIHI5sI5sIIIH20s",0,0,0,0, toUid, fromRes, toUid,toRes,0, platform, RETURN_N_IP, RETURN_N_PORT, DBHEADRES)
        PKG=PKG_HEAD+PKG_DB_HEAD+PKG_BODY

        #time.sleep(0.1)
        # s.sendto(PKG, SVRADD)
        s.send(PKG)
        time.sleep(0.1)
        i+=1

    s.close()
    exit(0)

    #test case 2 -- succ*1 - fail*2 - succ*2 - fail*1 - succ*1
    testSeq = [     (1,1)   ,(0,2)   ,(1,2)   ,(0,1)   ,(1,1)]
    print "test case 2: succ,fail in same apn ssl mixed : %s" % (repr(testSeq),)
    for pair in testSeq:
        testcnt = pair[1]
        i = 1
        if 1==pair[0]:
            pkgUid = toUid
        else:
            pkgUid = errToUid
        while i<= testcnt:
            #make up udp pkg
            msg = '{"aps":{"sound":"default","badge":%d, "alert":"case 2 : %s"}}' % (i,repr(pair))
            fmt = '!H%dsIIH24s' % len(msg)
            badge = 1
            withsound = 1
            PKG_BODY = struct.pack(fmt, len(msg), msg, badge, withsound, 24 , str(appKey) )

            ALL_len = CON_LEN + len(PKG_BODY)
            PKG_HEAD = struct.pack("!HBBHI", ALL_len , ECMD_VERSION, 0, 1234, 0)
            PKG_DB_HEAD = struct.pack("!IHIHI5sI5sIIIH20s",0,0,0,0, pkgUid, fromRes, pkgUid, toRes,0, platform, RETURN_N_IP, RETURN_N_PORT, DBHEADRES)
            PKG=PKG_HEAD+PKG_DB_HEAD+PKG_BODY

            s.send(PKG)
            time.sleep(0.1)
            i+=1

    #test case 3 -- succ1*1 - fail1*2 - succ2*2 - succ1*1 - fail2*1 - succ1*1
    testSeq = [     (1,1,1)  ,(0,1,2)  ,(1,2,2)  ,(1,1,1)  ,(0,2,1)  ,(1,1,1)]
    print "test case 3: succ,fail in same apn ssl mixed : %s" % (repr(testSeq),)
    for pair in testSeq:
        testcnt = pair[2]
        i = 1
        if 1==pair[1]:
            pkgAppKey = appKey
        else:
            pkgAppKey = appKey2

        if 1==pair[0]:
            if 1==pair[1]:
                pkgUid = toUid
            else:
                pkgUid = toUid2
        else:
            if 1==pair[1]:
                pkgUid = errToUid
            else:
                pkgUid = errToUid
        while i<= testcnt:
            #make up udp pkg
            msg = '{"aps":{"sound":"default","badge":%d, "alert":"case 3 : %s"}}' % (i,repr(pair))
            fmt = '!H%dsIIH24s' % len(msg)
            badge = 1
            withsound = 1
            PKG_BODY = struct.pack(fmt, len(msg), msg, badge, withsound, 24 , str(pkgAppKey) )

            ALL_len = CON_LEN + len(PKG_BODY)
            PKG_HEAD = struct.pack("!HBBHI", ALL_len , ECMD_VERSION, 0, 1234, 0)
            PKG_DB_HEAD = struct.pack("!IHIHI5sI5sIIIH20s",0,0,0,0, pkgUid, fromRes, pkgUid, toRes,0, platform, RETURN_N_IP, RETURN_N_PORT, DBHEADRES)
            PKG=PKG_HEAD+PKG_DB_HEAD+PKG_BODY

            s.send(PKG)
            time.sleep(0.1)
            i+=1


    s.close()

