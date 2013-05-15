apns-enhanced-for-erlang
========================
Apple Push Notification Server with Enhanced Binary Interface for Erlang by JPush (http://jpush.cn). If you get any questions, you can send me an email to <iamzhanghu AT gmail.com>.

HOWTO
=====
1. Put the certificate at priv/cert.pem
2. make run
3. Send test messages by ./test_data.sh localhost 2222. Make sure to change the
CORRECT_DEVICE_TOKEN to your own device token.

Open Source
===========
For developing a quick prototype, some codes are copied from following projects.
https://github.com/inaka/apns4erl.git
https://code.google.com/p/erl-generic-otp-tcp-server-framework/
http://hg.rabbitmq.com/rabbitmq-server/
