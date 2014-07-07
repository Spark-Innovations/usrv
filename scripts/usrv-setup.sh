#!/bin/bash

set -e

if [ -z "$1" ]
then
echo 'Missing host name'
exit -1
fi

host=$1

scp ~/.ssh/config $host:.ssh/

cat<<EOF | ssh -t $host

rm -rf usrv
git clone git@github.com:Spark-Innovations/usrv.git

rm -rf mailinabox
git clone https://github.com/JoshData/mailinabox.git

cd usrv/scripts
sudo ./miab-config.sh
sudo ./miab-prep.sh
sudo ./miab-setup.sh

EOF
