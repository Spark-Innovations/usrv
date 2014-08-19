set -e

if [ -z "$1" ]
then
echo 'No user name specified'
exit -1
fi

cal_data=~/radicale/data
scripts=~/usrv/scripts
miab=~/mailinabox/tools/mail.py
host=$(hostname)
user=$1

echo Setting up user $user

mkdir -p $cal_data/$user
cp ~/usrv/config_files/Calendar* $cal_data/$user/
touch $cal_data/$user/contacts.vcf

passwd=$(pwgen -0 -A 12 1)
ticket=$(pwgen -s 20 1)
echo $user:$passwd > ~/usrv/www/tickets/$ticket
echo $user:$passwd >> ~/radicale/htpasswd
sudo $miab user add $user@$host $passwd
echo Ticket for $user is https://$host/public/login?$ticket=
