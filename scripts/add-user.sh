set -e

if [ -z "$1" ]
then
echo 'No user name specified'
exit -1
fi

user=$1
echo Setting up user $user
pwd=$(pwgen -0 -A 12 1)
ticket=$(pwgen -s 20 1)
echo $user:$pwd > ~/usrv/www/tickets/$ticket
echo $user:$pwd >> ~/radicale/htpasswd
mkdir -p ~/radicale/data
cd ~/radicale/data
mkdir $user
cp ~/usrv/config_files/Calendar* $user/
sudo ~/mailinabox/tools/mail.py user add ${user}@$(hostname) $pwd
echo Ticket for $user is $ticket
