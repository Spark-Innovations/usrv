
if [ -z "$1" ]
then
echo 'No user name specified'
exit -1
fi

user = $1
echo Setting up user $user
pwd1 = $(pwgen -0 -A 12 1)
pwd2 = $(pwgen -s 20 1)
echo $user:$pwd1 > ~/usrv/www/tickets/$pwd2
echo $user:$pwd1 >> ~/radicale/htpasswd
cd ~/radicale/data
mkdir $user
cp $config_files/Calendar* $user/
sudo ./tools/mail.py user add user1@$(hostname) passwd1
