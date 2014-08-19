
config_files=~/usrv/config_files
www_files=~/usrv/www
rcm_dir=/usr/local/lib/roundcubemail/skins/classic
cal_data=~/radicale/data/user
LN='sudo ln -s -f'
host=$(hostname)
user=$(whoami)

# Change host name in config files
cd $config_files
git checkout .
sed -i "s/{HOSTNAME}/$host/g" $config_files/*

sudo rm -rf ~user-data/www/default
$LN $www_files ~user-data/www/default
$LN $config_files/usrv_logo.png $rcm_dir/images/roundcube_logo.png
$LN $config_files/rcm-login.html $rcm_dir/templates/login.html
$LN $config_files/tcm-taskbar.html $rcm_dir/includes/taskbar.html

$LN ~/caldavzap $www_files/calendar
$LN ~/carddavmate $www_files/contacts
$LN $config_files/cdz-config.js ~/caldavzap/config.js
$LN $config_files/cdm-config.js ~/carddavmate/config.js

$LN $config_files/nginx.local.conf /etc/nginx/conf.d/local.conf
sudo service nginx reload

# Create radicale data
mkdir -p $cal_data
touch $cal_data/contacts.vcf
cp $config_files/Calendar* $cal_data
sudo ln -s -f ~/radicale/radicale.sh /etc/init.d/radicale
sudo service radicale start

# Set up initial user accounts
mkdir ~/mail
sudo mv ~user-data/mail/users.sqlite ~/mail/users.sqlite
$LN ~/mail/users.sqlite ~user-data/mail/users.sqlite
sudo chown $user ~/mail/users.sqlite

scripts=~/usrv/scripts
cd $scripts
./email_config.py
