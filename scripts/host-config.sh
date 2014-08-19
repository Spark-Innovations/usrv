
config_files=~/usrv/config_files
scripts=~/usrv/scripts
www_files=~/usrv/www
rcm_dir=/usr/local/lib/roundcubemail/skins/classic
LN='sudo ln -s -f'
host=$(hostname)

# For add-user, should go in usrv-setup
ipkg pwgen

# Set up MIAB for this host
sudo $scripts/miab-config.sh

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

# Configure radicale
sudo ln -s -f ~/radicale/radicale.sh /etc/init.d/radicale
sudo service radicale start

# Add a user (for testing only at the moment)
sudo sqlite3 ~user-data/mail/users.sqlite 'delete from users'
sudo sqlite3 ~user-data/mail/users.sqlite 'delete from aliases'
$scripts/add-user ron
