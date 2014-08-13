if [ -z "$1" ]
then
echo 'Missing host name'
exit -1
fi

sudo hostname $1

config_files=~/usrv/config_files
www_files=~/usrv/www
rcm_dir=/usr/local/lib/roundcubemail/skins/classic
cal_data=~/radicale/data/user

# Change host name in config files
cd $config_files
git checkout .
sed -i "s/h1.usrv.us/$(hostname)/g" $config_files/*

sudo rm -rf ~user-data/www/default
sudo cp -r $www-files ~user-data/www/default

sudo cp $config_files/usrv_logo.png $rcm_dir/images/roundcube_logo.png

sudo cp $config_files/cloud.html ~user-data/www/default/
sudo ln -s -f ~/caldavzap ~user-data/www/default/calendar
sudo ln -s -f ~/carddavmate ~user-data/www/default/contacts
cp $config_files/cdm-config.js ~/carddavmate/config.js
cp $config_files/cdz-config.js ~/caldavzap/config.js

sudo cp $config_files/rcm-login.html $rcm_dir/templates/login.html

sudo cp $config_files/nginx.local.conf /etc/nginx/conf.d/local.conf
sudo service nginx reload

# Create radicale data
mkdir -p $cal_data
touch $cal_data/contacts.vcf
cp $config_files/Calendar* $cal_data
sudo ln -s -f ~/radicale/radicale.sh /etc/init.d/radicale
sudo service radicale start
