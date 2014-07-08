my_dir="$(dirname "$0")"
config_files=$my_dir/../config_files
scripts=$my_dir/../scripts

rm -rf mailinabox
git clone https://github.com/JoshData/mailinabox.git

sudo $scripts/miab-config.sh
sudo $scripts/miab-prep.sh
sudo $scripts/miab-setup.sh

sudo cp $config_files/usrv_logo.png \
 /usr/share/roundcube/skins/larry/images/roundcube_logo.png

sudo cp $config_files/usrv-home.html ~user-data/www/default/index.html
