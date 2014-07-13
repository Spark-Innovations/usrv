my_dir="$(dirname "$0")"
config_files=$my_dir/../config_files
scripts=$my_dir/../scripts

rm -rf mailinabox
git clone git@github.com:Spark-Innovations/mailinabox.git

sudo $scripts/miab-config.sh
sudo $scripts/miab-prep.sh
sudo $scripts/miab-setup.sh

sudo cp $config_files/usrv-home.html ~user-data/www/default/index.html

sudo cp $config_files/usrv_logo.png \
 /usr/local/lib/roundcubemail/skins/classic/images/roundcube_logo.png
