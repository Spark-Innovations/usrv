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

# Set up Radicale
git clone git@github.com:Spark-Innovations/radicale.git
wget http://www.inf-it.com/CalDavZAP_0.10.0.5.zip
wget http://www.inf-it.com/CardDavMATE_0.11.1.zip
ipkg unzip
unzip Cal*
unzip Card*
mkdir -p pkg
mv *.zip pkg

sudo cp $config_files/cloud.html ~user-data/www/default/
sudo ln -s ~/caldavzap ~user-data/www/default/calendar
sudo ln -s ~/carddavmate ~user-data/www/default/contacts
