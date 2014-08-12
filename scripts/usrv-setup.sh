my_dir="$(dirname "$0")"
config_files=$my_dir/../config_files
scripts=$my_dir/../scripts

# Set up MIAB
rm -rf mailinabox
git clone git@github.com:Spark-Innovations/mailinabox.git

sudo $scripts/miab-config.sh
sudo $scripts/miab-prep.sh
sudo $scripts/miab-setup.sh

# Set up Radicale
git clone git@github.com:Spark-Innovations/radicale.git
sudo touch /var/log/radicale
sudo chown ron /var/log/radicale

# Set up dav clients
wget http://www.inf-it.com/CalDavZAP_0.10.0.5.zip
wget http://www.inf-it.com/CardDavMATE_0.11.1.zip
ipkg unzip
unzip Cal*
unzip Card*
mkdir -p pkg
mv *.zip pkg
