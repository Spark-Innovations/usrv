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

. /etc/mailinabox.conf

# Create a configuration file.
#
# For security, temp and log files are not stored in the default locations
# which are inside the roundcube sources directory. We put them instead
# in normal places.
cat<<EOF>foo
<?php
/*
 * Do not edit. Written by Mail-in-a-Box. Regenerated on updates.
 */
\$config = array();
\$config['log_dir'] = '/var/log/roundcubemail/';
\$config['temp_dir'] = '/tmp/roundcubemail/';
\$config['db_dsnw'] = 'sqlite:///$STORAGE_ROOT/mail/roundcube/roundcube.sqlite?mode=0640';
\$config['default_host'] = 'ssl://localhost';
\$config['default_port'] = 993;
\$config['imap_timeout'] = 15;
\$config['smtp_server'] = 'tls://localhost';
\$config['smtp_port'] = 587;
\$config['smtp_user'] = '%u';
\$config['smtp_pass'] = '%p';
\$config['support_url'] = 'http://usrv.us/';
\$config['product_name'] = 'μServer Webmail';
\$config['des_key'] = '$SECRET_KEY';
\$config['plugins'] = array('archive', 'zipdownload', 'password', 'managesieve');
\$config['skin'] = 'classic';
\$config['login_autocomplete'] = 2;
\$config['password_charset'] = 'UTF-8';
\$config['junk_mbox'] = 'Spam';
?>
EOF

sudo mv foo /usr/local/lib/roundcubemail/config/config.inc.php
