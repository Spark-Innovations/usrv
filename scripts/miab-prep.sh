my_dir="$(dirname "$0")"

source $my_dir/miab-config

sudo useradd nsd

ipkg nsd
ipkg ldnsutils
ipkg nginx
ipkg php5-cgi
ipkg python3-flask
ipkg links
ipkg duplicity
ipkg libyaml-dev
ipkg python3-dnspython
ipkg openssl
ipkg opendkim
ipkg opendkim-tools
ipkg python3
ipkg python3-pip
ipkg wget
ipkg curl
ipkg bind9-host
ipkg ntp
ipkg fail2ban
ipkg ufw
ipkg bind9
ipkg spampd
ipkg razor
ipkg pyzor
ipkg dovecot-sieve
ipkg dovecot-antispam
ipkg dbconfig-common
ipkg php5
ipkg php5-sqlite
ipkg php5-mcrypt
ipkg php5-intl
ipkg php5-json
ipkg php5-common
ipkg php-auth
ipkg php-net-smtp
ipkg php-net-socket
ipkg php-net-sieve
ipkg php-mail-mime
ipkg php-crypt-gpg
ipkg php5-gd
ipkg php5-pspell
ipkg tinymce
ipkg libjs-jquery
ipkg libjs-jquery-mousewheel
ipkg libmagic1
ipkg postfix
ipkg postgrey
ipkg postfix-pcre
ipkg dovecot-core
ipkg dovecot-imapd
ipkg dovecot-lmtpd
ipkg dovecot-sqlite
ipkg sqlite3

cd ~/mailinabox
mkdir externals
cd externals
apt-get download roundcube roundcube-core roundcube-sqlite3 roundcube-plugins
