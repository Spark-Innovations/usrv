#!/bin/bash

# Set up mail-in-a-box non-interactively

cd ~/mailinabox
. setup/functions.sh # load our functions
. /etc/mailinabox.conf

# Start service configuration.
. setup/system.sh
. setup/ssl.sh
. setup/dns.sh
. setup/mail-postfix.sh
. setup/mail-dovecot.sh
. setup/mail-users.sh
. setup/dkim.sh
. setup/spamassassin.sh
. setup/web.sh
. setup/webmail.sh
. setup/zpush.sh
. setup/management.sh

# Write the DNS and nginx configuration files.
sleep 5 # wait for the daemon to start
curl -s -d POSTDATA --user $(</var/lib/mailinabox/api.key): http://127.0.0.1:10222/dns/update
curl -s -d POSTDATA --user $(</var/lib/mailinabox/api.key): http://127.0.0.1:10222/web/update

# If there aren't any mail users yet, create one.
if [ -z "`tools/mail.py user`" ]; then
	tools/mail.py user add $EMAIL_ADDR $EMAIL_PW
fi
