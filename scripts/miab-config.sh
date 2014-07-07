
PRIMARY_HOSTNAME=`hostname`
PUBLIC_IP=`hostname --ip-address`
PUBLIC_IPV6=
CSR_COUNTRY=US
EMAIL_ADDR=user@$PRIMARY_HOSTNAME
EMAIL_PW=pass

STORAGE_USER=user-data
if [ ! -d /home/$STORAGE_USER ]; then useradd -m $STORAGE_USER; fi
STORAGE_ROOT=/home/$STORAGE_USER
mkdir -p $STORAGE_ROOT

# Save the global options in /etc/mailinabox.conf so that standalone
# tools know where to look for data.
cat > /etc/mailinabox.conf << EOF;
STORAGE_USER=$STORAGE_USER
STORAGE_ROOT=$STORAGE_ROOT
PRIMARY_HOSTNAME=$PRIMARY_HOSTNAME
PUBLIC_IP=$PUBLIC_IP
PUBLIC_IPV6=$PUBLIC_IPV6
CSR_COUNTRY=$CSR_COUNTRY
EMAIL_ADDR=$EMAIL_ADDR
EMAIL_PW=$EMAIL_PW
EOF
