#!/bin/bash

set -e

if [ -z "$1" ]
then
echo 'Missing host name'
exit -1
fi

ec2user=ubuntu
host=$1
my_dir="$(dirname "$0")"
config_files=$my_dir/../config_files

# Move my key into ubuntu user authorized keys so we don't need -i any more
ssh -i ~/.ssh/amazonkey1 $ec2user@$host 'mkdir -p .ssh ; cat > .ssh/authorized_keys' < ~/.ssh/authorized_keys

ssh -t $ec2user@$host sudo apt-get -y update
ssh -t $ec2user@$host sudo groupadd -f sudo
ssh -t $ec2user@$host sudo usermod -a -G sudo,adm $ec2user

scp $config_files/sudoers.ubuntu $ec2user@$host:sudoers
ssh -t $ec2user@$host sudo chown root:root sudoers
ssh -t $ec2user@$host sudo chmod 440 sudoers
ssh -t $ec2user@$host sudo mv sudoers /etc/sudoers

user=$(whoami)

cat<<EOF|ssh -t $ec2user@$host
if
 id $user >& /dev/null
then
 echo User $user exists
else
 echo Setting up user $user
 sudo useradd -m $user
 sudo usermod -Gsudo,adm $user
 sudo mkdir -p ~$user/.ssh
 sudo cp .ssh/authorized_keys ~$user/.ssh
 sudo chown -R $user:$user ~$user/.ssh
fi
EOF

scp $config_files/bashrc $host:.bashrc
scp $config_files/dotemacs $host:.emacs
scp $config_files/ssh-config $host:.ssh/config

cat<<EOF|ssh -t $host 'cat>ipkg'
#!/bin/sh
sudo DEBIAN_FRONTEND=noninteractive apt-get -y install \$*
EOF

cat<<EOF|ssh -t $host
# The default login shell on ubuntu is dash
sudo chsh -s /bin/bash $user

sudo hostname $host
hostname | sudo tee /etc/hostname

sudo chmod a+x ipkg
sudo mv ipkg /usr/local/bin
sudo apt-get update
sudo apt-get -y upgrade
ipkg emacs git
EOF

# Warning! This opens a short-lived security hole, but this should
# be OK because we've just this server up so the hackers should not have
# compromised it yet.
#
# See https://lyte.id.au/2012/03/19/ssh-agent-forwarding-is-a-bug/
#
ssh $host rm -rf usrv  # Make it idempotent
ssh -A $host git clone git@github.com:Spark-Innovations/usrv.git
