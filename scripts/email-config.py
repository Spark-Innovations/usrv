#!/usr/bin/python3

import os
import socket
import sys
sys.path.append(os.path.expanduser('~/mailinabox/management/'))

from mailconfig import *

env={'STORAGE_ROOT' : os.path.expanduser("~"),
     'PRIMARY_HOSTNAME' : socket.gethostname()}

for user in get_mail_users(env): remove_mail_user(user, env)
