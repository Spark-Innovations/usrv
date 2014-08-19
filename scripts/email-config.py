#!/usr/bin/python

import os
import sqlite3
import socket
import subprocess
import shutil

conn = sqlite3.connect(os.path.expanduser('~/mail/users.sqlite'))
cur = conn.cursor()
hostname = socket.gethostname()

def query(s, args=()):
  cur.execute(s, args)
  return cur.fetchall()

def db_reset():
  query('delete from users')
  query('delete from aliases')
  pass

def get_mail_users():
  return query('select * from users')

def ucmd(s):
  return subprocess.check_output(s.split())

def crypt512(pw):
  s = 'sudo /usr/bin/doveadm pw -s SHA512-CRYPT -p ' + pw
  return ucmd(s).strip()

def random_password(n):
  s = 'qwertyuiopasdfghjklzxcvbnm'
  cnt = 0
  for c in open('/dev/urandom').read(n): cnt = (cnt<<8) + ord(c)
  r=''
  for i in range(n):
    r += s[cnt % 26]
    cnt = cnt/26
    pass
  return r

def cd(path):
  os.chdir(os.path.expanduser(path))
  return os.getcwd()

def reset():
  query("delete from users")
  query("delete from aliases")
  cd('~/radicale')
  open('htpasswd','w').close()
  cd("~/radicale/data")
  for file in os.listdir('.'): shutil.rmtree(file)
  pass

def add_user(user):
  pw1 = random_password(12)
  query("insert into users(email,password) values(?,?)",
        (user + '@' + hostname, crypt512(pw1)))
  cd('~/radicale')
  with open('htpasswd','a') as f: f.write('%s:%s\n' % (user, pw1))
  cd('~/radicale/data')
  os.mkdir(user)
  cd(user)
  open('contacts.vcf','w').close()
  pw2 = random_password(20)
  cd('~/usrv/www/tickets')
  with open(pw2,'w') as f: f.write('%s:%s\n' % (user, pw1))
  return pw2
