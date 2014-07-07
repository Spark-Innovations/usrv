(init)

(require :aws)
(require :dns)
(require :html-utils)
(require :bashlink)

(defv $bash (make-bash-server))

(defun bash (cmd &optional verbose)
  (cmd $bash cmd verbose))

(bash "cd")
(defun bash-status () (ignore-errors (bash "hostname ; pwd")))
(defv $bash-home (bash "hostname ; pwd"))

(defun ensure-bash ()
  (bb s (bash-status)
      (cond ((null s)
             (close $bash)
             (setf $bash (make-bash-server))
             (ensure-bash))
            ((equal s $bash-home) s)
            ((equal (1st s) (1st $bash-home))
             (bash "cd")
             (ensure-bash))
            (t (bash "logout")
               (ensure-bash)))))

(defun host-ip (name)
  (ignore-errors (ipaddr-to-dotted (lookup-hostname name))))

(defun host-instance-id (name)
  (1st (find (host-ip name) (all-aws-instances) :key '4th :test 'equal)))

(defun kill-all-running-instances ()
  (for spec in (all-aws-instances) do
    (when (equal (3rd spec) "running")
      (logmsg "Killing instance ~A" spec)
      (kill-instance (1st spec)))))

(defun open-client-socket (host port)
  (make-socket :type :stream :remote-host host :remote-port port))

(defun kwd (thing)
  (if (keywordp thing)
    thing
    (intern (string-upcase (->string thing)) :keyword)))

(defun wait-for-instance-running (id)
  (block wait
    (bb :fn check (c) (bb status (kwd (instance-status id))
                          (case status
                            (:running (return-from wait t))
                            (:pending (princ c) (force-output))
                            (otherwise (error "Instance ~A state is ~A" id status))))
        (check "")
        (logmsg "Waiting for instance ~A to start" id)
        (loop (check #\.) (sleep 1)))))

(defun wait-for-ssh (hostname)
  (bb
   :fn ssh-check () (if (ignore-errors (close (open-client-socket hostname 22))) (return t))
   (logmsg "Waiting for ssh daemon on ~A" hostname)
   (loop (ssh-check) (princ #\.) (sleep 1))))

(defun all-usrv-hosts ()
  (bb l (live (list-dns-records "usrv.us"))
      (for entry in (rst (2nd l))
        collect (list (cdr (assoc :name entry))
                      (cdr (assoc :content entry))
                      (host-instance-id (cdr (assoc :name entry))))
        if (equal (cdr (assoc :type entry)) "A"))))

(defun unassigned-instances ()
  (bb usrv-hosts (all-usrv-hosts)
      (for spec in (all-aws-instances)
        collect spec if (and (equal (3rd spec) "running")
                             (not (member (4th spec) usrv-hosts :test 'equal :key '2nd))))))

(defun kill-unassigned-instances ()
  (for spec in (unassigned-instances) do
    (logmsg "Killing ~A" spec)
    (kill-instance (1st spec))))

(defmacro sed (path rule)
  `(bb lines (for line in (lines ,path) collect line if ,rule)
       (with-open-file (f ,path :direction :output :if-exists :supersede)
         (write-sequence (join lines #\newline) f))
       (values)))

(defun kill-host (name)
  (bb ip (host-ip name)
      (unless ip (error "Host ~A not found" name))
      id (host-instance-id name)
      (unless id (return (logmsg "No AWS instance found for host ~A" name)))
      (logmsg "Killing ~A" id)
      (kill-instance id)
      (sed #P"~/.ssh/known_hosts" (not (or (starts-with line name)
                                           (starts-with line ip))))))

(defun make-usrv-host (name)
  (bb host (strcat name ".usrv.us")
      ip (host-ip host)
      (unless ip (error "No DNS entry for ~A" host))
      (if (host-instance-id host) (error "Host ~A exists" host))
      (logmsg "Creating ~A.usrv.us" name)
      instance-spec (mkinstance)
      id (ref* (tree-search-for-tag instance-spec :instanceId) 0 0 1)
      (wait-for-instance-running id)
      (assign-elastic-ip ip id)
      (wait-for-ssh host)
      (logmsg "Setting up host")
      (setup-usrv-host host)))

(defun setup-usrv-host (host)
  (ensure-bash)
  (bb :fn c (&rest args) (bash (join args #\space) t)
      (c "cd ~/devel/cloudutils/scripts")
      (c "./setup.ec2.ubuntu" host)
      (c "cd ~/devel/cloudutils/lisp")
      (c "./setup.ccl" host)
      ))

(defun setup-miab (host)
  (ensure-bash)
  (bb :fn c (&rest args) (bash (join args #\space) t)
      (c "ssh" host "ipkg git")
      (c "ssh" host "rm -rf mailinabox")
      (c "ssh" host "git clone https://github.com/JoshData/mailinabox.git")
      (c "scp ~/Documents/Active/uServer/miab*" (fmt "~A:" host))
      (c "ssh" host)
      (c "chmod a+x ./miab*")
      (c "mv miab* mailinabox/")
      (c "cd mailinabox")
      (c "sudo ./miab-prep.sh")
      (c "logout")
))

(defun reset ()
  (kill-host "h2.usrv.us")
  (bb uil (unassigned-instances)
      iid (ffst uil)
      (unless iid (error "No unassigned instances"))
      (unless (rst uil) (mkinstance :imageid "ami-dd7709ed")) ; Keep the pipeline full
      (wait-for-instance-running iid)
      (assign-elastic-ip (host-ip "h2.usrv.us") iid)
      (sleep 1)
      (wait-for-ssh "h2.usrv.us")))

#+NIL(

(reset)
(ensure-bash)
(bash "scp ~/Documents/Active/uServer/miab-config h2.usrv.us:mailinabox/")

sudo hostname h2.usrv.us

cd mailinabox
sudo ./miab-config
mkdir externals
mv *.deb externals

host www.google.com
sudo apt-get -y upgrade
host www.google.com

sudo ./miab-setup.sh



(save-image (host-instance-id "h2.usrv.us") "usrv4" "Usrv host 4")

(defun my-ip-address ()
  (remove-if 'whitespacep (wget "http://curlmyip.com/")))

(my-ip-address)
)
