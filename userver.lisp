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
         (for line in lines do
           (write-sequence line f)
           (terpri f)))
       (values)))

(require :cl-ppcre)
(defun grep (path regex)
  (bb s (cl-ppcre:create-scanner regex)
      (for line in (lines path) collect line if (cl-ppcre:scan s line))))

(defun remove-from-known-hosts (host &optional (ip (host-ip host)))
  (sed #P"~/.ssh/known_hosts" (not (or (starts-with line host)
                                       (starts-with line ip)))))

(defun kill-host (name)
  (bb ip (host-ip name)
      (unless ip (error "Host ~A not found" name))
      (remove-from-known-hosts name ip)
      id (host-instance-id name)
      (unless id (return (logmsg "No AWS instance found for host ~A" name)))
      (logmsg "Killing ~A" id)
      (kill-instance id)))

(defv $script-directory (namestring (merge-pathnames "scripts/" (this-directory))))

(defun make-host (host)
  (bb ip (host-ip host)
      (unless ip (error "No DNS entry for ~A" host))
      (if (host-instance-id host) (error "Host ~A exists" host))
      (remove-from-known-hosts host ip)
      (logmsg "Creating ~A" host)
      instance-spec (mkinstance)
      id (ref* (tree-search-for-tag instance-spec :instanceId) 0 0 1)
      (wait-for-instance-running id)
      (assign-elastic-ip ip id)
      (wait-for-ssh host)
      (ensure-bash)
      (bb :fn c (&rest args) (bash (join args #\space) t)
          (c "cd" $script-directory)
          (c "./ec2-ubuntu-setup.sh" host))))

(defun make-usrv-host (host)
  (make-host host)
  (setup-usrv-host host))

(defun setup-usrv-host (host)
  (ensure-bash)
  (bb :fn c (&rest args) (bash (join args #\space) t)
      (c "cd" $script-directory)
      (c "ssh" host "./usrv/scripts/usrv-setup.sh" t)))

(defun reset (&optional (host "h1.usrv.us"))
  (kill-host host)
  (bb uil (unassigned-instances)
      iid (ffst uil)
      (unless iid (error "No unassigned instances"))
      (unless (rst uil) (mkinstance :imageid "ami-5d5a246d")) ; Keep the pipeline full
      (wait-for-instance-running iid)
      (assign-elastic-ip (host-ip host) iid)
      (sleep 1)
      (wait-for-ssh host)))

#+NIL(

(reset)

(save-image (host-instance-id "h2.usrv.us") "usrv4" "Usrv host 4")

(defun my-ip-address ()
  (remove-if 'whitespacep (wget "http://curlmyip.com/")))

(my-ip-address)
)
