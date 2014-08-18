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

(defun reset-bash ()
  (close $bash)
  (ensure-bash))

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
   (ssh-check)
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
        collect spec if (and (member (3rd spec) '("pending" "running") :test 'equal)
                             (not (member (4th spec) usrv-hosts :test 'equal :key '2nd))))))

(defun kill-unassigned-instances ()
  (for spec in (unassigned-instances) do
    (logmsg "Killing ~A" spec)
    (kill-instance (1st spec))))

(defun remove-from-known-hosts (host &optional (ip (host-ip host)))
  (bash (fmt "ssh-keygen -R ~A" host) t)
  (bash (fmt "ssh-keygen -R ~A" ip) t))

(defun kill-host (name)
  (bb ip (host-ip name)
      (unless ip (error "Host ~A not found" name))
      (remove-from-known-hosts name ip)
      id (host-instance-id name)
      (unless id (return (logmsg "No AWS instance found for host ~A" name)))
      (logmsg "Killing ~A" id)
      (kill-instance id)))

(defv $script-directory (namestring (merge-pathnames "scripts/" (this-directory))))

(defun aws-host-name (instance-id)
  (ref* (xml-find (aws-describe-instances "instance-id" instance-id) '(dnsname)) 0 1))

(defun make-aws-instance (&optional image-id)
  (ref* (tree-search-for-tag (mkinstance :image-id image-id) :instanceId) 0 0 1))

(defun make-basic-host (&optional (id (make-aws-instance)))
  (wait-for-instance-running id)
  (bb host (aws-host-name id)
      (wait-for-ssh host)
      (ensure-bash)
      (bb :fn c (&rest args) (bash (join args #\space) t)
          (c "cd" $script-directory)
          (c "./ec2-ubuntu-setup.sh" host))
      (values host id)))

(defun setup-usrv-host (host)
  (ensure-bash)
  (bb :fn c (&rest args) (bash (join args #\space) t)
      (c "cd" $script-directory)
      (c "ssh" host "./usrv/scripts/usrv-setup.sh" t)
      (c "ssh" host "./usrv/scripts/ccl-setup.sh" t)))

(defun make-usrv-host-base-image (image-name)
  (bb :mv (host id) (make-basic-host)
      (setup-usrv-host host)
      (save-image id image-name (strcat "USRV base image " (format-date-time (now))))
      (find-image image-name)))

;;; Should go in library
(defun ->keyword (thing) (intern (->string thing) :keyword))

(defun fmt-substitute (string dictionary &optional (open-sub-char #\[) (close-sub-char #\]))
  (bb :db (first . rest) (for s in (split string open-sub-char) collect s)
      _ (for s in rest collect
          (bb :db (name rest) (split s close-sub-char :max 1)
              (strcat (ref dictionary (->keyword (string-upcase name))) rest)))
      (apply 'strcat first _)))

;;; Certificates

(defv $certificate-directory (merge-pathnames "certs/" (this-directory)))

(defun ssl-keyfile (host)
  (probe-file (merge-pathnames (fmt "~A.key" host) (truename $certificate-directory))))

(defun ssl-certfile (host)
  (probe-file (merge-pathnames (fmt "~A.crt" host) (truename $certificate-directory))))

(defun generate-ssl-key (host &key (bits 2048) (force nil))
  (bb keyfile (ssl-keyfile host)
      (if keyfile
        (if force
          (rename-file keyfile (strcat keyfile ".old") :if-exists :supersede)
          (return (logmsg "Key exists.  Specify :force t to replace it"))))
      (bash (fmt "openssl genrsa -out ~A.key ~A" host bits) t)
      (bash (fmt "chmod 400 ~A" (ssl-keyfile host)) t)
      (ssl-keyfile host)))

(defun csr-subject-line (common-name &key (country "US") (state "CA") (locale "")
                                     (organization "Spark Innovations") (group ""))
  (strcat "/C=" country "/ST=" state "/L=" locale "/O=" organization "/OU=" group
          "/CN=" common-name))

(defun generate-certificate-authority (name &optional (days 3650))
  (bash (fmt "openssl req -new -x509 -key ~A.key -out ~A.crt -subj ~S -days ~A"
             name name (csr-subject-line name) days) t))

(defun generate-csr (name)
  (bash (fmt "openssl req -new -key ~A.key -out ~A.csr -subj ~S"
             name name (csr-subject-line name))))

(defun generate-certificate (name ca &optional (days 3650))
  (bash (fmt "openssl x509 -req -days ~A -in ~A.csr -CA ~A.crt -CAkey ~A.key -CAcreateserial -out ~A.crt"
             days name ca ca name) t))

(defun gencert (host &key (ca "usrv-root") (days 3650) (renew nil))
  (bash (fmt "cd ~A" $certificate-directory))
  (unless (ssl-keyfile ca)
    (logmsg "Generating CA key")
    (generate-ssl-key ca))
  (unless (ssl-certfile ca)
    (logmsg "Generating CA certificate")
    (generate-certificate-authority ca))
  (if (ssl-keyfile host)
    (if renew
      (progn
        (logmsg "Renewing certificate for ~A" host)
        (generate-csr host)
        (generate-certificate host ca days))
      (logmsg "Certificate exists.  Specify :renew t to renew it."))
    (progn
      (logmsg "Generating new key for ~A" host)
      (generate-ssl-key host)
      (generate-csr host)
      (generate-certificate host ca days)))
  (values (ssl-certfile host) (ssl-keyfile host)))

(defun setup-ssl-keys (host)
  (bb :mv (certfile keyfile) (gencert host)
      (bash (fmt "scp ~A ~A ~A:" certfile keyfile host) t)
      (bash (fmt "ssh ~A sudo mv ~A.crt /home/user-data/ssl/ssl_certificate.pem" host host) t)
      (bash (fmt "ssh ~A sudo mv ~A.key /home/user-data/ssl/ssl_private_key.pem" host host) t)
      t))

(defun wait-for-instance-available (instance-id)
  (logmsg "Checking instance ~A for availability" instance-id)
  (wait-for-instance-running instance-id)
  (wait-for-ssh (instance-ip instance-id)))

(defun allocate-host (host &optional (image-name "usrv2"))
  (bb image-id (find-image image-name)
      (if (null image-id) (error "Unknown image: ~A" image-name))
      ip (host-ip host)
      (if (null ip) (error "Unknown host: ~A" host))
      (if (host-instance-id host)
        (error "Host exists.  Use KILL-HOST to kill it first."))
      (when (null (unassigned-instances))
        (mkinstance :image-id image-id)
        (mkinstance :image-id image-id))
      uil (unassigned-instances)
      iid (ffst uil)
      (unless (rst uil) (mkinstance :image-id image-id)) ; Keep the pipeline full
      (wait-for-instance-available iid)
      (logmsg "Host is running, assigning IP address")
      (assign-elastic-ip ip iid)
      iid))

(defun init-host (host)
  (logmsg "Setting up host")
  (bash (fmt "ssh ~A sudo hostname ~A" host host) t) ; Required to make sudo happy
  (setup-ssl-keys host)
  (bash (fmt "ssh ~A 'cd usrv;git pull'" host) t)
  (bash (fmt "ssh ~A 'cd radicale; git pull'" host) t)
  (bash (fmt "ssh ~A 'cd ergolib; git pull'" host) t)
  (bash (fmt "ssh ~A ./usrv/scripts/host-config.sh" host) t))

(defun reset-host (host)
  (reset-bash)
  (kill-host host)
  (allocate-host host)
  (init-host host))


#+NIL(

(all-usrv-hosts)
(list-my-images)
(find-image "usrv1")

(kill-host "h2.usrv.us")
(mkhost1 "h2.usrv.us")
(init-host "h2.usrv.us")

(require :email)
(send-email "user@h2.usrv.us" "Test" "Test 123")

(defun my-ip-address ()
  (remove-if 'whitespacep (wget "http://curlmyip.com/")))

(my-ip-address)

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

)
