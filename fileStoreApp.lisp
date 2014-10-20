(require :webutils)

(defun url (s) (puri:parse-uri s))

(ensure-http-server 1234)

(defv $storage-root "~/data/")

(defun unauth ()
  (set-return-code ht::+http-forbidden+)
  (set-header :content-type "text/html; charset=utf-8")
  "<h1>Unauthorized</h1>")

(defun user-info ()
  (bb ticket (ht:cookie-in "USRV_TICKET")
      (unless ticket (return nil))
      info (file-contents (strcat "/home/ron/usrv/www/tickets/" ticket))
      :db (user pwd) (split (CHUNGA:TRIM-WHITESPACE info) #\:)
      (values user pwd ticket)))

(defun require-login ()
  (or (user-info) (throw 'page-handler-done (unauth))))

(defurl "/file-upload" :text
  (bb fi (once-only (multifileinput "dnd_file" :binary))
      user (require-login)
      (for (name content type) in fi do
        (logmsg "Upload: ~A ~A ~A bytes" name type (length content))
	(unless (zerop (length name))
	  (with-open-file (f (strcat $storage-root user "/" name)
			     :direction :output
			     :element-type 'u8 :if-exists :supersede)
			  (write-sequence content f))))
      ; Return value should be "OK" or an absolute path or an error message
      (ref (puri:parse-uri (formval "referrer")) 'puri::path)))

(defpage "/" (forward #U"/dnd"))

(defpage "/dnd"
  "Drag and drop..." :br
  (bb user (require-login)
      dir (strcat $storage-root user "/")
      (check-path dir :create)
      files (directory (strcat dir "*.*"))
      (if files
	  (for file in files do
	       (bb file (file-namestring file)
                   encfile (HUNCHENTOOT:URL-ENCODE file)
		   (who
		    ((:a :href (url (strcat "/delete?file=" encfile))) "(x)")
		    " - "
		    ((:a :href (url (strcat "/download?file=" encfile)))
		     (str file)) :br)))
	(str "No files")))
  (:script :src "/jquery-2.1.1.min.js")
  (dnd #U"/file-upload"))

(defpage "/delete"
  (bb user (require-login)
      path (probe-file (strcat $storage-root user "/" (formval "file")))
      (if path (delete-file path))
      (forward #U"/dnd")))

(defurl "/download"
  (bb user (require-login)
      path (probe-file (strcat $storage-root user "/" (formval "file")))
      (unless path (return (not-found nil)))
      bytes (file-contents path :binary)
      (set-header :content-length (length bytes))
      (set-header :content-type (ht:mime-type path))
      (set-header :content-disposition (fmt "attachment; filename=~S" (formval "file")))
      (write-sequence bytes (ht:send-headers))
      ""))
