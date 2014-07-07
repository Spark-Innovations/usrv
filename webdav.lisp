
; http://localhost:1235/lisp/

;(load #P"~/devel/spark/server/rgllib/lisp/init.lisp")
(init)

(require :ergolib)
(require :dictionary)

(set-global *print-string-length* 50000)

(defun logstream () (HEMLOCK-EXT:TOP-LISTENER-OUTPUT-STREAM))

(defun logmsg (s &rest args)
  (bb logstream (logstream)
      (apply 'format logstream s args)
      (terpri logstream)
      (force-output logstream)))

(require :cl-webdav)

(rename-package :tbnl :tbnl '(:ht))

(setf ht::*ACCEPT-BOGUS-EOLS* t)

(setf (symbol-function 'dav::log-message) #'ht:log-message*) ; Bug patch

(setf dav:*FILE-RESOURCE-BASE-PATH-NAMESTRING* "/Users/ron/devel/calendars/data/")

;;; Proputils

(defv $resource-cache (->))

(defun ->resource (s)
  (if (stringp s)
    (progn
      (if (< (length s) (length dav:*FILE-RESOURCE-BASE-PATH-NAMESTRING*))
        (setf s (strcat (slice dav:*FILE-RESOURCE-BASE-PATH-NAMESTRING* 0 -1) s)))
      (ref $resource-cache s  (make-instance 'dav:file-resource :real-path s)))
    s))

(defun setprop (r name val &optional (xmlns "DAV:"))
  (setf r (->resource r))
  (CL-WEBDAV:SET-DEAD-PROPERTY
   r (dav::make-xmls-node :local-name name :namespace-uri xmlns
                          :children (mcond (null val) nil (atom val) (list val) val))))

(defun proplist (r)
  (dav:get-dead-properties (->resource r)))

(defun delprop (r name)
  (setf r (->resource r))
  (bb props (proplist r)
      prop (find name props :test 'string-equal :key 'caar)
      (if prop
        (progn (dav:remove-dead-property r prop) t)
        nil)))

(defun delall (r)
  (setf r (->resource r))
  (for prop in (dav:get-dead-properties r) do
    (dav:remove-dead-property r prop)))

;;;  Persistent properties

(in-package :dav)

(defun propfile (key) (cl-user::strcat key "_props"))

(defun retrieve-properties (key)
  "Retrieves the properties stored under the \(EQUAL) key KEY."
  (or (gethash key *property-hash*)
      (setf (gethash key *property-hash*)
            (ignore-errors
             (read-from-string (cl-user::file-contents (propfile key)))))))

(defun store-properties (key properties)
  "Stores PROPERTIES under the \(EQUAL) key KEY."
  (with-open-file (f (propfile key) :direction :output
                     :if-exists :supersede :if-does-not-exist :create)
    (pprint properties f))
  (setf (gethash key *property-hash*) properties))

(defun remove-properties (key)
  (if (probe-file (propfile key)) (delete-file (propfile key)))
  (remhash key *property-hash*))

(defun copy-properties (from-key to-key)
  (store-properties to-key (retrieve-properties from-key)))

(defun move-properties (from-key to-key)
  (copy-properties from-key to-key)
  (remove-properties from-key))

(in-package :cl-user)

;;; Make displayname overridable

(defun get-dprop (resource propname &optional (namespace "DAV:"))
  (if (stringp resource)
    (setf resource (make-instance 'dav:file-resource :real-path resource)))
  (3rd (find (list (cons propname namespace) nil)
             (dav:get-dead-properties resource) :test #'dav::property-equal)))

(advise dav::resource-display-name
  (aif (get-dprop (1st ccl::arglist) "displayname") (return it)))

(defvar *dav-property-function-displayname-hack* nil)

(advise dav::dav-property-function
  (if *dav-property-function-displayname-hack* (return nil)))

(advise dav::proppatch-handler
  (let ((*dav-property-function-displayname-hack* t)) (:do-it))
  :when :around)

(in-package :dav)
(defmethod initialize-instance :after ((resource file-resource) &rest initargs)
  (declare (ignore initargs))
  (cond ((slot-boundp resource 'real-path)
         (setf (resource-script-name resource)
               (subseq (namestring (real-path resource))
                       (1- (length *file-resource-base-path-namestring*)))))
        (t (setf (real-path resource)
                   (compute-real-path (resource-script-name resource))))))
(in-package :cl-user)

;;; Calendar resource type

(defv $caldav "urn:ietf:params:xml:ns:caldav")

(defun ends-with (s1 s2) (string-equal s1 s2 :start1 (max 0 (- (length s1) (length s2)))))

(defun resource-type (path)
  (setf path (namestring (truename path)))
  (mcond
   (or (ends-with path "/lisp") (ends-with path "/lisp/"))
   '(("resourcetype" . "DAV:") NIL (("collection" . "DAV:") NIL))
   (ends-with path "/") `(("resourcetype" . "DAV:") NIL
                          (("calendar" . ,$caldav) NIL)
                          (("collection" . "DAV:") NIL))
   (ends-with path "_props") '(("resourcetype" . "DAV:") NIL)
   `(("resourcetype" . "DAV:") NIL
     (("calendar" . ,$caldav) NIL))))

(define-method (dav::resource-type resource)
  (resource-type (namestring (dav::real-path resource))))

;;; Caldav dispatch

(defun caldav-dispatcher (request &optional (resource-class dav:*resource-class*))
  (bb method (dav::request-method request)
      sym (find-symbol (string-upcase (strcat method '-handler)) :dav)
      handler (or (and sym (fboundp sym) (symbol-function sym))
                  (case method
                    (:report 'report-handler)
                    (:mkcalendar 'mkcalendar-handler)
                    (otherwise 'dav::not-implemented)))
      (lambda ()
        (let ((dav:*resource-class* resource-class))
          (funcall handler)))))

(defun create-caldav-dispatcher (resource-class)
  (lambda (request)
    (caldav-dispatcher request resource-class)))

(push (create-caldav-dispatcher 'dav:file-resource) ht:*dispatch-table*)

(defv server (make-instance 'ht:easy-acceptor :port 1236))

(define-method (ht:ACCEPTOR-MESSAGE-LOG-DESTINATION (s (eql server)))
  (logstream))

(define-method (ht:ACCEPTOR-ACCESS-LOG-DESTINATION (s (eql server)))
  (logstream))

(defmethod dav:resource-content-type ((resource dav:file-resource))
  (logmsg "RCT: ~A ~A" (namestring (dav::real-path resource))
          (namestring (truename (dav::real-path resource))))
  (bb p (namestring (dav::real-path resource))
      (mcond (ends-with p "/") "httpd/unix-directory"
             (ends-with p "/lisp") "httpd/unix-directory"
             (ends-with p "_props") "text/plain"
             "text/calendar")))

(defun mkcalendar-handler ()
 (prog1
     (dav::mkcol-handler)
   (bb r (dav::get-resource)
       ; This triggers "... is not a location that supports this request"
             (setprop r "getctag" (strcat "C" (random (ash 1 40))) "http://calendarserver.org/ns/")
       ; Should be a no-op
       ;      (setprop r "getcontenttype" "text/calendar")
       ; Apparently no-ops, but Radicale does this so we will too
             (setprop r "current-user-privilege-set" '((("privilege" . "DAV:") NIL
                                                        (("all" . "DAV:") NIL))))
       ; Required, or we can't delete
       (setprop r "supported-calendar-component-set"
                `((("comp" . ,$caldav) (("name" "VEVENT"))))
                $caldav)
       )))

(pushnew :makcalendar dav::*allowed-methods*)

#|
;;; REPORT handler

REPORT /lisp/CF34F452-02BE-4461-B971-9A8713CCA179/ HTTP/1.1
Host: localhost:1235
Content-Type: text/xml
Depth: 1
Brief: t
Accept: */*
Connection: keep-alive
Prefer: return-minimal
User-Agent: Mac_OS_X/10.9.3 (13D65) CalendarAgent/176.2
Content-Length: 301
Accept-Language: en-us
Accept-Encoding: gzip, deflate

<?xml version="1.0" encoding="UTF-8"?>
<B:calendar-query xmlns:B="urn:ietf:params:xml:ns:caldav">
  <A:prop xmlns:A="DAV:">
    <A:getcontenttype/>
  </A:prop>
  <B:filter>
    <B:comp-filter name="VCALENDAR">
      <B:comp-filter name="VTODO"/>
    </B:comp-filter>
  </B:filter>
</B:calendar-query>

|#

(defun parse-report (octets)
  (bb node (parse-xml octets)
      (rrst (1st (rrst node)))))

(in-package :dav)

(defmethod resource-children ((resource file-resource))
  "The children of a \(collection) file resource are the contents
of the corresponding directory in the file system."
  (when (and (resource-collection-p resource)
             (resource-exists resource))
    (loop
      for real-path in (fad:list-directory (real-path resource))
      if (not (cl-user::ends-with (namestring real-path) "_props"))
      collect (make-instance 'file-resource :real-path real-path))))

(defun report-handler ()
  ; propfind handler
  (let* ((depth-header (header-in* :depth))
         (depth-value (cond ((or (null depth-header)
                                 (string-equal depth-header "infinity")) nil)
                            ((string= depth-header "0") 0)
                            ((string= depth-header "1") 1)
                            (t (warn "Depth header is ~S." depth-header)
                               (bad-request))))
         (initial-resource (get-resource)))
    (unless (resource-exists initial-resource)
      (not-found))
    (multiple-value-bind (properties propname)
                         (cl-user::parse-report (raw-post-data :force-binary t))
      (setf (content-type*) "text/xml; charset=utf-8"
            (return-code*) +http-multi-status+)
      (let ((result
             ;; loop through the resource and its descendants until
             ;; depth limit is reached
             (loop for depth = depth-value then (if depth (1- depth) nil)
               for resources = (list initial-resource)
               then (and (or (null depth) (not (minusp depth)))
                         (mapcan #'resource-children resources))
               while resources
               nconc (loop for resource in resources
                       collect (collect-properties resource
                                                   properties
                                                   (not propname))))))
        (serialize-xmls-node (apply #'dav-node "multistatus" result))))))
(in-package :cl-user)

(defun report-handler () (dav::report-handler))

(pushnew :report dav::*allowed-methods*)

(defun href (path) `((("href" . "DAV:") NIL ,path)))

(bb p "/Users/ron/devel/calendars/data/lisp/"
    
    (setprop p "calendar-home-set" (href "/lisp/") $caldav)
    (setprop p "calendar-user-address-set" (href "/lisp/") $caldav)
    (setprop p "current-user-principal" (href "/lisp/"))
    (setprop p "principal-collection-set" (href "/"))
    (setprop p "principal-URL" (href "/lisp/"))
    (setprop p "current-user-privilege-set"
             '((("privilege" . "DAV:") NIL
                (("all" . "DAV:") NIL))))
)

(ht:start server)
; (ht:stop server)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun delete-whitespace (l)
  (mcond (atom l) l
         (and (stringp (car l)) (every 'whitespacep (car l)))
         (delete-whitespace (cdr l))
         (cons (delete-whitespace (car l)) (delete-whitespace (cdr l)))))

(define-method (parse-xml (s string)) (parse-xml (string-to-bytes s)))

(define-method (parse-xml (v vector))
  (delete-whitespace (dav::parse-octets v (dav::make-xmls-builder))))

; Pretty-print server XML responses to make debugging easier

(defun serialize-xmls-node (xmls-node)
  (let ((handler (dav::make-octet-vector-sink)))
    (sax:start-document handler)
    (dav::walk-xmls-node handler xmls-node)
    (sax:end-document handler)))

(defun xml-render (l) (bytes-to-string (serialize-xmls-node l)))

(require :s-xml)

(define-method (pprint-xml (s string))
  (s-xml:print-xml (s-xml:parse-xml-string s) :pretty t)
  (values))

(define-method (pprint-xml (v vector)) (pprint-xml (bytes-to-string v)))

(defun dav:serialize-xmls-node (xmls-node) (serialize-xmls-node xmls-node))

(defun dav:serialize-xmls-node (xmls-node)
  (prog1
      (string-to-bytes ; NOTE: Returns two values!
       (strcat
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        (with-output-to-string (s)
          (s-xml:print-xml (s-xml:parse-xml-string (xml-render xmls-node))
                           :pretty t :stream s))))))
