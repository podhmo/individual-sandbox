(require 'eieio)

(defclass record () ; No superclasses
  ((name :initarg :name
         :initform ""
         :type string
         :custom string
         :documentation "The name of a person.")
   (birthday :initarg :birthday
             :initform "Jan 1, 1970"
             :custom string
             :type string
             :documentation "The person's birthday.")
   (phone :initarg :phone
          :initform ""
          :documentation "Phone number."))
  "A single record for tracking people I know.")

(cl-defmethod call-record ((rec record) &optional scriptname)
  "Dial the phone for the record REC.
     Execute the program SCRIPTNAME to dial the phone."
  (message "Dialing the phone for %s"  (oref rec name))
  (shell-command (concat (or scriptname "dialphone.sh")
                         " "
                         (oref rec phone))))

(setq rec (record :name "Eric" :birthday "June" :phone "555-5555"))

(call-record rec)
(call-record rec "my-call-script")
