(in-package :sykosomatic)

(defgeneric body-souls (body))

(defgeneric body-location (body))
(defgeneric (setf body-location) (new-location body))

(defgeneric body-name (body))
(defgeneric (setf body-name) (new-name body))
(defgeneric body-description (body))
(defgeneric (setf body-description) (new-description body))

