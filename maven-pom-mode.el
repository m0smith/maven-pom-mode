;;; maven-pom-mode.el --- A major mode for pom files

;; Copyright (c) 2014 Matthew O. Smith
;; Author: 
;;     Matthew Smith
;; URL: http://www.github.com/m0smith/maven-pom-mode
;; Version: 0.0.1
;; Package-Requires: 
;; Keywords: java, maven, language, pom

;;; License:

;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA.
;;

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A major mode for pom files
;;

;;; Code:


(autoload 'json-read-from-string "json")

(defgroup maven-pom-mode nil
  "Major mode for editting pom.xml files")


(defcustom maven-pom-mode-install-directory
  (file-name-as-directory (file-name-directory load-file-name))
  "The directory where malabar-mode was installed"
  :group 'maven-pom-mode
  :type 'directory)

(eval-after-load 'nxml-mode
  '(progn
     (add-to-list 'rng-schema-locating-files 
		  (expand-file-name (format "%s/%s" 
					    maven-pom-mode-install-directory
					    "schemas.xml")))))

;;;; mvn-search.el --- do searches against search.maven.org
;;;; From https://github.com/upgradingdave/maven-mode/blob/master/mvn-search.el


(defvar maven-pom-search-maven-org-url "http://search.maven.org")
(defvar maven-pom-results-buffer "*maven-pom-results*")

;;;; Getters
;; The Search functions below convert json to vectors. Use these
;; getters to extract info from the vectors

(defun maven-pom-coords-from-alist (alist)
  "Given a alist representing a maven artifact, return coords"
  (cdr (assoc 'id alist)))

(defun maven-pom-artifactId-from-alist (alist)
  "Given a alist representing a maven artifact, return artifactId"
  (cdr (assoc 'a alist)))

(defun maven-pom-groupId-from-alist (alist)
  "Given a alist representing a maven artifact, return groupId"
  (cdr (assoc 'g alist)))

(defun maven-pom-latest-version-from-alist (alist)
  "Given a alist representing a maven artifact, return version"
  (cdr (assoc 'latestVersion alist)))

(defun maven-pom-version-from-alist (alist)
  "Given a alist representing a maven artifact, return version"
  (cdr (assoc 'v alist)))

;;;; Search functions

(defun maven-pom-search-get-json (url)
  "Make a request, get response, strip response headers, pass back json"
  (let* ((buffer 
          (url-retrieve-synchronously url))
         (json (concat "{" (with-current-buffer buffer
                             (goto-char (point-min))
                             (substring (buffer-string) (- (search-forward "{") 1))))))
    (kill-buffer buffer)
    json))

(defun maven-pom-search-by-term (search-term)
  "Do search against search.maven.org for SEARCH-TERM"
  (maven-pom-search-get-json 
   (url-generic-parse-url 
    (concat maven-pom-search-maven-org-url "/solrsearch/select?q=\"" 
            search-term "\"&rows=20&wt=json"))))

(defun maven-pom-search-for-versions (groupId artifactId)
  "Given GROUPID and ARTIFACTID, return list of all available versions"
  (maven-pom-search-get-json 
   (url-generic-parse-url
    (concat maven-pom-search-maven-org-url "/solrsearch/select?q=g:\"" groupId 
            "\"+AND+a:\"" artifactId "\"&core=gav&rows=20&wt=json"))))

(defun maven-pom-search-json-to-vector (json)
  "Convert json into a vector. Use getters to extract info from vector"
  (let ((json-object-type 'alist))
    (cdr (assoc 'docs (assoc 'response (json-read-from-string json))))))

(defun maven-pom-search-completing-groupIds (search-term)
  "Search for artifact by search term"
  (interactive "MSearch: ")
  (let ((groupIds 
         (mapcar (lambda (x) 
                   (concat 
                    (maven-pom-groupId-from-alist x) ":"
                    (maven-pom-artifactId-from-alist x)))
                 (maven-pom-search-json-to-vector (maven-pom-search-by-term search-term)))))
    (completing-read "Choose artifact:" groupIds nil t)))

(defun maven-pom-search-completing-versions (coord)
  "Search for all versions of coord <groupId:artifactId>"
  (interactive "MSearch: ")
  (let* ((artifactId (cadr (split-string coord ":")))
         (groupId (car (split-string coord ":")))
	 (vvv (maven-pom-search-for-versions groupId artifactId))
         (versions 
          (mapcar (lambda (x) 
                    (concat 
                     (maven-pom-groupId-from-alist x) ":"
                     (maven-pom-artifactId-from-alist x) ":"
                     (maven-pom-version-from-alist x) ))
                  (maven-pom-search-json-to-vector vvv))))
    (completing-read "Choose Version: " versions nil t coord)))

(defun maven-pom-insert-dependency-xml (coord)
  "Generate the dependency xml to insert into pom for coord <groupId:artifactId:version>"
  (interactive "MGroupId:ArtifactId:Version: ")
  (let ((groupId (car (split-string coord ":")))
        (artifactId (cadr (split-string coord ":")))
        (version (caddr (split-string coord ":")))
        (start (point)))
    (insert
     (message
      (concat "<dependency>\n" 
              "  <groupId>%s</groupId>\n"
              "  <artifactId>%s</artifactId>\n"
              "  <version>%s</version>\n"
              "</dependency>\n")
      groupId artifactId version))
    (indent-region start (point))))

(defun maven-pom-add-dependency (search-term)
  "Do search, then choose groupId, then choose version.  Search
for artifact by search term and return the GAV"
  (interactive "MSearch: ")
  (maven-pom-insert-dependency-xml (maven-pom-search-completing-versions 
                              (maven-pom-search-completing-groupIds search-term))))



