;;; maven-pom-mode.el --- A major mode for pom files

;; Copyright (c) 2014 Matthew O. Smiht
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


(defgroup maven-pom-mode nil
  "Major mode for editting pom.xml files")


(defcustom maven-pom-mode-install-directory
  (file-name-as-directory (file-name-directory load-file-name))
  "The directory where malabar-mode was installed"
  :group 'maven-pom-mode
  :type 'directory)


(add-to-list 'rng-schema-locating-files (expand-file-name (format "%s/%s" maven-pom-mode-install-directory
								  "schemas.xml")))
