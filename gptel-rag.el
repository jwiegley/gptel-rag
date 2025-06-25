;;; gptel-rag --- Retrieval-Augmented Generation for GPTel -*- lexical-binding: t; -*-

;; Copyright (C) 2025 John Wiegley

;; Author: John Wiegley <johnw@gnu.org>
;; Created: 28 Apr 2025
;; Version: 0.1
;; Keywords: ai gptel tools
;; X-URL: https://github.com/jwiegley/dot-emacs

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; A "augment context" is context that is added to a query based on
;; information determined at the time of submission. This information can come
;; from anywhere, and must simply result in text to be appended to the either
;; the system or user context of the query.

(require 'cl-lib)
(require 'cl-macs)
(require 'seq)
(require 'gptel)

(defcustom gptel-rag-client-exe "rag-client"
  "Name or path of the rag-client executable."
  :type 'file)

(defcustom gptel-rag-config-file "config.yaml"
  "Config file used to configure rag-client for `gptel-rag'."
  :type 'file)

(defcustom gptel-rag-files-and-directories nil
  "List of files and directories to use for RAG searches.
This should be set buffer-local to have custom file sets per chat
buffer."
  :type '(repeat (choice file directory)))

(defcustom gptel-rag-top-k 3
  "Return the top K document nodes when querying a collection."
  :type 'integer)

(defun gptel-rag--search (config-path query-string callback-fn
                                      &rest files-or-directories)
  "Given a rag-client CONFIG-PATH anda QUERY-STRING, find similar documents.
The set of documents to be search is given by FILES-OR-DIRECTORIES.

This function is asynchronous, and will call CALLBACK-FN with a list of
results of the following type:

  [((text . STRING)
    (metadata ((file_path . PATH)
               (file_name . STRING)
               (file_type . STRING)
               (file_size . INT)
               (creation_date . DATE)
               (last_modified_date . DATE)))
   ...
  ]"
  (unless (file-readable-p config-path)
    (error "Invalid config path passed to `gptel-rag-search'"))
  (let ((proc
         (make-process
          :name "*rag-client*"
          :buffer "*rag-client-output*"
          :command
          (list (executable-find gptel-rag-client-exe)
                "--config" (expand-file-name config-path)
                "--top-k" (number-to-string gptel-rag-top-k)
                "--from" "-"
                "search" query-string)
          :connection-type 'pipe
          :sentinel
          #'(lambda (proc _event)
              (unless (process-live-p proc)
                (with-current-buffer (process-buffer proc)
                  (goto-char (point-max))
                  (backward-sexp)
                  (funcall callback-fn (seq--into-list (json-read)))))))))
    (process-send-string
     proc
     (with-temp-buffer
       (dolist (path
                (cl-mapcan
                 #'(lambda (path)
                     (if (file-directory-p path)
                         (cl-mapcar #'(lambda (file)
                                        (expand-file-name file path))
                                    (cl-set-difference
                                     (directory-files path)
                                     '("." "..")
                                     :test #'string=))
                       (list (expand-file-name path))))
                 files-or-directories))
         (insert path ?\n))
       (buffer-string)))
    (process-send-eof proc)))

(defun gptel-rag-transform (callback _info)
  "A prompt transformation function that augment the prompt using RAG.
The CALLBACK is called with the result when ready."
  (let ((buffer (current-buffer)))
    (apply
     #'gptel-rag--search
     gptel-rag-config-file
     (buffer-string)
     #'(lambda (results)
         (with-current-buffer buffer
           (insert "\n\n")
           (dolist (result results)
             (insert (format "With context from file '%s':\n\n%s\n\n"
                             (alist-get 'file_name
                                        (alist-get 'metadata result))
                             (alist-get 'text result)))))
         (funcall callback))
     gptel-rag-files-and-directories)))

(provide 'gptel-rag)

;;; gptel-rag.el ends here
