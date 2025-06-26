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

;; This package integrates Retrieval-Augmented Generation (RAG) capabilities
;; with GPTel. It provides a backend for searching contextual documents using
;; an external rag-client binary and a prompt transformation function to
;; inject retrieved context into LLM prompts.
;;
;; Main components:
;; - Asynchronous rag-client integration using JSON output format
;; - Buffer-local document path customization
;; - Prompt augmentation with ranked document contexts
;;
;; To use: Set `gptel-rag-files-and-directories' in your chat buffer, then
;; call `gptel-rag-transform' as a prompt transformation function during GPTel
;; sessions.

;;; Code:

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

(cl-defun gptel-rag--search (query
                             &key
                             (config gptel-rag-config-file)
                             (top-k gptel-rag-top-k)
                             paths
                             callback)
  "Given a rag-client CONFIG anda QUERY, find similar documents.
The set of documents to be search is given by PATHS, with the maximum
number of such documents indicated by TOP-K.

This function is asynchronous, and will call CALLBACK with a list of
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
  (unless (file-readable-p config)
    (error "Invalid config path passed to `gptel-rag-search'"))
  (let ((proc
         (make-process
          :name "*rag-client*"
          :buffer "*rag-client-output*"
          :command
          (list (executable-find gptel-rag-client-exe)
                "--config" (expand-file-name config)
                "--top-k" (number-to-string top-k)
                "--from" "-"
                "search" query)
          :connection-type 'pipe
          :sentinel
          #'(lambda (proc _event)
              (unless (process-live-p proc)
                (with-current-buffer (process-buffer proc)
                  (goto-char (point-max))
                  (backward-sexp)
                  (funcall callback (seq--into-list (json-read)))))))))
    (process-send-string
     proc
     (with-temp-buffer
       (dolist (path (mapcar #'expand-file-name paths))
         (insert path ?\n))
       (buffer-string)))
    (process-send-eof proc)))

(defun gptel-rag-transform (callback fsm)
  "A prompt transformation function that augment the prompt using RAG.
The CALLBACK is called with the result when ready, and FSM gives the
finite state machine object."
  (let ((info-buf (plist-get (gptel-fsm-info fsm) :buffer)))
    (cl-macrolet ((chat-sym (sym)
                    `(with-current-buffer info-buf ,sym)))
      (let ((buffer (current-buffer)))
        (gptel-rag--search
         (buffer-string)
         :config (chat-sym gptel-rag-config-file)
         :top-k (chat-sym gptel-rag-top-k)
         :paths (chat-sym gptel-rag-files-and-directories)
         :callback
         #'(lambda (results)
             (with-current-buffer buffer
               (goto-char (point-max))
               (insert "\n\n")
               (dolist (result results)
                 (insert (format "With context from file '%s':\n\n%s\n\n"
                                 (alist-get 'file_name
                                            (alist-get 'metadata result))
                                 (alist-get 'text result)))))
             (funcall callback)))))))

(provide 'gptel-rag)

;;; gptel-rag.el ends here
