;;; nssh.el --- New SSH mode

;; Copyright (C) 2014, 2015, 2017  Ian Eure

;; Author: Ian Eure <ian.eure@gmail.com>
;; Version: 0.9.11
;; Keywords: tools, unix, processes
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; nssh is a new SSH mode, based on TRAMP and shell-mode. It takes
;; some inspiration and code from `ssh-mode`, by Noah Friedman.

;; `M-x nssh RET` will open a connection. If a live connection is open,
;; it will be brought to the foreground. If the buffer exists, but the
;; process is dead, it will reconnect. With a prefix argument, opens a
;; new connection.

;;; Code:

(defvar nssh-history nil
  "nssh-mode history")

(defvar nssh-sudo t
  "Use sudo

  When T, `nssh' will use the sudo protocol in the case where the
  username of the `nssh' destination is different from
  USER-LOGIN-NAME.

  When NIL, `nssh' will use the ssh protocol")

(defvar nssh-known-hosts-files
  '("/etc/ssh/ssh_known_hosts"
    "~/.ssh/known_hosts"))

(defun nssh-replace (from to)
  "Replace all occurrences of FROM with TO, in the current buffer."
  (goto-char (point-min))
  (while (re-search-forward from nil t)
    (replace-match to nil nil)))

(defun nssh-process-hosts ()
  "Process a SSH known_hosts file, returning hosts therein."
  (save-match-data
    (nssh-replace " .*$" "")
    (nssh-replace "," "\n")
    ;; IPv4 addresses
    (flush-lines "[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+" (point-min) (point-max))
    ;; IPv4 addresses
    (flush-lines "[0-9a-f]\\{4\\}:+" (point-min) (point-max))
    (flush-lines "^$")
    (nssh-replace "^\\(.*\\)$" "\"\\1\"")
    (goto-char (point-min))
    (insert "(\n")
    (goto-char (point-max))
    (insert ")")
    (goto-char (point-min))
    (delete-dups (read (current-buffer)))))

(defun nssh-known-hosts ()
  "Return entries from NSSH-KNOWN-HOSTS-FILES for completion."
  (with-temp-buffer
    (mapc (lambda (file)
            (ignore-errors
              (insert-file-contents file)
              (insert "\n")))
          nssh-known-hosts-files)
    (nssh-process-hosts)))

(defun nssh-resolve (host)
  "Resolve HOSTNAME. Returns a list of IPs."
  (require 'url-gw)
  (if url-gateway-nslookup-program
      (let ((proc (start-process " *nslookup*" " *nslookup*"
                                 url-gateway-nslookup-program host))
            (res))
        (set-process-query-on-exit-flag proc nil)
        (with-current-buffer (process-buffer proc)
          (while (memq (process-status proc) '(run open))
            (accept-process-output proc))
          (goto-char (point-min))
          (while (re-search-forward "Name:.*\nAddress: *\\(.*\\)$" nil t)
            (add-to-list 'res (buffer-substring (match-beginning 1)
                                                (match-end 1))))
          (kill-buffer (current-buffer)))
        res)
    host))

(defun nssh-user-host (dest)
  "Extract user and host from DEST.

   DEST is a SSH-style 'user@host'
   Returns a list of USER HOST."
  (let* ((host-parts (split-string dest "@"))
         (host (car (last host-parts)))
         (user (or (if (= 2 (length host-parts)) (car host-parts))
                   (user-login-name))))
    (list user host)))

(defun nssh-buffer-name (user host)
  "Generate a nssh buffer name."
  (if (string= user (user-login-name))
      (format "*ssh %s*" host)
    (format "*ssh %s@%s*" user host)))

(defun nssh-buffer (user host buffer)
  "Return the target buffer for this nssh session."
  (get-buffer-create
   (cond ((stringp buffer) buffer)
         ((bufferp buffer) (buffer-name buffer))
         ((or (and (not (eq nil buffer)) (listp buffer)) (numberp buffer))
          (generate-new-buffer-name (nssh-buffer-name user host)))
         (t (nssh-buffer-name user host)))))

(defun nssh-protocol (user)
  (if (and (not (string= user user-login-name)) nssh-sudo)
      "sudo"
    "ssh"))

;;;###autoload
(defun nssh (dest &optional buffer pop-to)
  "Log into a remote machine with SSH."
  (interactive (list (completing-read "Host: "
                                      (append nssh-history (nssh-known-hosts))
                                      nil nil nil 'nssh-history)
                     current-prefix-arg))
  (add-to-list 'nssh-history dest)
  (let* ((user-host (nssh-user-host dest))
         (user (car user-host))
         (host (cadr user-host))
         (buf (nssh-buffer user host buffer)))
    (when pop-to
      (pop-to-buffer buf))
    (with-current-buffer buf
      (unless (comint-check-proc (current-buffer))
        (setq comint-prompt-read-only t)
        (cd (format "/%s:%s@%s:" (nssh-protocol user) user host))
        (shell (current-buffer))))))

(defun nssh-all-1 (dest)
  "Log into all hosts DEST resolves to. Returns new buffers."
  (let* ((user-host (nssh-user-host dest))
         (user (car user-host))
         (host (cadr user-host)))
    (mapcar
     (lambda (ip)
       (nssh ip (get-buffer-create
                 (nssh-buffer user (format "%s(%s)" host ip) nil))))

     (nssh-resolve host))))



(defvar nssh-controlled-buffers nil
  "List of comint buffers being controlled")
(make-variable-buffer-local 'nssh-controlled-buffers)

(defconst nssh-comint-prompt "nssh> ")

(defun nssh-comint-controller-insert (content)
  "Insert some text."
  (comint-output-filter (get-buffer-process (current-buffer)) content))

(defun nssh-comint-controller-insert-prompt ()
  "Insert the prompt.

   We have to use comint-output-filter, because (insert ...) is
   treated as user input."
  (nssh-comint-controller-insert nssh-comint-prompt))

(defun nssh-comint-controller-commandp (cmd)
  "Is this an internal command?"

  (and (> (length cmd) 0)
       (string= "," (substring cmd 0 1))))

(defun nssh-comint-controller-internal-command (proc input)
  (cond
   ((string= ",tile" input) (nssh-comint-controller-tile))
   ((string= ",quit" input) (nssh-comint-controller-quit))
   ((string= ",bufs" input) (nssh-comint-controller-buffers))))

(defun nssh-comint-controller-distribute (input)
  "Distribute `input' to controlled comints."
  (unwind-protect
      (mapc (lambda (cbuf)
              (with-current-buffer cbuf
                (insert input)
                (comint-send-input nil t)))
            nssh-controlled-buffers)))

(defun nssh-comint-controller-sender (proc input)
  "Handle nssh-comint-controller input"

  (if (nssh-comint-controller-commandp input)
      (nssh-comint-controller-internal-command proc input)
    (nssh-comint-controller-distribute input))

  (nssh-comint-controller-insert-prompt))

(defun nssh-comint-controller-quit ()
  (unwind-protect
      (mapc (lambda (cbuf)
              (quit-process (get-buffer-process cbuf)))
            (cons (current-buffer) nssh-controlled-buffers)))

(defun nssh-comint-controller-buffers ()
  (nssh-comint-controller-insert ";; Controlling:\n")
  (mapc (lambda (cbuf)
          (nssh-comint-controller-insert (format ";;  %s\n" (buffer-name cbuf))))
        comint-controlled-buffers))

(defun nssh-comint-controller-tile ()
  "Tile nssh-comint-controller windows."
  (interactive)

  (delete-other-windows)
  (let* ((controlwin (split-window-below -10))
         (lastwin (next-window controlwin)))
    (loop for rem on comint-controlled-buffers
          do
          (let ((buf (car rem))
                (more (cdr rem)))
            (message (format "Showing `%s' in `%s'" buf lastwin))
            (set-window-buffer lastwin buf)
            (when more (split-window-right))))

    (setq window-resizeable nil
          window-size-fixed t)
    (select-window controlwin))
  (balance-windows))

(define-derived-mode nssh-comint-controller-mode comint-mode
  "nssh-controller"
  "Major mode for controlling multiple comint buffers"
  (setq comint-prompt-regexp (concat "^" (regexp-quote comint-control-prompt)))
  (setq comint-use-prompt-regexp t)
  (setq comint-prompt-read-only t)
  (setq comint-input-sender 'nssh-comint-controller-sender)
  (unless (comint-check-proc (current-buffer))
    ;; Was cat, but on non-Unix platforms that might not exist, so
    ;; use hexl instead, which is part of the Emacs distribution.
    (condition-case nil
        (start-process "nssh-comint-controller" (current-buffer) "hexl")
      (file-error (start-process "nssh-comint-controller" (current-buffer) "cat")))
    (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)
    (goto-char (point-max))
    (nssh-comint-controller-insert ";; nssh cluster mode\n\n")))

;;;###autoload
(defun nssh-cluster (dest)
  (interactive (list (completing-read "Host: "
                                      (append nssh-history (nssh-known-hosts))
                                      nil nil nil 'nssh-history)))
  (let ((old-point)
        (bufname (format "*nssh-control %s*" dest)))

    (unless (comint-check-proc bufname)
      (let ((controlbuf (get-buffer-create bufname)))
        (with-current-buffer controlbuf
          (setq nssh-old-window-configuration (current-window-configuration))
          (nssh-comint-controller-mode)
          (unless (zerop (buffer-size)) (setq old-point (point)))
          (setq-local comint-controlled-buffers (nssh-all-1 dest))
          (nssh-comint-controller-buffers)
          (nssh-comint-controller-insert-prompt)
          (message (format "Current buffer is %s" (current-buffer)))
          (switch-to-buffer controlbuf)
          (nssh-comint-controller-tile))))
    (when old-point (push-mark old-point))))

(defun nssh-cluster-other-frame (dest)
  (interactive (list (completing-read "Host: "
                                      (append nssh-history (nssh-known-hosts))
                                      nil nil nil 'nssh-history)))
  (select-frame-set-input-focus (make-frame))
  (nssh-cluster dest))

(provide 'nssh)
;;; nssh.el ends here
