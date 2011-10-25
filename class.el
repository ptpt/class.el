;;; class.el --- an Object Oriented Programming extension for Emacs Lisp

;; Copyright (C) 2011 Tao Peng <ptpttt@gmail.com>

;; Author:   Tao Peng <ptpttt@gmail.com>
;; Keywords: internal, extension
;; URL:      https://github.com/ptpt/class.el

;; This file is NOT part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;;;###autoload
(defmacro class (name bases &rest forms)
  (let ((bases (cond ((listp bases) (remove-duplicates bases))
                     ((symbolp bases) (list bases))
                     (t (error "wrong type of bases"))))
        (members (mapcar
                  (lambda (form)
                    (cond ((eq (car form) 'defun)
                           (cons (cadr form)
                                 (cons 'lambda (cddr form))))
                          ((eq (car form) 'setq) nil)
                          (t nil)))
                  forms)))
    (setq members (delq nil members))
    (delete-duplicates members :test (lambda (x y) (eq (car x) (car y))))
    (delete-if (lambda (m) (memq (car m) '(class bases))) members)
    (mapc (lambda (base)
            (unless (boundp base)
              (error (format "%s: class doesn't exist" base))))
          bases)
    `(progn
       (setq ,name (append
                    (quote ,members)
                    (list (cons 'class 'Type)
                          (cons 'bases (quote ,bases)))))
       (defun ,name (&rest args)
         (let ((instance '((class . ,name)))
               (init (cdr (funcall
                           (cdr (assoc 'search-member Object))
                           'init ,name))))
           (when init
             (apply init instance args))
           instance)))))

;;;###autoload
(defun @ (instance member &rest args)
  (let* ((class (cdr (assoc 'class instance)))
         (cell (or (assoc member instance)
                   (funcall (cdr (assoc 'search-member Object))
                            member (symbol-value class))))
         (body (cdr cell)))
    (when (null class) (error "invalid instance"))
    (cond ((functionp body)
           ;; call instance or class method
           (if (assoc 'bases instance)
               (apply body args)
             (apply body instance args)))
          ;; get and set instance property
          (cell (if args (setcdr cell (car args)) body))
          ;; create and set instance property
          (args (setcdr (last instance) (list (cons member (car args)))))
          (t    (format "%s: no such member" member)))))

(class Object ()
       (defun search-member (member class &optional visited)
         (unless (memq class visited)
           (setq visited (cons class visited))
           (let* ((search-member (cdr (assoc 'search-member Object)))
                  (class (if (symbolp class)
                             (symbol-value class)
                           class))
                  (found (assoc member class))
                  (bases (cdr (assoc 'bases class))))
             (while (and bases (not found))
               (setq found (funcall search-member member (car bases) visited))
               (setq bases (cdr bases)))
             found))))

(class Type (Object))

(provide 'class)
