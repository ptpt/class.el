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

(defun search-member (member class &optional visited)
  (unless (memq class visited)
    (setq visited (cons class visited))
    (let ((found (assoc member class))
          (super (cdr (assoc 'super class))))
      (while (and super (not found))
        (setq found (search-member member (car super) visited))
        (setq super (cdr super)))
      found)))

;;;###autoload
(defmacro class (name super &rest forms)
  (set name
       (cons (cons 'super (if (listp super) super (list super)))
             (mapcar (lambda (form)
                       (if (and (eq (car form) 'defun)
                                (not (eq (cadr form) 'super)))
                           (cons (cadr form)
                                 (cons 'lambda (cddr form)))))
                     forms)))
  `(defun ,name (&rest args)
     (let ((instance '((class . ,name)))
           (init (cdr (search-member 'init ,name))))
       (when init
         (apply init instance args))
       instance)))

;;;###autoload
(defun @ (instance member &rest args)
  (let* ((class (cdr (assoc 'class instance)))
         (cell (or (assoc member instance)
                   (search-member member (symbol-value class))))
         (body (cdr cell)))
    (when (null class)
      (error "invalid instance"))
    (cond ((functionp body) (apply body instance args))
          (cell (if args (setcdr cell (car args)) body))
          (args (setcdr (last instance) (list (cons member (car args)))))
          (t (format "no such member %s" member)))))

(provide 'class)
