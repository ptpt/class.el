;;; class.el --- an OOP system in emacs lisp

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

(require 'cl)

(defun oo--search-member (member instance &optional nested visited)
  (unless (memq instance visited)
    (let* ((instance (if (symbolp instance)
                         (symbol-value instance)
                       instance))
           (found (assoc member instance))
           (class (oo-class-of instance))
           (bases (cond ((eq class 'Type)
                         (oo-bases-of instance))
                        (class (list class))
                        (t nil))))
      (when (and found
                 (or (and visited (memq 'private (cadr found)))
                     (and (not nested) (memq 'protected (cadr found)))))
        (setq found nil))
      (while (and bases (not found))
        (setq found (oo--search-member
                     member (car bases)
                     nested (cons instance visited)))
        (setq bases (cdr bases)))
      found)))

(defun oo--read-forms (forms &optional types)
  (let* ((allowed '(public private protected classmethod staticmethod))
         (tail types))
    (while tail
      (cond ((memq (car tail) '(private protected public))
             (setcdr tail (remove-if
                           (lambda (type)
                             (memq type '(private protected public)))
                           (cdr tail))))
            ((memq (car tail) '(classmethod staticmethod))
             (setcdr tail (remove-if
                           (lambda (type)
                             (memq type '(classmethod staticmethod)))
                           (cdr tail))))
            (t (error "wrong type of member")))
      (setq tail (cdr tail)))
    (apply #'nconc
           (mapcar
            (lambda (form)
              (let ((type (car form)) (subforms (cdr form)))
                (cond
                 ((symbolp type)
                  (cond ((eq type 'defun)
                         (list (append (list (car subforms) types 'lambda)
                                       (cdr subforms))))
                        ((eq type 'setq)
                         (list (cons (car subforms)
                                     (cons types (eval (cadr subforms))))))
                        ((memq type allowed)
                         (oo--read-forms subforms (cons type types)))
                        (t (error "invalid syntax"))))
                 ((listp type)
                  (oo--read-forms subforms (append type types)))
                 (t (error "invalid syntax")))))
            forms))))

(defmacro oo-class-of (instance)
  `(cddr (assoc 'class ,instance)))

(defmacro oo-bases-of (instance)
  `(cddr (assoc 'bases ,instance)))

(defmacro oo-class-p (instance)
  `(eq (oo-class-of ,instance) 'Type))

(defmacro self. (property &rest args)
  `(@ self (quote ,property) ,@args))

(defmacro cls. (property &rest args)
  `(@ cls (quote ,property) ,@args))

(defmacro this. (property &rest args)
  `(@ this. (quote ,property) ,@args))

(defmacro class (name bases &rest forms)
  (let ((bases (cond ((listp bases) (remove-duplicates bases))
                     ((symbolp bases) (list bases))
                     (t (error "wrong type of bases"))))
        (members (remove-if (lambda (m) (memq (car m) '(class bases)))
                            (remove-duplicates
                             (oo--read-forms forms)
                             :test
                             (lambda (x y) (eq (car x) (car y)))))))
    (mapc (lambda (base)
            (unless (boundp base)
              (error (format "%s: class doesn't exist" base))))
          bases)
    `(progn
       (setq ,name (append
                    (quote ,members)
                    (list (cons 'class (cons nil 'Type))
                          (cons 'bases (cons nil (quote ,bases))))))
       (defun ,name (&rest args)
         (let ((instance '((class . (nil . ,name))))
               (init (cddr (oo--search-member 'init ,name))))
           (when init
             (apply init instance args))
           instance)))))

(defun @ (instance member &rest args)
  (when (null (oo-class-of instance))
    (error "invalid instance"))
  (let* ((nested (and (boundp 'this--instance)
                      (eq this--instance instance)))
         (this--instance instance)
         ;; a slot is (cons member (cons type body))
         (slot (oo--search-member member instance nested))
         (body (cddr slot)))
    (cond ((and (functionp body)
                (not (memq member '(class bases))))
           ;; call instance or class method
           (if (and (oo-class-p instance)
                    (or (not (memq 'classmethod (cadr slot)))
                        (memq 'staticmethod (cadr slot))))
               (apply body args)
             (apply body instance args)))
          ;; get and set instance property
          (slot (if args (setcdr (cdr slot) (car args)) body))
          ;; create and set instance property
          (args (setcdr (last instance)
                        (list (cons member (cons nil (car args))))))
          (t    (format "%s: no such member" member)))))

(class Object ()
       (defun init (self))
       (defun get-member (self member)
         (oo--search-member member self)))

(class Type (Object))

(provide 'class)
