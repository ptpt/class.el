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

(defun class--search-member (member instance &optional nested visited)
  (unless (memq instance visited)
    (let* ((instance (if (symbolp instance)
                         (symbol-value instance)
                       instance))
           (found (assoc member instance))
           (class (class-type instance))
           (bases (cond ((eq class 'Type)
                         (class-bases-of instance))
                        (class (list class))
                        (t nil))))
      (when (and found
                 (or (and visited (memq 'private (cadr found)))
                     (and (not nested) (memq 'protected (cadr found)))))
        (setq found nil))
      (while (and bases (not found))
        (setq found (class--search-member
                     member (car bases)
                     nested (cons instance visited)))
        (setq bases (cdr bases)))
      found)))

(defun class--read-slots (slots &optional modifiers)
  "Read SLOTS and return the normalized slots."
  (let* ((allowed-modifiers '(public private protected classmethod staticmethod))
         (tail modifiers))
    ;; remove confilctive modifiers
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
            (lambda (slot)
              (let ((key (car slot)) (rest (cdr slot)))
                (cond
                 ((symbolp key)
                  (cond ((eq key 'defun)
                         (list (append (list (car rest) modifiers 'lambda)
                                       (cdr rest))))
                        ((eq key 'setq)
                         (list (cons (car rest)
                                     (cons modifiers (eval (cadr rest))))))
                        ((memq key allowed-modifiers)
                         (class--read-slots rest (cons key modifiers)))
                        (t (error "invalid syntax"))))
                 ((listp key)
                  (class--read-slots rest (append key modifiers)))
                 (t (error "invalid syntax")))))
            slots))))

(defmacro class-type (instance)
  "Return the class of INSTANCE."
  `(cddr (assoc 'class ,instance)))

(defmacro class-bases-of (instance)
  "Return the bases (superclasses) of INSTANCE."
  `(cddr (assoc 'bases ,instance)))

(defmacro classp (instance)
  "Return t if INSTANCE is an class object."
  `(eq (class-type ,instance) 'Type))

;;;;;;;;; aliases
(defmacro @self (property &rest args)
  "A shortcut to (@ self (quote PROPERTY) ARGS).
It can be used in class methods in which first argument is `self'."
  `(@ self (quote ,property) ,@args))

(defmacro @cls (property &rest args)
  "A shortcut to (@ cls (quote PROPERTY) ARGS).
It can be used in class methods in which first argument is `cls'."
  `(@ cls (quote ,property) ,@args))

(defmacro @this (property &rest args)
  "A shortcut to (@ this (quote PROPERTY) ARGS).
It can be used in class methods in which first argument is `this'."
  `(@ this (quote ,property) ,@args))

(defmacro class (name bases &optional doc &rest slots)
  "Define NAME as a class that inherits from BASES.
SLOTS is where you can define class members, which can be one of the
following forms:

Method:
	(defun NAME ARGLIST BODY...)

Property:
	(setq SYM VALUE)

Methodes or properties with specified types:
	(TYPE
		METHODS or PROPERTIES...)

Supported types are `private', `protected', `classmethod' and
`staticmethod'."
  (unless (stringp doc)
    (when doc (setq slots (cons doc slots)))
    (setq doc ""))
  (setq doc (concat "This is a CLASS." (if doc "\n\n") doc))
  (let ((bases (cond ((listp bases) (remove-duplicates bases))
                     ((symbolp bases) (list bases))
                     (t (error "wrong type of bases"))))
        (members (remove-if (lambda (m) (memq (car m) '(class bases)))
                            (remove-duplicates
                             (class--read-slots slots)
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
         ,doc
         (let ((instance '((class . (nil . ,name))))
               (init (cddr (class--search-member 'init ,name))))
           (when init
             (apply init instance args))
           instance)))))

(defun @ (instance member &rest args)
  "Invoke MEMBER of INSTANCE.

If MEMBER is a method, call it with ARGS as arguments.
If MEMBER is a property, return its value or set the value of ARGS
to the property, depending on if ARGS supplied."
  (when (null (class-type instance))
    (error "invalid instance"))
  (let* ((nested (and (boundp 'this--instance)
                      (eq this--instance instance)))
         (this--instance instance)
         ;; a slot is (cons member (cons type body))
         (slot (class--search-member member instance nested))
         (body (cddr slot)))
    (cond ((and (functionp body)
                (not (memq member '(class bases))))
           ;; call instance or class method
           (if (and (classp instance)
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
         (class--search-member member self)))

(class Type (Object))

(provide 'class)
