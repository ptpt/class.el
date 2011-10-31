(require 'cl)
(require 'class)

(defvar pt-font-list '("Courier New-14"
                       "Meslo LG S DZ-11"
                       "Andale Mono-14"
                       "Consolas-14"
                       "Courier-14"))

(class Iterator (Object)
       (defun init (self set)
         (self. set (remove-duplicates set))
         (self. pos 0))

       (defun length (self)
         (length (self. set)))

       (defun next (self &optional n)
         (when (> (self. length) 1)
           (setq n (or n 1))
           (self. pos (mod (+ n (self. pos))
                           (self. length)))
           (self. current)))

       (defun current (self)
         (nth (self. pos) (self. set)))

       (defun shuffle (self)
         (when (> (self. length) 1)
           (let ((n (1+ (random (self. length)))))
             (while (= n (self. length))
               (setq n (1+ (random (self. length)))))
             (self. next n))))

       (defun reset (self)
         (self. pos 0))

       (defun push (self elm)
         (let ((set (self. set))
               (pos (self. pos)))
           (if (= 0 pos)
               (self. set (cons elm set))
             (setcdr (nthcdr (1- pos) set)
                     (cons elm (nthcdr pos set))))
           elm))

       (defun pop (self)
         (let ((set (self. set))
               (pos (self. pos))
               (top (self. current)))
           (if (= 0 pos)
               (self. set (cdr set))
             (setcdr (nthcdr (1- pos) set)
                     (nthcdr (1+ pos) set)))
           (self. next)
           top)))

(class Fonts (Iterator)
       (staticmethod
        (defun exists-p (font)
          (if (stringp font)
              (member (symbol-name (font-get (font-spec :name font) :family))
                      (font-family-list))
            nil)))

       (defun verify (self)
         (@ Fonts 'exists-p (self. current)))

       (defun apply (self)
         (let ((font (self. current)))
           (when (self. verify)
             (add-to-list 'default-frame-alist (cons 'font font))
             (set-frame-font font))
           font)))

(setq pt-fonts
      (Fonts pt-fonts-list))

(defun pt-font-shuffle ()
  (interactive)
  (@ pt-fonts 'shuffle)
  (while (and (not (@ pt-fonts 'verify))
              (@ pt-fonts 'length))
    (@ pt-fonts 'pop)
    (@ pt-fonts 'shuffle))
  (@ pt-fonts 'apply))

(defun pt-font-next ()
  (interactive)
  (@ pt-fonts 'next)
  (@ pt-fonts 'apply))

(defun pt-font-first ()
  (interactive)
  (@ pt-fonts 'reset)
  (while (and (not (@ pt-fonts 'verify))
              (@ pt-fonts 'length))
    (@ pt-fonts 'next))
  (@ pt-fonts 'apply))
