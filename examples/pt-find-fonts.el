(require 'cl)
(require 'pt-iterator)

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

(setq pt-fonts (Fonts nil))

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
              (not (zerop (@ pt-fonts 'length))))
    (@ pt-fonts 'next))
  (@ pt-fonts 'apply))

(provide 'pt-find-fonts)
