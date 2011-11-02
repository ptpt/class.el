(require 'cl)
(require 'pt-iterator)

(class Themes (Iterator)
       (defun apply (self)
         (let ((theme (self. current)))
           (when theme
             (cond ((listp theme)
                    (eval theme))
                   ((symbolp theme)
                    (require theme)
                    (funcall theme))
                   (t (error "Color theme not found"))))
           theme)))

(setq pt-themes (Themes nil))

(defun pt-theme-shuffle ()
  (interactive)
  (@ pt-themes 'shuffle)
  (@ pt-themes 'apply))

(defun pt-theme-next ()
  (interactive)
  (@ pt-themes 'next)
  (@ pt-themes 'apply))

(provide 'pt-find-themes)
