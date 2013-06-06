(require 'cl)
(require 'pt-iterator)

(class Themes (Iterator)
       (defun apply (self)
         (let ((theme (@self current)))
           (mapc (lambda (theme) (disable-theme theme))
                 custom-enabled-themes)
           (load-theme theme t)
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
