;;; package --- summary
;;; Commentary:
;;; my secrets file
;;; Code:

(defun me/secret-settings ()
  "Set secret configuration."
  (setq projectile-project-search-path '("~/github/"
                                         "~/bitbucket/"))
  (setq me/dotfiles "~/github/dotFiles")
  (setq me/github "~/github")
  (setenv "PATH"
          (concat "/home/" (user-login-name) "/bin" ":" (getenv "PATH")))

  (setenv "WORK" "~/bitbucket/work")
  (projectile-add-known-project "~/exercism"))

(provide 'me/secrets)
;;; secrets.el ends here
