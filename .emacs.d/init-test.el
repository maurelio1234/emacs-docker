;;; package --- summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; unit/integration tests for my init.el file

;;; Code:
(require 'ert)

;;; Unit tests

(ert-deftest init-disables-aggressive-mode-in-tsx-files ()
  (with-temp-buffer
    (set-visited-file-name "myfile.tsx")
    (should (equal major-mode 'typescript-mode))
    (should (not aggressive-indent-mode))
    (set-buffer-modified-p nil)))

(ert-deftest init-enables-aggressive-mode-in-test-tsx-files ()
  (with-temp-buffer
    (set-visited-file-name "myfile.test.tsx")
    (should (equal major-mode 'typescript-mode))
    (should aggressive-indent-mode)
    (set-buffer-modified-p nil)))

(ert-deftest init-disables-aggressive-mode-in-test-vue-files ()
  (with-temp-buffer
    (set-visited-file-name "myfile.vue")
    (should (equal major-mode 'vue-mode))
    (should-not aggressive-indent-mode)
    (set-buffer-modified-p nil)))

(ert-deftest init-sets-up-org ()
  (with-temp-buffer
    (org-mode)
    (should org-catch-invisible-edits)))

(ert-deftest init-secret-stuff ()
  (should (featurep 'me/secrets))
  (should me/github)
  (should me/dotfiles))

;; (ert-deftest init-packages ()
;;   (require 'helm-dash)
;;   (should (featurep 'helm-dash)))

(ert-deftest init-themes ()
  (me/dark-mode)
  (should custom-enabled-themes)
  (me/light-mode)
  (should custom-enabled-themes)
  (me/light-office-mode)
  (should custom-enabled-themes))

(ert-deftest init-settings ()
  (should (< register-preview-delay 0.5))
  (should (member 'eshell-history-ring savehist-additional-variables))
  (should (member 'register-alist savehist-additional-variables))
  (should (string-equal python-shell-interpreter "python3"))
  ;; (should calendar-mark-holidays-flag)
  ;; (should (equal calendar-date-style 'european))
  ;; (should (= 21 (length calendar-holidays)))
  (should (> (face-attribute 'default :height) 132))
  (should (< echo-keystrokes 0.5))
  (should-not (and (boundp 'auto-save-mode) auto-save-mode)))

(ert-deftest init-modes ()
  (should savehist-mode))

(ert-deftest init-mode-line ()
  (should (me/init-test--is-diminished 'company-mode))
  (should (me/init-test--is-diminished 'guru-mode))
  (should (me/init-test--is-diminished 'anzu-mode))
  (should (me/init-test--is-diminished 'undo-tree-mode))
  (should (me/init-test--is-diminished 'aggressive-indent-mode))
  (should (me/init-test--is-diminished 'flycheck-mode))
  ;; (should (me/init-test--is-diminished 'company-box-mode))
  ;; (should (me/init-test--is-diminished 'helm-mode))
  (should (me/init-test--is-diminished 'projectile-mode))
  (should (me/init-test--is-diminished 'which-key-mode)))

(ert-deftest init-multiple-cursors ()
  (with-temp-buffer
    (multiple-cursors-mode)
    (should (equal (lookup-key mc/keymap "\C-s") 'phi-search))
    (should (equal (lookup-key mc/keymap "\C-r") 'phi-search-backward))))

(ert-deftest init-prog-mode-clean-whitespace ()
  (with-temp-buffer
    (prog-mode)
    (should (memq 'whitespace-cleanup before-save-hook))))

(ert-deftest init-csharp-mode ()
  (with-temp-buffer
    (set-visited-file-name "myfile.cs")
    (should subword-mode)
    (should omnisharp-mode)
    (me/init-test--is-diminished 'subword-mode)
    (set-buffer-modified-p nil)))

(ert-deftest init-eshell-mode-commands-saved-to-history ()
  (with-temp-buffer
    (let ((randomstr (concat "echo " (number-to-string (random)))))
      (eshell-mode)
      (insert randomstr)
      (eshell-send-input)
      (with-temp-buffer
        (insert-file-contents-literally eshell-history-file-name)
        (search-forward randomstr)))))

;;; Helper functions

(defun me/ert ()
  "Run all init.el test cases.
Save and restore the current enabled theme."
  (interactive)
  (let* ((theme custom-enabled-themes))
    (ert-run-tests
     "init-"
     (lambda (event-type &rest event-args)
       (cl-ecase event-type
         (run-started)
         (test-started)
         (test-ended)
         (run-ended
          (cl-destructuring-bind (stats abortedp) event-args
            (unless (= (ert-stats-total stats) (ert-stats-completed-expected stats))
              (error "Some tests failed!"))))))
     nil)
    ;; TODO refactor out logic for setting theme
    (custom-set-variables
     `(custom-enabled-themes (quote ,theme)))
    (powerline-reset)))

(defun me/init-test--is-diminished (mode)
  "Check if MODE is diminished."
  (cl-some (lambda (x) (equal mode (car x))) diminished-mode-alist))

(provide 'init-test)
;;; init-test.el ends here
