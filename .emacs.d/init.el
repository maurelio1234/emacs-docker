;;; package --- summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; my init.el file

;;; Code:
;;;; Initialization
(require 'package)

;; No GC
;; https://github.com/hlissner/doom-emacs/wiki/FAQ#how-is-dooms-startup-so-fast
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; Then reset it as late as possible; these are the reasonable defaults I use.
(add-hook 'emacs-startup-hook (lambda () (setq gc-cons-threshold 16777216
                                               gc-cons-percentage 0.1)))

(require 'bootstrap "~/.emacs.d/bootstrap.el")

;; Compute init loading time
;; (from: https://github.com/emacs-dashboard/emacs-dashboard/issues/57)
(defvar me/init-el-start-time (current-time) "Time when init.el was started.")
(add-hook
 'after-init-hook
 (lambda
   ()
   (message
    " ★ Emacs initialized in %.2fs ★ "
    (float-time (time-subtract (current-time) me/init-el-start-time)))))

;; TODO I don't really know why I need it
(require 'transient nil t)

;; TODO: automate installation of eterm-color
;; (https://www.emacswiki.org/emacs/AnsiTermHints)

;;;; My variables

(defvar
  me/dotfiles
  "~/github/dotFiles"
  "The place where I checked out my dotfiles github repository.")

(defvar
  me/github
  "~/github"
  "The place where I checked out all my github repositories.")

(defvar
  me/in-docker-p
  (string-equal "true" (getenv "DOCKER"))
  "Are we on a docker container?")

;;;; My functions
(defun me/db (server user password db)
  "Connect to DB in SERVER using USER and PASSWORD."
  (interactive)
  (let
      ((buffer-name (format "*mssql %s@%s@%s*" user db server))
       (command (format
                 "reset && mssql-cli -U %s -P %s -d %s -S %s && exit"
                 user
                 password
                 db
                 server)))
    (if (get-buffer buffer-name)
        (switch-to-buffer buffer-name)
      (vterm buffer-name)
      (with-current-buffer buffer-name
        (vterm-send-string command)
        (vterm-send-return)))
    buffer-name))

(defun me/dockerfile-mode-hook ()
  "Hook for dockerfile mode, fixes indent."
  ;; https://github.com/sp3ctum/spacemacs/commit/99e9875f8
  (setq-local indent-line-function #'sh-indent-line))

(defun me/powerline-theme ()
  "Setup the my mode-line."
  (interactive)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (tabs (frame-parameter nil 'tabs))
                          (tabs-count (length tabs))
                          (tab-name (when (and tabs (> tabs-count 1))
                                      (cdr (assq 'name (assq 'current-tab tabs)))))
                          (mode-line (if active 'mode-line 'mode-line-inactive))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (separator-left (intern (format "powerline-%s-%s"
                                                          powerline-default-separator
                                                          (car powerline-default-separator-dir))))
                          (separator-right (intern (format "powerline-%s-%s"
                                                           powerline-default-separator
                                                           (cdr powerline-default-separator-dir))))
                          (lhs (list (powerline-raw "%*" nil 'l)
                                     (powerline-buffer-size nil 'l)
                                     (powerline-raw mode-line-mule-info nil 'l)
                                     (powerline-buffer-id nil 'l)
                                     (when me/in-docker-p
                                       (powerline-raw " (DOCKER) "))
                                     (when tabs
                                       (powerline-raw
                                        (concat " -" tab-name "- ")))
                                     (when
                                         (and
                                          (boundp 'which-func-mode)
                                          which-func-mode)
                                       (powerline-raw which-func-format nil 'l))
                                     (powerline-raw " ")
                                     (funcall separator-left mode-line face1)
                                     (when
                                         (boundp 'erc-modified-channels-object)
                                       (powerline-raw
                                        erc-modified-channels-object
                                        face1
                                        'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (funcall separator-left face1 face2)
                                     (powerline-vc face2 'r)))
                          (rhs (list (powerline-raw global-mode-string face2 'r)
                                     (funcall separator-right face2 face1)
                                     (powerline-raw "%4l" face1 'l)
                                     (powerline-raw ":" face1 'l)
                                     (powerline-raw "%3c" face1 'r)
                                     (funcall separator-right face1 mode-line)
                                     (powerline-raw " ")
                                     (powerline-raw "%6p" nil 'r)
                                     (powerline-hud face2 face1))))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))

;;; Misc
(defun me/magit-commit-setup ()
  "Generate magit commit template."
  (let* ((branch-name    (magit-get-current-branch))
         (merge-commit?  (save-excursion
                           (search-forward "Merge branch" nil t)))
         (revert-commit?  (save-excursion
                            (search-forward "Revert " nil t)))
         (amend-commit?  (save-excursion
                           (and
                            (prog2
                                (goto-char 0)
                                (search-forward branch-name nil t))
                            (= (1+ (length branch-name)) (point)))))
         (master-branch? (equal branch-name "master"))
         (staged-files   (magit-staged-files))
         (staged-file    (car staged-files))
         (staged-filename (f-filename staged-file))
         (staged-dirname  (f-filename (f-parent staged-file)))
         (category-name   (if (string-equal "values.yaml" staged-filename)
                              staged-dirname
                            staged-filename))
         (category       (if (= 1 (length staged-files))
                             (concat category-name ": ")
                           ""))
         (branch-prefix  (if
                             master-branch?
                             ""
                             (concat branch-name ": "))))
    (unless (or merge-commit?n
                revert-commit?
                amend-commit?)
      (insert branch-prefix category)))

(defun me/eww-after-render-hook ()
  "Things to do after rendereing a eww page."
  (rename-buffer
   (concat
    "*eww* "
    (plist-get eww-data :title)
    " - "
    (plist-get eww-data :url))))

(defun me/shell-hook ()
  "Fix things I hate in this mode."
  (read-only-mode 1)
  (when (and (eq major-mode 'shell-mode) (get-buffer-process (current-buffer)))
    (rename-buffer
     (concat
      "*Process* "
      list-buffers-directory
      " "
      (string-join (process-command (get-buffer-process (current-buffer))) " "))
     t)))

(defun me/fix-async-processes ()
  "Fix async processes so they are nicer to use."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (me/shell-hook))))

(defun me/kill-async-no-process ()
  "Kill all shell buffers without process."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and
             (eq major-mode 'shell-mode)
             (not (get-buffer-process (current-buffer))))
        (kill-buffer buffer)))))

(defun me/revert-buffer-noconfirm ()
  "Revert current buffer without asking for confirmation."
  (interactive)
  (revert-buffer nil t))

(defun me/insert-buffer-name ()
  "Insert the name of the current (non minibuffer) buffer in the current bufer.
To be used on a minibuffer."
  (interactive)
  (insert (buffer-name (stackoverflow/current-buffer-not-mini))))

(defun me/node-repl ()
  "Node js repl."
  (interactive)
  (setenv "NODE_NO_READLINE" "1") ;avoid fancy terminal codes
  (pop-to-buffer (make-comint "node-repl" "node" nil "--interactive")))

;; TODO port it to docker
(defun me/fix-caps ()
  "Fix caps lock key."
  (interactive)
  (shell-command "setxkbmap -layout fr -option ctrl:nocaps"))

(defun me/switch-to-last-buffer ()
  "Switch to latest buffer.
Useful when switching back and forth between two buffers."
  (interactive)
  (switch-to-buffer nil))

(defun me/frame-fullscreen (&optional frame)
  "Set frame to fullscreen.
Optionlly, FRAME is the frame to turn to fullscreen.
This code is based on the code of `toogle-frame-fullscreen'"
  (let* ((fullscreen (frame-parameter frame 'fullscreen)))
    (modify-frame-parameters frame
                             `((fullscreen . fullboth)
                               (fullscreen-restore . ,fullscreen)))
    (when (featurep 'cocoa) (sleep-for 0.5))))

(defun stackoverflow/current-buffer-not-mini ()
  "Return 'current-buffer' if current buffer is not the *mini-buffer* else return buffer before minibuf is activated."
  (if (not (window-minibuffer-p)) (current-buffer)
    (if (eq (get-lru-window) (next-window))
        (window-buffer (previous-window)) (window-buffer (next-window)))))

(defun stackoverflow/recompile-all-packages ()
  "Should be used after an Emacs version upgrade.
For more information: https://stackoverflow.com/questions/24725778/how-to-rebuild-elpa-packages-after-upgrade-of-emacs"
  (byte-recompile-directory package-user-dir nil 'force))

(defun me/open-line-above ()
  "Similar to Vim Control o."
  (move-beginning-of-line nil)
  (open-line 1))

(defun me/projectile-project-dashboard ()
  "Dashboard for projectile project."
  (interactive)
  (if (me/git-repository-p)
      (me/magit-status-full-screen)
    (projectile-dired)))

(defun me/magit-status-full-screen ()
  "Full screen magit status."
  (interactive)
  (message "Loading git status...")
  (magit-status)
  (delete-other-windows)
  (message ""))

(defun me/git-repository-p ()
  "Is current directory a git repository?"
  (interactive)
  (file-exists-p (concat default-directory ".git")))

;; From Emacswiki: https://www.emacswiki.org/emacs/PasswordGenerator
(defun wiki/make-password ()
  "Return a password."
  (interactive)
  (let ((length 10)
        (upper t)
        (lower t)
        (number t)
        (symbol t)
        (ambiguous nil))
    (let ((char-list (wiki/make-password-char-list upper
                                                   lower
                                                   number
                                                   symbol
                                                   ambiguous))
          position
          password)
      (random t)
      (loop for i from 1 to length
            do (setq position (random (length char-list))
                     password (concat
                               password
                               (string (nth position char-list)))))
      (kill-new password)
      (message "New password generated and added to kill ring!"))))

(defun wiki/make-password-char-list (upper lower number symbol ambiguous)
  "Make password according to criteria (UPPER, LOWER, NUMBER, SYMBOL or AMBIGUOUS."
  (let* ((upper-chars-ambiguous '(?I ?O ?G))
         (upper-chars (loop for i from ?A to ?Z unless
                            (member i upper-chars-ambiguous)
                            collect i))
         (lower-chars-ambiguous '(?l ?o))
         (lower-chars (loop for i from ?a to ?z unless
                            (member i lower-chars-ambiguous)
                            collect i))
         (number-chars-ambiguous '(?0 ?1 ?6))
         (number-chars (loop for i from ?0 to ?9 unless
                             (member i number-chars-ambiguous)
                             collect i))
         (symbol-chars '(?! ?@ ?# ?$ ?% ?& ?* ?\( ?\) ?+ ?= ?/
                            ?{ ?} ?\[ ?\] ?: ?\; ?< ?>))
         (symbol-chars-ambiguous '(?_ ?- ?| ?, ?. ?` ?' ?~ ?^ ?\"))
         char-list)
    (if upper
        (setq char-list (append char-list upper-chars)))
    (if lower
        (setq char-list (append char-list lower-chars)))
    (if number
        (setq char-list (append char-list number-chars)))
    (if symbol
        (setq char-list (append char-list symbol-chars)))
    (if ambiguous
        (setq char-list (append char-list
                                upper-chars-ambiguous
                                lower-chars-ambiguous
                                number-chars-ambiguous
                                symbol-chars-ambiguous)))
    char-list))

;;; Goodies
(defun me/Emacsd()
  "Load emacs.d."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun me/SaveConfigs ()
  "Save configuration files onto github dotfiles project."
  (interactive)
  (message "q to close terminal")
  (magit-shell-command (concat "bash " me/dotfiles "/saveConfigs.sh")))

(defun me/dark-mode ()
  "Dark mode theme."
  (interactive)
  (custom-set-variables
   '(custom-enabled-themes (quote (doom-outrun-electric))))
  (setq frame-background-mode 'dark)
  (powerline-reset)
  (tab-bar-mode -1))

(defun me/light-mode ()
  "Light mode theme."
  (interactive)
  (custom-set-variables
   '(custom-enabled-themes (quote (doom-opera-light))))
  (setq frame-background-mode 'light)
  (powerline-reset))

(defun me/light-office-mode ()
  "Light mode theme when working on the office."
  (interactive)
  (custom-set-variables
   '(custom-enabled-themes (quote (leuven))))
  (setq frame-background-mode 'light)
  (powerline-reset))

;;; Eshell
(defun me/shell-history ()
  "Let me choose a comand from eshell and bash histories."
  (interactive)
  (eshell-bol)
  (ignore-errors (kill-line))
  (message "Current command was added to kill ring!")
  (let* ((ring-commands-list (ring-elements eshell-history-ring))
         (history-list (with-temp-buffer
                         (insert-file-contents-literally "~/.bash_history")
                         (insert-file-contents-literally
                          eshell-history-file-name)
                         (split-string (buffer-string) "\n" t)))
         (complete-list (-distinct (append ring-commands-list history-list)))
         (command (completing-read "Command: " complete-list)))
    (when command
      (insert command))))

(defun me/eshell-input-filter (input)
  "Filter certain common INPUT commands out of eshell history."
  (and
   (eshell-input-filter-default input)
   (eshell-input-filter-initial-space input)
   (not (string-prefix-p "cat " input))
   (not (string-prefix-p "find-file " input))))

;;; Exercism
(defun me/exercism-configure (token)
  "Configure Exercism TOKEN and config directory."
  (interactive "sAPI token: ")
  (shell-command (concat
                  "exercism configure "
                  "--token " token
                  " --workspace " "/home/" (user-login-name) "/exercism/")))

(defun me/exercism-submit ()
  "Submit current file as exercism solution."
  (interactive)
  (let* ((filename (buffer-file-name (stackoverflow/current-buffer-not-mini)))
         (commandline (format "exercism submit %s" filename))
         (url (with-temp-buffer
                (shell-command commandline (current-buffer) nil)
                (goto-char (point-min))
                (search-forward "https")
                (back-to-indentation)
                (kill-line)
                (car kill-ring))))
    (browse-url-firefox url)))

(defun me/exercism-download ()
  "Download exercice using command copied from website."
  (interactive)
  (let* ((download-command (read-string "Download command: "))
         (download-path (with-temp-buffer
                          (shell-command download-command (current-buffer) nil)
                          (goto-char (point-min))
                          (forward-line 2)
                          (kill-line)
                          (car kill-ring))))
    (message download-command)
    (dired download-path)))

;;; Search/occur
(defun me/search-word ()
  "Search current word in current project's files."
  (interactive)
  (let* ((initial-text (if (use-region-p)
                           (progn
                             (deactivate-mark)
                             (buffer-substring-no-properties
                              (region-beginning)
                              (region-end)))
                         (thing-at-point 'word 'no-properties)))
         (subword-mode-enabled subword-mode))
    (subword-mode -1)
    (cond
     (helm-mode (helm-grep-do-git-grep t))
     (ivy-mode (counsel-git-grep initial-text))
     (t (vc-git-grep initial-text)))
    (subword-mode (if subword-mode-enabled 1 -1))))

(defun me/search-word-in-file ()
  "Search current word in current file."
  (interactive)
  (let* ((initial-text (if (use-region-p)
                           (progn
                             (deactivate-mark)
                             (buffer-substring-no-properties
                              (region-beginning)
                              (region-end)))
                         (thing-at-point 'word 'no-properties)))
         (subword-mode-enabled subword-mode))
    (subword-mode -1)
    (cond
     (helm-mode (helm-occur))
     (ivy-mode (counsel-grep-or-swiper initial-text))
     (t (occur initial-text)))
    (subword-mode (if subword-mode-enabled 1 -1))))

(defun me/just-occur ()
  "Activate occur in current file."
  (interactive)
  (cond
   (helm-mode (helm-occur))
   (ivy-mode (counsel-grep-or-swiper))
   (t (occur))))

;;; Hooks
(defun me/emacs-startup-hook ()
  "Things to do on startup."
  (unless
      me/bootstraping-p
    (message "Default theme...")
    (me/dark-mode) ;; FIX Reactivate

    (set-face-attribute 'default nil :height 180)

    ;; Disable tab bar
    (tab-bar-mode -1)

    (unless me/in-docker-p
      (message "Changing caps lock for ctrl...")
      (when (eq window-system 'x)
        (me/fix-caps)))))

(defun me/minibuffer-hook ()
  "Things to do when entering minibuffer."
  (electric-pair-local-mode 1))

(defun me/prog-mode-hook ()
  "Prog mode hook."
  (interactive)
  (subword-mode 1)
  (electric-pair-local-mode 1)
  (whitespace-mode 1)
  (setq-local show-trailing-whitespace t)
  (setq-local show-leading-whitespace t)
  (setq-local indicate-empty-lines t)
  (when (or
         (s-ends-with-p ".cake" (buffer-file-name))
         (s-ends-with-p "/data.json" (buffer-file-name))
         (string-suffix-p ".vue" (buffer-file-name))
         (and
          (string-suffix-p ".tsx" (buffer-file-name))
          (not (string-suffix-p "test.tsx" (buffer-file-name)))))
    (aggressive-indent-mode -1))
  (add-hook 'before-save-hook 'whitespace-cleanup nil 'make-it-local))

;;;; My minor modes

;;; CircleCI build output minor mode
(defvar me/circleci-test-marker "^=\> "
  "Regex to identify a NUnit test.")

(defun me/circleci-narrow-to-test (test-name)
  "Narrow current buffer to log beloging to a given TEST-NAME."
  (interactive "sTest name: ")
  (setq test-name (or test-name (me/circleci--current-test-name)))
  (goto-char (point-min))
  (search-forward-regexp test-name)
  (move-beginning-of-line nil)
  (let ((start-test (point)))
    (unless (search-forward-regexp me/circleci-test-marker nil t 2)
      (goto-char (point-max)))
    (move-end-of-line nil)
    (let ((end-test (point)))
      (narrow-to-region start-test end-test)
      (goto-char (point-min)))))

(defun me/circleci--current-test-name ()
  "Return the name of the current test."
  (save-excursion
    (move-end-of-line nil) ; in case we are in the first line
    (search-backward-regexp me/circleci-test-marker)
    (search-forward-regexp " ")
    (let ((test-name-start (point)))
      (move-end-of-line nil)
      (let ((test-name-end (point)))
        (buffer-substring-no-properties test-name-start test-name-end)))))

;; TODO: check if we are already in narrow mode
(defun me/circleci-narrow-to-current-test ()
  "Narrow to current test."
  (interactive)
  (me/circleci-narrow-to-test (me/circleci--current-test-name)))

(defun me/circleci/previous-test-case ()
  "Navigate to previous test case."
  (interactive)
  (if (search-backward-regexp me/circleci-test-marker nil t)
      (move-beginning-of-line nil)
    (message "This is the first test case!")))

(defun me/circleci/next-test-case ()
  "Navigate to next test case."
  (interactive)
  (move-end-of-line nil) ;; to avoid matching the case where we are NOW
  (if (search-forward-regexp me/circleci-test-marker nil t)
      (move-beginning-of-line nil)
    (message "This is the last test case!")))

(defun me/circleci-display-current-test ()
  "Display current test."
  (interactive)
  (message (me/circleci--current-test-name)))

(define-minor-mode circleci-build-mode
  "Minor mode for navigating on CircleCI TI build outputs."
  nil
  " TI"
  :keymap  (let ((map (make-sparse-keymap)))
             (define-key map (kbd "M-n") 'me/circleci/next-test-case)
             (define-key map (kbd "M-p") 'me/circleci/previous-test-case)
             (define-key map (kbd "C-c , n") 'me/circleci-narrow-to-current-test)
             (define-key map (kbd "C-c , w") 'widen)
             (define-key map (kbd "C-c , ,") 'me/circleci-display-current-test)
             (define-key map (kbd "C-c , g") 'me/circleci-narrow-to-test)
             map)
  :global nil)

;;; Programming misc
(defun me/build-output ()
  "Load a dired buffer with build downloads."
  (interactive)
  (dired "~/Downloads/build*.txt")

  ;; for some reason sometimes I get a stale view of the directory
  (revert-buffer nil t)

  ;; selects last file, since it's probably the one I want to open
  (goto-char (point-max))
  (dired-previous-line 1))

;;; CSharp mode customizations
(defun me/nunit-cmd (version category dll)
  "Generate the NUnit command depending on the selected DLL, runner VERSION and CATEGORY filter."
  (let* ((exe (if (eq version 2)
                  "./tools/NUnit.Runners.2.6.4/tools/nunit-console.exe"
                "./tools/NUnit.ConsoleRunner.3.10.0/tools/nunit3-console.exe"))
         (cat (if category
                  (if (eq version 2)
                      (concat "--include:" category)
                    (concat "--where \"cat == " category "\""))
                ""))
         (cmd (concat
               "mono  --debug "
               exe
               " --config=Debug "
               (concat default-directory dll)
               " "
               cat)))
    cmd)) ; --runtime=4.5.1

;; TODO test: locate-dominating-file
(defun me/find-something (pattern)
  "Helper function to find files ressembling a given PATTERN upwards from current path."
  (car (or (file-expand-wildcards pattern)
           (file-expand-wildcards (concat "../" pattern))
           (file-expand-wildcards (concat "../../" pattern))
           (file-expand-wildcards (concat "../../../" pattern))
           (file-expand-wildcards (concat "../../../.." pattern)))))

(defun me/nunit-find-test-dll ()
  "Find the generated test DLL."
  (me/find-something "*/*/*Tests*.dll"))

(defun me/csharp-find-csproj ()
  "Find project csproj."
  (me/find-something "*.csproj"))

(defun me/csharp-find-sln ()
  "Find project sln."
  (me/find-something "*.sln"))

(defun me/csharp-build-csproj ()
  "Build curent csproj."
  (interactive)
  (compile
   (concat
    "msbuild /p:buildmode=debug /p:PreBuildEvent= /p:PostBuildEvent= "
    (me/csharp-find-csproj))))

(defun me/csharp-build-sln ()
  "Build current sln."
  (interactive)
  (compile
   (concat
    "msbuild /p:buildmode=debug /p:PreBuildEvent= /p:PostBuildEvent= "
    (me/csharp-find-sln))))

(defun me/nunit2-run-tests-me ()
  "Run TestMe NUnit2."
  (interactive)
  (magit-shell-command-topdir (concat (me/nunit-cmd 2 "TestMe" (me/nunit-find-test-dll)))))

(defun me/nunit2-run-tests-all ()
  "Run all NUnit2."
  (interactive)
  (magit-shell-command-topdir (concat (me/nunit-cmd 2 nil (me/nunit-find-test-dll)))))

(defun me/nunit3-run-tests-all ()
  "Run all NUnit3."
  (interactive)
  (magit-shell-command-topdir (concat (me/nunit-cmd 3 nil (me/nunit-find-test-dll)))))

(defun me/nunit3-run-tests-me ()
  "Run TestMe NUnit3."
  (interactive)
  (magit-shell-command-topdir (concat (me/nunit-cmd 3 "TestMe" (me/nunit-find-test-dll)))))


(defun me/fix-csharp-mode ()
  "Fixes some variables in cc-mode that break csharp mode."
  (interactive)
  ;; this commit seems to have introduced an incomcompatibility between csharp and c modes
  ;; https://github.com/emacs-mirror/emacs/commit/b61b3c804cb7c1348097ccef18d26fa4bdd37117
  (setq-local c-block-stmt-hangon-key "")
  (setq-local c-block-stmt-2-key "")
  (setq-local c-block-stmt-1-key "")
  (setq-local c-block-stmt-1-2-key "")
  ;; (setq c-syntactic-ws-start "") ; for some reason this is not needed anymore
  )


(defun me/nunit-add-category (pattern)
  "Add TestMe category before the previous appearance of PATTERN."
  (search-backward pattern)
  (me/open-line-above)
  (insert "[Category(\"TestMe\")]")
  (when (boundp 'evil-local-mode) (evil -normal-state)))

(defun me/nunit-add-category-testcase ()
  "Add TestMe to current test case."
  (interactive)
  (me/nunit-add-category "[Test"))

(defun me/nunit-add-category-testfixture ()
  "Add TestME category to current test fixture."
  (interactive)
  (me/nunit-add-category "[TestFixture"))

(defun me/cedict-entries (chinese)
  "Return entries that match the word CHINESE."
  (unless cc-cedict-cache
    (setq cc-cedict-cache (cc-cedict-parse)))
  (seq-filter
   (lambda (entry) (or (string= chinese (cc-cedict-entry-traditional entry))
                       (string= chinese (cc-cedict-entry-simplified entry))))
   cc-cedict-cache))

;;; CC-CEDIT Popup Dictionary
(defvar me/cc-cedict-cache
  nil
  "Cache dictionary ZH->Pinyin/English.")

(defun me/ensure-cc-init-cache ()
  "Make sure the dictionary cache is initialized."
  (unless me/cc-cedict-cache
    (setq me/cc-cedict-cache (make-hash-table :test 'equal :size 4000000))

    (unless cc-cedict-cache
      (setq cc-cedict-cache (cc-cedict-parse)))

    (seq-each
     (lambda (entry)
       (let* ((cache me/cc-cedict-cache)
              (traditional (cc-cedict-entry-traditional entry))
              (simplified  (cc-cedict-entry-simplified entry))
              (existing-simplified  (gethash simplified cache ""))
              (existing-traditional (gethash traditional cache ""))
              (to-add (format
                       "%s:\n %s\n"
                       (propertize
                        (cc-cedict-entry-pinyin entry)
                        'face
                        '(:underline t :weight bold))
                       (mapconcat
                        (lambda (english) (concat "\t" english "\n"))
                        (cc-cedict-entry-english entry)
                        "")))
              (new-simplified (concat existing-simplified to-add))
              (new-traditional (concat existing-traditional to-add)))
         (puthash simplified new-simplified cache)
         (puthash traditional new-traditional cache)))
     cc-cedict-cache)))

(defun me/cc-cedict-extended-translation (chinese)
  "Return translation of CHINESE including 'see also' references."
  (me/ensure-cc-init-cache)
  (let*
      ((cache me/cc-cedict-cache)
       (original-translation-text (gethash chinese cache nil))
       (extra-translations ""))
    (if original-translation-text
        (with-temp-buffer
          (insert original-translation-text)
          (goto-char (point-min))
          (while
              (re-search-forward
               "see \\(also \\)?\\([^\\|]*\\)\\(.*\\)?\\[.*\\]"
               nil
               t)
            (setq
             extra-translations
             (concat
              extra-translations
              (gethash (match-string 2) cache nil))))
          (concat original-translation-text "\n" extra-translations))
      nil)))

(defun me/cc-cedict-find-word-around-point ()
  "Find the longest chinese word around point."
  (me/ensure-cc-init-cache)
  (if (use-region-p)
      (buffer-substring-no-properties
       (region-beginning)
       (region-end))
    (let* ((cache me/cc-cedict-cache)
           (left (point))
           (right (1+ (point)))
           (current
            (gethash (buffer-substring-no-properties left right) cache nil)))
      (when current

        ;; left direction
        (while (gethash (buffer-substring-no-properties left right) cache nil)
          (setq right (1+ right)))
        (setq right (1- right))

        ;; right direction
        (while (gethash (buffer-substring-no-properties left right) cache nil)
          (setq left (1- left)))
        (setq left (1+ left))

        (buffer-substring-no-properties left right)))))

(defun me/cc-cedict-selection-hide()
  "Hide the popup displayed by me/cc-cedict-selection."
  (interactive)
  (pos-tip-hide))

(defun me/cc-cedict-selection ()
  "Show the pinyin and english translation for text in current region."
  (interactive)
  (me/ensure-cc-init-cache)
  (let* ((chinese-text (me/cc-cedict-find-word-around-point))
         (translation-text
          (me/cc-cedict-extended-translation chinese-text)))
    (if translation-text
        (pos-tip-show-no-propertize
         (concat
          (format
           "%s\n\n"
           (propertize
            chinese-text
            'face
            '(:height 1.25 :weight bold :underline t)))
          translation-text)
         nil
         nil
         nil
         10
         6000) ; just give it as much as room as necessary
      (youdao-dictionary-search chinese-text))))

;;;; Packages I use and their configuration
;; https://github.com/raxod502/straight.el/issues/361
(straight-use-package '(org :type built-in))
(use-package emacs
  :diminish (subword-mode . " sW")
  :bind
  ("C-z" . nil) ; disables C-z
  ("C-c 5 b" . 'select-frame-by-name)
  ("C-c D" . 'cd)
  ("C-c E m" . 'me/Emacsd)
  ("C-c E d" . 'me/dark-mode)
  ("C-c E l" . 'me/light-mode)
  ("C-c E o" . 'me/light-office-mode)
  ("C-c R" . 'me/revert-buffer-noconfirm)
  ("C-c r" . 'rename-buffer)
  ("C-c S a" . 'me/SaveConfigs)
  ("C-c o" . 'me/switch-to-last-buffer)
  ("C-c q" . 'so-long)
  ("C-c O" . 'other-frame)
  ("C-c K" . 'me/kill-async-no-process)
  ("M-s-o" . 'me/switch-to-last-buffer)
  ("M-<dead-circumflex>" . 'delete-indentation)
  ("C-<dead-circumflex>" . 'join-line)
  ("M-^" . 'delete-indentation)
  ("C-^" . 'join-line)
  ("M-s-à" . 'delete-window)
  ("C-x é" . 'split-window)        ; comme C-x 2 mais on se passe de MAJ
  ("C-x \"" . 'split-window-horizontally) ;C-x 3
  ("C-x &" . 'delete-other-windows)             ;C-x 1
  ("C-x à" . 'delete-window)              ;C-x 0
  ;; ("C-c f w" . 'me/search-word)
  ("M-s-w" . 'me/search-word)
  ("M-s-l" . 'me/search-word-in-file)
  ("M-s-f" . 'me/firefox)
  ("C-c F" . 'me/firefox)
  ("C-c f" . 'browse-url-firefox)
  ("M-s-m" . 'me/just-occur)
  ("s-l" . 'me/lock)
  ("M-s-c" . 'me/clockify)
  ("C-c C" . 'me/clockify)
  ("C-c t t" . 'tab-bar-mode)
  ("C-c t RET" . 'tab-switcher)

  (:map minibuffer-local-map ("<f5>" . 'me/insert-buffer-name))
  (:map paredit-mode-map ("C-c b" . 'eval-buffer))

  :hook
  (minibuffer-setup . me/minibuffer-hook)
  (dired-load . (lambda ()
                  (load "dired-x")
                  ;; Set dired-x global variables here.  For example:
                  ;; (setq dired-guess-shell-gnutar "gtar")
                  ;; (setq dired-x-hands-off-my-keys nil)
                  ))
  (dired-mode . (lambda ()
                  ;; Set dired-x buffer-local variables here.  For example:
                  ;; (dired-omit-mode 1)
                  ))
  (text-mode . (lambda ()
                 (when
                     (zerop
                      (string-match
                       "^build_.*step.*container.*txt"
                       (buffer-name)))
                   (circleci-build-mode 1))))
  :config
  ;; Settings
  ;; From https://wikemacs.org/wiki/French
  (setq calendar-date-style 'european
        calendar-holidays '((holiday-fixed 1 1 "Jour de l'an")
                            (holiday-fixed 1 6 "Épiphanie")
                            (holiday-fixed 2 2 "Chandeleur")
                            (holiday-fixed 2 14 "Saint Valentin")
                            (holiday-fixed 5 1 "Fête du travail")
                            (holiday-fixed 5 8 "Commémoration de la capitulation de l'Allemagne en 1945")
                            (holiday-fixed 6 21 "Fête de la musique")
                            (holiday-fixed 7 14 "Fête nationale - Prise de la Bastille")
                            (holiday-fixed 8 15 "Assomption (Religieux)")
                            (holiday-fixed 11 11 "Armistice de 1918")
                            (holiday-fixed 11 1 "Toussaint")
                            (holiday-fixed 11 2 "Commémoration des fidèles défunts")
                            (holiday-fixed 12 25 "Noël")
                            ;; fêtes à date variable
                            (holiday-easter-etc 0 "Pâques")
                            (holiday-easter-etc 1 "Lundi de Pâques")
                            (holiday-easter-etc 39 "Ascension")
                            (holiday-easter-etc 49 "Pentecôte")
                            (holiday-easter-etc 50 "Lundi de Pentecôte")
                            (holiday-easter-etc -47 "Mardi gras")
                            (holiday-float 6 0 3 "Fête des pères") ;; troisième dimanche de juin
                            ;; Fête des mères
                            (holiday-sexp
                             '(if (equal
                                   ;; Pentecôte
                                   (holiday-easter-etc 49)
                                   ;; Dernier dimanche de mai
                                   (holiday-float 5 0 -1 nil))
                                  ;; -> Premier dimanche de juin si coïncidence
                                  (car (car (holiday-float 6 0 1 nil)))
                                ;; -> Dernier dimanche de mai sinon
                                (car (car (holiday-float 5 0 -1 nil))))
                             "Fête des mères"))
        calendar-mark-holidays-flag t)
  (setq savehist-save-minibuffer-history 1)
  (setq savehist-additional-variables
        '(kill-ring
          search-ring
          regexp-search-ring
          last-kbd-macro
          kmacro-ring
          shell-command-history
          eww-history
          Info-history-list
          register-alist
          eshell-history-ring))
  (setq battery-mode-line-format " %b %t %rW")
  (setq echo-keystrokes 0.1)
  (setq python-shell-interpreter "python3")
  (setq register-preview-delay 0.2)
  (setq compilation-scroll-output t)
  (setq-default indicate-empty-lines t)
  (setq display-time-day-and-date nil)
  (setq display-time-24hr-format t)

  (setq vc-follow-symlinks t)
  (setq make-backup-files nil)
  (setq ring-bell-function 'ignore)
  (setq revert-without-query (quote (".*")))

  (setq set-mark-command-repeat-pop t)
  (setq history-delete-duplicates t)
  (setq use-dialog-box nil)
  (defalias 'yes-or-no-p 'y-or-n-p)

  (setq async-shell-command-buffer 'rename-buffer)
  (setq shell-command-prompt-show-cwd t)
  (setq shell-command-dont-erase-buffer 'beg-last-out)
  (setq vterm-shell "/bin/bash")

  (add-to-list 'default-frame-alist '(font . "JetBrains Mono" )) ; Noto Mono
  (set-face-attribute 'default t :font "JetBrains Mono")

  (setenv "PATH"
          (concat "/home/" (user-login-name) "/bin" ":" (getenv "PATH")))
  (setenv "WORK" "~/bitbucket/work")

  (setenv
   "DOTNET_SKIP_FIRST_TIME_EXPERIENCE"
   "true") ; so that dotnet stops printing obnoxious messages

  (setenv "GIT_PAGER" "cat") ; so that I can use git without paging..

  ;; Modes
  (display-time-mode 1)
  (display-battery-mode 1)
  (tooltip-mode    -1)
  (desktop-save-mode -1)
  (delete-selection-mode 1)
  (size-indication-mode)
  (auto-save-visited-mode -1)
  (column-number-mode 1)
  (savehist-mode t)
  (electric-pair-mode -1)
  (global-auto-revert-mode -1)
  (global-hl-line-mode 1)
  (global-whitespace-mode -1)

  ;; Its off by default, it works, it's just not visible, so it's a win-win
  (tab-bar-mode -1)
  (global-so-long-mode 1)

  ;; Hooks
  (add-hook 'prog-mode-hook 'me/prog-mode-hook)
  (add-hook 'after-init-hook 'me/frame-fullscreen)
  (add-hook 'emacs-startup-hook 'me/emacs-startup-hook)
  (add-hook 'shell-mode-hook 'me/shell-hook)

  ;; Enabled advanced commands
  (put 'narrow-to-region 'disabled nil))

(use-package cc-cedict
  :config
  (setq
   cc-cedict-file
   "~/Downloads/cedict_1_0_ts_utf-8_mdbg/cedict_ts.u8"))

(use-package docker
  :hook ((dockerfile-mode . me/dockerfile-mode-hook)))

(use-package org
  :straight nil
  :ensure nil
  :defer t
  :init
  (defun me/setup-org ()
    "Customizations to be activated on Org mode."
    (visual-line-mode t)
    (org-indent-mode 1))
  :bind
  ("C-c a t" . 'org-todo-list)
  :hook ((org-mode . me/setup-org))
  :config
  (setq org-todo-keywords '((sequence "TODO" "DOING" "|" "CANCELLED" "DONE"))
        org-startup-with-inline-images t
        org-log-done 'time)
  (require 'jq-mode)
  (require 'ob-http)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((js . t)
     (dot . t)
     (jq . t)
     (lilypond . t)
     (python . t)
     (sed . t)
     (sql . t)
     (shell . t)
     (http . t)
     (emacs-lisp . t)))
  (require 'org-indent)
  (require 'htmlize)
  (require 'ox-reveal)
  (setq org-catch-invisible-edits 'error)
  (setq org-default-notes-file (concat (concat me/github "/perso/notes.org")))
  (define-key global-map "\C-cc" 'org-capture))

;; Should be loaded before every package that uses diminish
(use-package diminish :config (diminish 'eldoc-mode))

(use-package speed-type
  :config
  (require 'disable-mouse)
  (defun me/speed-type ()
    (interactive)
    (speed-type-text)
    (disable-mouse-mode 1)))

(use-package undo-tree
  :diminish ""
  :config (global-undo-tree-mode)
  (setq undo-tree-show-minibuffer-help t))

(use-package kubernetes
  :commands (kubernetes-overview)
  :config
  (setq kubernetes-poll-frequency (* 60 60)))

(use-package deft
  :bind
  ("M-s-d" . 'deft)
  ("C-c d" . 'deft)
  :init
  (defun me/deft/after-init ()
    (setq deft-directory (concat me/github "/perso/deft")))
  :config
  (setq deft-extensions '("org" "txt"))
  (setq deft-default-extension "org")
  (setq deft-use-filter-string-for-filename t)
  ;; needs secret stuff
  (add-hook 'after-init-hook 'me/deft/after-init t))

(use-package guru-mode
  :diminish (guru-mode . "")
  :config (guru-global-mode -1))

(use-package eshell
  :defer t
  :straight nil
  :commands 'eshell
  :init
  (require 'esh-mode)
  :config
  (setq eshell-history-size 1024)
  (setq eshell-hist-ignoredups t)
  (setq eshell-input-filter 'me/eshell-input-filter)
  (add-to-list 'eshell-modules-list 'eshell-tramp)
  (require 'em-term)
  (add-to-list 'eshell-visual-commands "powertop")
  (add-to-list 'eshell-visual-commands "run.sh")
  (add-hook 'eshell-post-command-hook #'eshell-write-history)

  (bind-keys  :map eshell-mode-map
              ("C-c h" . me/shell-history )))

(use-package powerline
  :straight (:host github :repo "Dewdrops/powerline"
                   :branch "master")
  :config
  (powerline-reset)
  (me/powerline-theme))

(use-package emacs-surround
  :straight (:host github :repo "ganmacs/emacs-surround"
                   :branch "master")
  :bind
  ("C-c s" . 'emacs-surround)
  :config
  (add-to-list 'emacs-surround-alist '("`"   . ("`"  . "`"))))

(use-package vue-mode
  ;; https://github.com/AdamNiederer/vue-mode/issues/74
  :hook ((vue-mode . (lambda () (setq syntax-ppss-table nil))))
  :config
  (add-hook 'mmm-mode-hook
            (lambda ()
              (set-face-background 'mmm-default-submode-face nil)
              (when
                  (string-suffix-p ".vue" (buffer-file-name))
                (setq-local vue-html-tab-width 4)
                (setq-local sgml-basic-offset 4)))))

(use-package eww
  :init
  (require 'youdao-dictionary)
  :bind (:map eww-mode-map
              ("C-c f" . 'me/eww/open-page-firefox)
              ("C-c y" . 'youdao-dictionary-search-at-point-tooltip)
              ("<f12>" . 'me/cc-cedict-selection)
              ("C-<f12>" . 'me/cc-cedict-selection-hide))
  :hook (
         (eww-mode . (lambda () (visual-line-mode 1)))
         (eww-after-render . me/eww-after-render-hook))
  :config
  (setq me/eww-saved-history eww-history)
  (setq me/eww-saved-history-position eww-history-position)
  (setq eww-history-limit nil)
  (setq browse-url-browser-function 'eww-browse-url)
  (add-to-list 'savehist-additional-variables
               'eshell-history-ring))

(use-package key-chord
  :config
  (key-chord-mode 1)
  ;; general mneumonic bindings
  (key-chord-define-global "qs" 'me/Emacsd)
  (key-chord-define-global "ml" 'me/magit-status-full-screen)
  (key-chord-define-global "kl" 'me/search-word-in-file)
  (key-chord-define-global "yu" 'counsel-yank-pop)
  (key-chord-define-global "lk" 'me/just-occur)
  (key-chord-define-global "è_" 'undo-tree-undo)
  (key-chord-define-global "cf" 'counsel-projectile-find-file)
  ;; (key-chord-define-global "cc" "\C-c\C-c")
  ;; (key-chord-define-global "ck" "\C-c\C-k")
  ;; C-x <something> translates to x<something>
  (key-chord-define-global "o*" 'me/switch-to-last-buffer)
  (key-chord-define-global "xb" 'ivy-switch-buffer)
  (key-chord-define-global "x&" 'delete-other-windows)
  (key-chord-define-global "xà" 'delete-window)
  (key-chord-define-global "xo" 'other-window)
  (key-chord-define-global "xs" 'save-buffer)
  (key-chord-define-global "xf" 'counsel-find-file)
  (key-chord-define-global "xk" 'kill-buffer)
  ;; mode specific bindingings, just a telling char, and its closest key
  (key-chord-define emacs-lisp-mode-map "xe" 'eval-defun)
  (key-chord-define emacs-lisp-mode-map "jk" 'kill-sexp)

  ;; general configuration
  (setq key-chord-two-keys-delay 0.05))

(use-package aggressive-indent
  :diminish " 🄰"
  :config
  (global-aggressive-indent-mode 1))

(use-package js
  :straight nil
  :defer t
  :init
  (defun me/setup-js ()
    "Js mode initialization code."
    (setq js-indent-level 2))
  :hook (js-mode . me/setup-js))

(use-package shr-color
  :straight nil
  :config
  (setq shr-color-visible-luminance-min 70))

(use-package tide
  :bind (:map tide-mode-map
              ("M-?" . 'tide-references)
              ("M-." . 'tide-jump-to-definition))
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(use-package web-mode
  :mode (("\\.html?" . web-mode)
         ("\\.php" . web-mode)
         ("\\.tsx" . web-mode)
         ("\\.jsx" . web-mode))
  :config
  (setq web-mode-enable-current-element-highlight t)
  (flycheck-add-mode 'typescript-tslint 'web-mode))

(use-package flycheck
  :diminish ""
  :config
  (global-flycheck-mode))

(use-package avy
  :bind
  ("C-c j j" . 'avy-goto-char-timer)
  ("M-s-j" . 'avy-goto-char-timer))

(use-package anzu
  :diminish (anzu-mode . "")
  :config
  (global-anzu-mode +1)
  (global-set-key [remap query-replace] 'anzu-query-replace)
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp))

(use-package ivy
  :diminish ""
  :bind
  ("C-x b" . 'ivy-switch-buffer)
  ("M-s-b" . 'ivy-switch-buffer)
  ("C-x 4 b" . 'ivy-switch-buffer-other-window)
  :config
  (setq ivy-re-builders-alist
        '((t . ivy--regex-plus)))
  (when (featurep 'magit)
    (setq magit-completing-read-function 'ivy-completing-read))
  (when (featurep 'projectile)
    (setq projectile-completion-system 'ivy)))

(use-package counsel
  :bind
  ;; ("C-c f g" . 'counsel-git)
  ("M-x" . 'me/counsel-M-x)
  ("C-h a" . 'counsel-apropos)
  ("C-c c" . 'counsel-company)
  ("C-x C-f" . 'counsel-find-file)
  ("C-c y" . 'counsel-yank-pop)
  :config
  (require 'dash-docs)
  (require 'counsel-dash)
  (use-package counsel-projectile
    :config
    (counsel-projectile-mode 1))
  (defun me/counsel-M-x ()
    "Call counsel-M-x without showing ^ as initial-input."
    (interactive)
    (counsel-M-x "")))

(use-package projectile
  :straight (:host github :repo "maurelio1234/projectile"
                   :branch "master")
  :diminish (projectile-mode . "")
  :init
  (setq projectile-switch-project-action #'me/projectile-project-dashboard)
  :bind
  ("C-c p C-R" . 'projectile-discover-projects-in-search-path)
  (:map projectile-mode-map ("C-c p" . 'projectile-command-map))
  :config
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/github/"
                                         "~/bitbucket/"))
  (projectile-add-known-project "~/exercism"))

(use-package paredit
  :diminish " 🄿"
  :hook
  (emacs-lisp-mode . paredit-mode))

(use-package nginx-mode :mode "\\nginx.*.confg'")

(use-package elpy
  :init
  (setq elpy-rpc-python-command "python3")
  :defer
  :config
  (use-package flycheck
    :init
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    :config
    (add-hook 'elpy-mode-hook 'flycheck-mode)))

(use-package terraform-mode
  :hook (terraform-mode . terraform-format-on-save-mode))

(use-package magit
  :init
  (require 'transient)
  :hook (git-commit-setup . me/magit-commit-setup)
  :bind
  ("C-c g" . 'me/magit-status-full-screen)
  (:map magit-process-mode-map ("C-c f" . 'browse-url-firefox)))

(when
    (and (display-graphic-p) (not me/in-docker-p))
  (use-package pretty-mode
    :config (global-pretty-mode t)))

(use-package csharp-mode
  :mode "\\.cake\\'"
  :bind
  (:map csharp-mode-map
        ("C-c t f" . 'me/nunit-add-category-testfixture)
        ("C-c t c" . 'me/nunit-add-category-testcase)
        ("C-c t 2 m" . 'me/nunit2-run-tests-me)
        ("C-c t 3 m" . 'me/nunit3-run-tests-me)
        ("C-c t 2 a" . 'me/nunit2-run-tests-all)
        ("C-c t 3 a" . 'me/nunit3-run-tests-all)
        ("C-c t m" . 'me/nunit3-run-tests-me)
        ("C-c t a" . 'me/nunit3-run-tests-all)
        ("C-c b p" . 'me/csharp-build-csproj)
        ("C-c b s" . 'me/csharp-build-sln))
  :hook
  (csharp-mode . me/fix-csharp-mode)
  :config
  (setq buffer-save-without-query t)
  (use-package omnisharp
    :after company
    :hook
    (csharp-mode . omnisharp-mode)
    ;; from https://github.com/OmniSharp/omnisharp-emacs/issues/431
    :bind (:map omnisharp-mode-map
                ([remap xref-find-definitions] . omnisharp-go-to-definition)
                ([remap xref-find-references] . omnisharp-find-usages)
                ([remap xref-find-definitions-other-window] . omnisharp-go-to-definition-other-window))
    :config
    (add-to-list 'company-backends 'company-omnisharp)))

(use-package typescript-mode
  :mode ("\\.jsx\\'" "\\.tsx\\'")
  :config
  (setq buffer-save-without-query t))

(use-package restclient
  :mode ("\\.rest\\'" . restclient-mode))

(use-package ace-window
  :bind
  ("C-c w w" . 'ace-window)
  ("M-s-(" . 'ace-window)
  :config
  (setq aw-keys '(?q ?s ?d ?f ?g ?h ?j ?k ?l ?m)))

(use-package which-key
  :diminish ""
  :init (which-key-mode)
  :bind
  ("C-c w m" . 'which-key-show-major-mode)
  ("C-c w t" . 'which-key-show-top-level)
  :config
  (which-key-add-key-based-replacements "C-c w" "Windows/WhichKey")
  (which-key-add-key-based-replacements "C-c v" "Vim")
  (which-key-add-key-based-replacements "C-c E" "Emacs")
  (which-key-add-key-based-replacements "C-c j" "Avy jump")
  (which-key-add-key-based-replacements "C-c b" "Build")
  (which-key-add-key-based-replacements "C-c m" "Multiple cursors")
  (which-key-add-key-based-replacements "C-c y" "Yasnippets")
  (which-key-add-key-based-replacements "C-c 5" "Frames")
  (setq which-key-idle-delay 0.1
        which-key-frame-max-height 50
        which-key-is-verbose t
        which-key-side-window-max-height 0.4
        which-key-sort-order 'which-key-description-order))

(use-package company
  :defer nil
  :diminish ""
  :init
  (setq company-dabbrev-downcase nil)
  :config

  (global-company-mode 1)
  (setq company-idle-delay 0.1)
  (use-package company-jedi
    :mode
    ("\\.py\\'" . python-mode)
    :config
    (add-to-list 'company-backends 'company-omnisharp))
  (use-package helm-company
    :after (helm)
    :bind (:map company-mode-map ("C-:" . 'helm-company)
                :map company-active-map ("C-:" . 'helm-company)))
  (use-package company-box
    :diminish ""
    :disabled t
    :hook (company-mode . company-box-mode)))

(use-package jq-mode
  :bind
  ("C-c C-j" . 'jq-interactively))

(use-package helpful
  :bind
  ("C-h f" . 'helpful-callable)
  ("C-h v" . 'helpful-variable)
  ("C-h k" . 'helpful-key))

(use-package multiple-cursors
  :init
  (require 'phi-search)
  :bind
  ("C-c m e" . 'mc/edit-lines)
  ("C-c m n" . 'mc/markmark-next-like-this)
  ("C-c m p" . 'mc/mark-previous-like-this)
  ("C-c m m" . 'mc/mark-more-like-this-extended)
  ("C-c m w" . 'mc/mark-all-words-like-this)
  :config
  (multiple-cursors-mode))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "markdown"))

(use-package yaml-mode
  :defer t
  :straight (:host github :repo "HParker/yaml-mode"
                   :branch "master")
  :mode (("\\.yml\\'" . yaml-mode)
         ("\\.tpl\\'" . yaml-mode)))

(message "init.el successfully loaded!")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:height 173 :foundry "GOOG" :family "JetBrains Mono"))))
 '(internal-border ((t (:background "dark orange"))))
 '(mode-line ((t (:background "#22242b" :foreground "#ededed" :height 0.7)))))


(provide 'init)

;;; init.el ends here
(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#2b2a27" "#ff5d38" "#98be65" "#bcd42a" "#51afef" "#c678dd" "#46D9FF" "#ede0ce"])
 '(counsel-projectile-action
   '(1
     ("o" counsel-projectile-action "current window")
     ("j" counsel-projectile-action-other-window "other window")
     ("k" counsel-projectile-action-kill-delete "kill buffer / delete-file")
     ("x" counsel-projectile-action-file-extern "open file externally")
     ("r" counsel-projectile-action-file-root "open file as root")
     ("m" counsel-projectile-action-find-file-manually "find file manually")
     ("p" counsel-projectile-action-switch-project "switch project")))
 '(counsel-projectile-mode t nil (counsel-projectile))
 '(counsel-projectile-switch-project-action
   '(4
     ("o" counsel-projectile-switch-project-action "jump to a project buffer or file")
     ("f" counsel-projectile-switch-project-action-find-file "jump to a project file")
     ("d" counsel-projectile-switch-project-action-find-dir "jump to a project directory")
     ("D" counsel-projectile-switch-project-action-dired "open project in dired")
     ("b" counsel-projectile-switch-project-action-switch-to-buffer "jump to a project buffer")
     ("m" counsel-projectile-switch-project-action-find-file-manually "find file manually from project root")
     ("S" counsel-projectile-switch-project-action-save-all-buffers "save all project buffers")
     ("k" counsel-projectile-switch-project-action-kill-buffers "kill all project buffers")
     ("K" counsel-projectile-switch-project-action-remove-known-project "remove project from known projects")
     ("c" counsel-projectile-switch-project-action-compile "run project compilation command")
     ("C" counsel-projectile-switch-project-action-configure "run project configure command")
     ("E" counsel-projectile-switch-project-action-edit-dir-locals "edit project dir-locals")
     ("v" counsel-projectile-switch-project-action-vc "open project in vc-dir / magit / monky")
     ("sg" counsel-projectile-switch-project-action-grep "search project with grep")
     ("si" counsel-projectile-switch-project-action-git-grep "search project with git grep")
     ("ss" counsel-projectile-switch-project-action-ag "search project with ag")
     ("sr" counsel-projectile-switch-project-action-rg "search project with rg")
     ("xs" counsel-projectile-switch-project-action-run-shell "invoke shell from project root")
     ("xe" counsel-projectile-switch-project-action-run-eshell "invoke eshell from project root")
     ("xt" counsel-projectile-switch-project-action-run-term "invoke term from project root")
     ("xv" counsel-projectile-switch-project-action-run-vterm "invoke vterm from project root")
     ("Oc" counsel-projectile-switch-project-action-org-capture "capture into project")
     ("Oa" counsel-projectile-switch-project-action-org-agenda "open project agenda")))
 '(custom-enabled-themes '(doom-opera-light))
 '(custom-safe-themes
   '("93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "6cbf6003e137485fb3f904e76fb15bc48abc386540f43f54e2a47a9884e679f6" "a03b86edca4a901e1576ddef72332d47dccec66dd487bb900a4f85ef5b8c8b68" "a4fa3280ffa1f2083c5d4dab44a7207f3f7bcb76e720d304bd3bd640f37b4bef" "c6b93ff250f8546c7ad0838534d46e616a374d5cb86663a9ad0807fd0aeb1d16" "1ca1f43ca32d30b05980e01fa60c107b02240226ac486f41f9b790899f6f6e67" "5091eadbb87fa0a168a65f2c3e579d1a648d764f12ab9d3ab7bdefca709cd2a5" "bc99493670a29023f99e88054c9b8676332dda83a37adb583d6f1e4c13be62b8" "e47c0abe03e0484ddadf2ae57d32b0f29f0b2ddfe7ec810bd6d558765d9a6a6c" "b60f08ddc98a95485ec19f046a81d5877b26ab80a67782ea5b91a00ea4f52170" "5e0b63e0373472b2e1cf1ebcc27058a683166ab544ef701a6e7f2a9f33a23726" "8c75e2bdf8d1293c77a752dd210612cfb99334f7edd360a42a58a8497a078b35" "5c9a906b076fe3e829d030a404066d7949e2c6c89fc4a9b7f48c054333519ee7" "41039913efab185af1ec1b13ff4df36d6941994d5e3dee39791f30fcd94b42be" "669e05b25859b9e5b6b9809aa513d76dd35bf21c0f16d8cbb80fb0727dc8f842" "32fd809c28baa5813b6ca639e736946579159098d7768af6c68d78ffa32063f4" "a9c619535d63719a15f22e3c450a03062d3fed1e356ef96d33015849c4c43946" "fe76f3d5094967034192f6a505085db8db6deb0e135749d9a54dc488d6d3ee2f" "0fe9f7a04e7a00ad99ecacc875c8ccb4153204e29d3e57e9669691e6ed8340ce" "423435c7b0e6c0942f16519fa9e17793da940184a50201a4d932eafe4c94c92d" "4a9f595fbffd36fe51d5dd3475860ae8c17447272cf35eb31a00f9595c706050" "e7666261f46e2f4f42fd1f9aa1875bdb81d17cc7a121533cad3e0d724f12faf2" "dc677c8ebead5c0d6a7ac8a5b109ad57f42e0fe406e4626510e638d36bcc42df" "15ba8081651869ec689c9004288bed79003de5b4ee9c51a9d4a208d9e3439706" "eb94e44599a45c495ad472ad551a40b87cbc4bae9631211e7a78d72b102c61b1" "dd854be6626a4243375fd290fec71ed4befe90f1186eb5b485a9266011e15b29" "de43de35da390617a5b3e39b6b27c107cc51271eb95cceb1f43d13d9647c911d" "2d392972cbe692ee4ac61dc79907af65051450caf690a8c4d36eb40c1857ba7d" "77b3ec58197612649f42edc55057499454554423b52bcca011fed9585a71d414" "75d3dde259ce79660bac8e9e237b55674b910b470f313cdf4b019230d01a982a" "100e7c5956d7bb3fd0eebff57fde6de8f3b9fafa056a2519f169f85199cc1c96" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "32f343de7f559e0fd36d6937b322d9b283faafec7ed8cc2de65ffc164d0fbf63" "8aca557e9a17174d8f847fb02870cb2bb67f3b6e808e46c0e54a44e3e18e1020" "d1b4990bd599f5e2186c3f75769a2c5334063e9e541e37514942c27975700370" "4697a2d4afca3f5ed4fdf5f715e36a6cac5c6154e105f3596b44a4874ae52c45" "93a0885d5f46d2aeac12bf6be1754faa7d5e28b27926b8aa812840fe7d0b7983" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "6b289bab28a7e511f9c54496be647dc60f5bd8f9917c9495978762b99d8c96a0" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "7e78a1030293619094ea6ae80a7579a562068087080e01c2b8b503b27900165c" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "fe666e5ac37c2dfcf80074e88b9252c71a22b6f5d2f566df9a7aa4f9bea55ef8" "49ec957b508c7d64708b40b0273697a84d3fee4f15dd9fc4a9588016adee3dad" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "151bde695af0b0e69c3846500f58d9a0ca8cb2d447da68d7fbf4154dcf818ebc" default))
 '(display-battery-mode t)
 '(electric-indent-mode nil)
 '(electric-pair-mode t)
 '(fci-rule-color "#5B6268")
 '(ivy-mode t)
 '(jdee-db-active-breakpoint-face-colors (cons "#2b2a27" "#ff5d38"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#2b2a27" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#2b2a27" "#3f444a"))
 '(line-number-mode nil)
 '(linum-format " %5i ")
 '(objed-cursor-color "#99324b")
 '(org-agenda-files
   '("~/bitbucket/work/todo.org" "~/gcal.org" "~/github/perso/emacs.org" "~/github/perso/deft" "/home/marcos/github/perso/deft/exercism maintainer elisp.org"))
 '(org-catch-invisible-edits 'error)
 '(org-fontify-whole-heading-line t)
 '(org-log-done 'time)
 '(org-startup-with-inline-images t)
 '(org-todo-keywords '((sequence "TODO" "DOING" "|" "CANCELLED" "DONE")))
 '(package-selected-packages
   '(helpful tide 0blayout web-mode helm-rg ripgrep disk-usage anzu focus tt-mode keyfreq smartparens real-auto-save ace-window avy expand-region omnisharp yasnippet-snippets which-key w3m w3 vimrc-mode use-package typescript-mode slack restclient rainbow-delimiters pretty-mode paredit ox-reveal org-present org-gcal no-littering multiple-cursors md4rd markdown-mode magit load-theme-buffer-local jq-mode image+ hide-mode-line helm-projectile helm-ls-git helm-company helm-ag graphviz-dot-mode flycheck fancy-battery evil-surround engine-mode elpy dumb-jump dot-mode doom-themes doom-modeline docker diminish csharp-mode))
 '(pdf-view-midnight-colors (cons "#fafafa" "#2a2a2a"))
 '(powerline-buffer-size-suffix t)
 '(powerline-text-scale-factor 0.75)
 '(projectile-switch-project-action 'me/projectile-project-dashboard)
 '(rustic-ansi-faces
   ["#0c0a20" "#e61f44" "#a7da1e" "#ffd400" "#1ea8fc" "#ff2afc" "#42c6ff" "#f2f3f7"])
 '(tab-bar-close-button-show nil)
 '(tab-bar-mode nil)
 '(tab-bar-new-tab-choice "*scratch*")
 '(tab-bar-select-tab-modifiers '(super))
 '(tab-bar-show nil)
 '(tab-bar-tab-hints t)
 '(vc-annotate-background "#2b2a27")
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#a4c551")
    (cons 60 "#b0cc3d")
    (cons 80 "#bcd42a")
    (cons 100 "#c1a623")
    (cons 120 "#c5781c")
    (cons 140 "#cb4b16")
    (cons 160 "#c95a58")
    (cons 180 "#c7699a")
    (cons 200 "#c678dd")
    (cons 220 "#d96fa6")
    (cons 240 "#ec666f")
    (cons 260 "#ff5d38")
    (cons 280 "#cf563c")
    (cons 300 "#9f5041")
    (cons 320 "#6f4a45")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil)
 '(window-divider-default-right-width 1)
 '(window-divider-mode t))
(put 'upcase-region 'disabled nil)
