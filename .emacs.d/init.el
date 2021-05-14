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
(defun me/start-language-server ()
  "Start the appropriate language server on the closest solution/project."
  (interactive)
  (omnisharp--do-server-start (file-name-directory (expand-file-name (me/csharp-find-sln)))))

(defun me/k9s ()
  "Run K9s."
  (interactive)
  (me/vterm-shell-command "."  "docker run --rm -it -v ~/.kube/config:/root/.kube/config quay.io/derailed/k9s" "*K9S*"))

(defun me/vterm-shell-command (path command buffer-name)
  "Run vterm with COMMAND on a given PATH and BUFFER-NAME."
  (cd path)
  (vterm buffer-name)
  (with-current-buffer buffer-name
    (vterm-send-string (concat "reset && " command))
    (vterm-send-return)))


(defun me/async-shell-command ()
  (interactive)
  (kill-new (completing-read
             "Command: "
             shell-command-history))
  (call-interactively 'async-shell-command))

(defun me/ansi-translate-current-buffer ()
  "Translate ANSI codes in current buffer."
  (interactive)
  (let ((min (if (region-active-p)
                 (region-beginning)
               (point-min)))
        (max (if (region-active-p)
                 (region-end)
               (point-max)))
        (inhibit-read-only t))
    (ansi-color-apply-on-region min max)))

(defun me/text-mode-hook ()
  "Hook to run on text-mode."
  (interactive)
  (when
      (equal
       0
       (string-match
        "^build_.*step.*container.*txt"
        (buffer-name)))
    (circleci-build-mode 1)))

;;; Browsh support
(defun me/browsh-copy-page-url ()
  "Copy the URL of the current page into the kill ring removing browsh prefix."
  (interactive)
  (let* ((url (eww-copy-page-url))
         (browsh-url "http://localhost:4333/"))
    (when (s-starts-with-p browsh-url url)
      (kill-new (substring url (length browsh-url))))))

(defun me/browsh-init ()
  "Start the Browsh Process."
  (async-shell-command "browsh --http-server-mode"))

(defun me/browsh (url)
  "Start the Browsh Process."
  (interactive "sURL: ")
  (eww (concat
        (when current-prefix-arg
          "http://localhost:4333/") url)))

(define-derived-mode me/slack-alerts-mode special-mode "Slack Alerts"
  "Alerts from Slack")

;;; Slack alerts major mode
(defun me/switch-to-alerts-buffer ()
  "Switch to alerts buffer."
  (interactive)
  (switch-to-buffer "* Slack Alerts *"))

(defun me/init-alerts-buffer ()
  "Create and initialize the slack alerts buffer."
  (with-current-buffer (get-buffer-create "* Slack Alerts *")
    (me/slack-alerts-mode)))

(defun me/slack-message-custom-nofifier (message room team)
  "Better handling of slack MESSAGEs TEAM's ROOM."
  (let ((team-name (oref team name))
        (room-name (slack-room-name room team))
        (text (with-temp-buffer
                (goto-char (point-min))
                (insert (slack-message-to-alert message team))
                (slack-buffer-buttonize-link)
                (buffer-substring-no-properties (point-min)
                                                (point-max))))
        (user-name (slack-message-sender-name message team))
        (timestamp (format-time-string
                    "%H:%M"
                    (slack-message-time-stamp message))))
    (with-current-buffer (get-buffer-create "* Slack Alerts *")
      (read-only-mode -1)
      (goto-char (point-max))
      (insert (format "\nTeam: %s\nRoom: %s\nTimestamp: %s\nUser: %s\n\n%s\n\n"
                      team-name room-name timestamp user-name text))
      (read-only-mode 1))))

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
         (issue-branch? (equal
                         (let ((case-fold-search nil))
                           (string-match-p "[A-Z0-9]+-[0-9]+" branch-name))
                         0))
         (issue-code (when issue-branch?
                       (let ((branch-name    (magit-get-current-branch)))
                         (string-match "[A-Z0-9]+-[0-9]+" branch-name)
                         (match-string 0 branch-name))) )
         (staged-files   (magit-staged-files))
         (staged-files-common-prefix (s-chop-suffixes '("src/" "/") (cl-reduce 's-shared-start staged-files)))
         (staged-file    (car staged-files))
         (staged-filename (f-filename staged-file))
         (staged-dirname  (f-filename (f-parent staged-file)))
         (category-name   (if (string-equal "values.yaml" staged-filename)
                              staged-dirname
                            staged-filename))
         (category       (if (= 1 (length staged-files))
                             (concat category-name ": ")
                           (concat staged-files-common-prefix ": ")))
         (branch-prefix  (if
                             (or master-branch? (not issue-branch?))
                             ""
                           (concat (if issue-branch? issue-code branch-name) ": "))))
    (unless (or merge-commit?
               revert-commit?
               amend-commit?)
      (insert branch-prefix category))))

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
  (when (display-graphic-p)
    (let* ((fullscreen (frame-parameter frame 'fullscreen)))
      (modify-frame-parameters frame
                               `((fullscreen . fullboth)
                                 (fullscreen-restore . ,fullscreen)))
      (when (featurep 'cocoa) (sleep-for 0.5)))))

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

(defun me/magit-status-full-screen ()
  "Full screen magit status."
  (interactive)
  (message "Loading git status...")
  (magit-status)
  (delete-other-windows)
  (message ""))

(defun me/make-password ()
  "Generate password using OpenSSL."
  (interactive)
  (let ((password (shell-command-to-string "openssl rand -base64 16")))
    (kill-new password)
    (message "New password generated and added to kill ring!")))

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
   '(custom-enabled-themes (quote (tsdh-dark))))
  (setq frame-background-mode 'dark)
  (tab-bar-mode -1))

(defun me/light-mode ()
  "Light mode theme."
  (interactive)
  (custom-set-variables
   '(custom-enabled-themes (quote (adwaita))))
  (setq frame-background-mode 'light))

(defun me/light-office-mode ()
  "Light mode theme when working on the office."
  (interactive)
  (custom-set-variables
   '(custom-enabled-themes (quote (leuven))))
  (setq frame-background-mode 'light))

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
  (xref-push-marker-stack)
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
     (ivy-mode (counsel-git-grep initial-text))
     (t (vc-git-grep initial-text)))
    (subword-mode (if subword-mode-enabled 1 -1))))

(defun me/search-word-in-file ()
  "Search current word in current file."
  (interactive)
  (xref-push-marker-stack)
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
     (ivy-mode (counsel-grep-or-swiper initial-text))
     (t (occur initial-text)))
    (subword-mode (if subword-mode-enabled 1 -1))))

(defun me/just-occur ()
  "Activate occur in current file."
  (interactive)
  (xref-push-marker-stack)
  (cond
   (ivy-mode (counsel-grep-or-swiper))
   (t (occur))))

(defun me/kill-ring-save-base64-decode ()
  "Copy the selected text on the kill ring after decoding it from base64."
  (interactive)
  (when (use-region-p)
    (let* ((selected-text (buffer-substring-no-properties
                           (region-beginning)
                           (region-end)))
           (decoded-text (base64-decode-string selected-text)))
      (kill-new decoded-text))))

;;; Hooks
(defun me/emacs-startup-hook ()
  "Things to do on startup."
  (unless me/bootstraping-p
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
  (display-fill-column-indicator-mode 1)
  ;; (when (display-graphic-p)
  ;;   (whitespace-mode 1))
  (setq-local show-trailing-whitespace t)
  (setq-local show-leading-whitespace t)
  (setq-local indicate-empty-lines t)
  (when (eq major-mode 'java-mode)
    (turn-off-pretty-mode))
  (when (or
         (s-ends-with-p ".cake" (buffer-file-name))
         ;; (s-ends-with-p ".cs" (buffer-file-name))
         (s-ends-with-p "/data.json" (buffer-file-name))
         (string-suffix-p ".vue" (buffer-file-name))
         (and
          (string-suffix-p ".tsx" (buffer-file-name))
          (not (string-suffix-p "test.tsx" (buffer-file-name)))))
    (aggressive-indent-mode -1))
  (unless (string-suffix-p ".tsx" (buffer-file-name))
    (add-hook 'before-save-hook 'whitespace-cleanup nil 'make-it-local)))

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
                "./tools/NUnit.ConsoleRunner.3.11.1/tools/nunit3-console.exe"))
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

(defun me/pretty-quick ()
  "Prettifies current node project."
  (interactive)
  (let* ((root (locate-dominating-file (buffer-file-name nil) "package.json"))
         (cmd (concat "cd " root " && npx pretty-quick")))
    (save-window-excursion
      (shell-command cmd))))

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
  (if current-prefix-arg
      (async-shell-command (concat
                            "dotnet build "
                            (me/csharp-find-csproj)))
    (async-shell-command
     (concat
      "msbuild /p:buildmode=debug /p:PreBuildEvent= /p:PostBuildEvent= "
      (me/csharp-find-csproj)))))

(defun me/csharp-build-sln ()
  "Build current sln."
  (interactive)
  (if current-prefix-arg
      (async-shell-command (concat
                            "dotnet build "
                            (me/csharp-find-sln)))
    (async-shell-command
     (concat
      "msbuild /p:buildmode=debug /p:PreBuildEvent= /p:PostBuildEvent= "
      (me/csharp-find-sln)))))

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
  (if current-prefix-arg
      (async-shell-command (concat
                            "dotnet test --filter \"TestCategory!=Integration\" "
                            (me/csharp-find-sln)))
    (magit-shell-command-topdir (concat (me/nunit-cmd 3 nil (me/nunit-find-test-dll))))))

(defun me/nunit3-run-tests-me ()
  "Run TestMe NUnit3."
  (interactive)
  (if current-prefix-arg
      (async-shell-command (concat
                            "dotnet test --filter \"TestCategory=TestMe\" "
                            (me/csharp-find-sln)))
    (magit-shell-command-topdir (concat (me/nunit-cmd 3 "TestMe" (me/nunit-find-test-dll))))))

(defun me/fix-csharp-mode ()
  "Fixes some variables in cc-mode that break csharp mode."
  (interactive)
  ;; this commit seems to have introduced an incomcompatibility between csharp and c modes
  ;; https://github.com/emacs-mirror/emacs/commit/b61b3c804cb7c1348097ccef18d26fa4bdd37117
  (setq-local c-block-stmt-hangon-key "")
  (setq-local c-block-stmt-2-key "")
  (setq-local c-block-stmt-1-key "")
  (setq-local c-block-stmt-1-2-key ""))

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

;;; CC-CEDIT Popup Dictionary
(defun me/cedict-entries (chinese)
  "Return entries that match the word CHINESE."
  (unless cc-cedict-cache
    (setq cc-cedict-cache (cc-cedict-parse)))
  (seq-filter
   (lambda (entry) (or (string= chinese (cc-cedict-entry-traditional entry))
                       (string= chinese (cc-cedict-entry-simplified entry))))
   cc-cedict-cache))

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
  ("M-s-&" . 'me/async-shell-command)
  ("C-z" . nil) ; disables C-z
  ("C-c 5 b" . 'select-frame-by-name)
  ("C-c D" . 'cd)
  ("C-c E m" . 'me/Emacsd)
  ("C-c E d" . 'me/dark-mode)
  ("C-c E l" . 'me/light-mode)
  ("C-c E o" . 'me/light-office-mode)
  ("C-c R" . 'me/revert-buffer-noconfirm)
  ("C-c p p" . 'bookmark-jump)
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
  ("C-x é" . 'split-window)               ; comme C-x 2 mais on se passe de MAJ
  ("C-x \"" . 'split-window-horizontally) ;C-x 3
  ("C-x &" . 'delete-other-windows)       ;C-x 1
  ("C-x à" . 'delete-window)              ;C-x 0
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
  (text-mode . me/text-mode-hook)
  :custom
  (savehist-save-minibuffer-history 1)
  (savehist-file "/home/marcos/github/perso/emacs/savehist.el")
  (bookmark-default-file "/home/marcos/github/perso/emacs/bookmarks.bmk")
  (bookmark-save-flag 1)
  (calendar-date-style 'european)
  (calendar-mark-holidays-flag t)
  (create-lockfiles nil)
  (savehist-additional-variables
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
  (battery-mode-line-format " %b %rr %p%% %t %rW")

  (echo-keystrokes 0.1)
  (python-shell-interpreter "python3")
  (register-preview-delay 0.2)
  (compilation-scroll-output t)
  (display-time-day-and-date nil)
  (display-time-24hr-format t)
  (vc-follow-symlinks t)
  (make-backup-files nil)
  (ring-bell-function 'ignore)
  (revert-without-query (quote (".*")))
  (set-mark-command-repeat-pop t)
  (history-delete-duplicates t)
  (use-dialog-box nil)
  (async-shell-command-buffer 'rename-buffer)
  (shell-command-prompt-show-cwd t)
  (shell-command-dont-erase-buffer 'beg-last-out)
  (vterm-shell "/bin/bash")
  :hook
  (prog-mode . me/prog-mode-hook)
  ;; (after-init . me/frame-fullscreen)
  (after-init . me/browsh-init)
  (emacs-startup . me/emacs-startup-hook)
  (shell-mode . me/shell-hook)
  :config
  ;; Settings
  ;; From https://wikemacs.org/wiki/French
  (setq calendar-holidays '((holiday-fixed 1 1 "Jour de l'an")
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
                             "Fête des mères")))


  (setq-default indicate-empty-lines t)
  (defalias 'yes-or-no-p 'y-or-n-p)

  (add-to-list 'default-frame-alist '(font . "JetBrains Mono" )) ; Noto Mono
  (set-face-attribute 'default t :font "JetBrains Mono")

  (setenv "PATH"
          (concat "/home/" (user-login-name) "/bin" ":" (getenv "PATH")))

  (setenv "WORK" "~/bitbucket/work")

  (setenv
   "DOTNET_SKIP_FIRST_TIME_EXPERIENCE"
   "true")                                        ; so that dotnet stops printing obnoxious messages

  (setenv
   "DOTNET_CLI_TELEMETRY_OPTOUT"
   "true")

  (setenv "GIT_PAGER" "cat") ; so that I can use git without paging..

  ;; Modes
  (tooltip-mode  1)
  (menu-bar-mode 1)
  (tool-bar-mode 1)
  (tab-bar-mode 1)
  (display-time-mode 1)
  (display-battery-mode 1)
  (desktop-save-mode -1)
  (delete-selection-mode 1)
  (size-indication-mode)
  (auto-save-visited-mode -1)
  (column-number-mode 1)
  (savehist-mode t)
  (electric-pair-mode -1)
  (global-auto-revert-mode -1)
  (when (display-graphic-p)
    (global-hl-line-mode 1))

  (global-whitespace-mode -1)

  ;; Its off by default, it works, it's just not visible, so it's a win-win
  (tab-bar-mode -1)
  (global-so-long-mode 1)

  ;; Enable advanced commands
  (put 'narrow-to-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil))

(use-package cc-cedict
  :config
  (setq
   cc-cedict-file
   "~/Downloads/cedict_1_0_ts_utf-8_mdbg/cedict_ts.u8"))

(use-package org
  :straight nil
  :ensure nil
  :defer t
  :init
  (defun me/setup-org ()
    "Customizations to be activated on Org mode."
    (display-fill-column-indicator-mode 1)
    (visual-line-mode t)
    (org-indent-mode 1))
  :bind
  ("C-c a t" . 'org-todo-list)
  :hook ((org-mode . me/setup-org))
  :custom
  (org-todo-keywords '((sequence "TODO" "DOING" "|" "CANCELLED" "DONE")))
  (org-startup-with-inline-images t)
  (org-log-done 'time)
  (org-catch-invisible-edits 'error)
  (org-default-notes-file (concat (concat me/github "/perso/notes.org")))
  (org-confirm-babel-evaluate nil)
  (org-reveal-reveal-js-version 4)
  (org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
  :config
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
  (require 'ox-reveal))

;; Should be loaded before every package that uses diminish
(use-package diminish :config (diminish 'eldoc-mode))

(use-package undo-tree
  :diminish ""
  :config (global-undo-tree-mode)
  (setq undo-tree-show-minibuffer-help t))

(use-package kubernetes
  :commands (kubernetes-overview)
  :custom
  (kubernetes-poll-frequency (* 60 60)))

(use-package explain-pause-mode
  :diminish ""
  :straight (:host github :repo "lastquestion/explain-pause-mode"
                   :branch "master")
  :custom
  (explain-pause-slow-too-long-ms 1000)
  :config
  (explain-pause-mode -1)) ;; only activate when you need something

(use-package wc-mode
  :hook ((org-mode . wc-mode)))

(use-package deft
  :bind
  ("M-s-d" . 'deft)
  ("C-c d" . 'deft)
  :init
  (defun me/deft/after-init ()
    (setq deft-directory (concat me/github "/perso/deft")))
  :custom
  (deft-extensions '("org" "txt"))
  :config
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

(use-package emacs-surround
  :straight (:host github :repo "ganmacs/emacs-surround"
                   :branch "master")
  :bind
  ("C-c S" . 'emacs-surround)
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
  :bind
  ("C-c e" . 'me/browsh)
  (:map eww-mode-map
        ("G" . 'me/browsh)
        ("w" . 'me/browsh-copy-page-url)
        ("C-c f" . 'me/eww/open-page-firefox)
        ("C-c y" . 'youdao-dictionary-search-at-point-tooltip)
        ("<f12>" . 'me/cc-cedict-selection)
        ("C-<f12>" . 'me/cc-cedict-selection-hide))
  :hook (
         (eww-mode . (lambda () (visual-line-mode 1)))
         (eww-after-render . me/eww-after-render-hook))
  :custom
  (eww-history-limit nil)
  (browse-url-browser-function 'eww-browse-url))

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
  (key-chord-define-global "cf" 'project-find-file)
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
  :custom
  (shr-color-visible-luminance-min 70))

(defun me/tide-before-save ()
  "Auto format for tide."
  (interactive)
  (unless (string-suffix-p ".tsx" (buffer-file-name))
    (tide-format-before-save)))

(defun me/tide-after-save ()
  "Auto format for tide."
  (interactive)
  (if (string-suffix-p ".tsx" (buffer-file-name))
      (me/pretty-quick)))

(use-package tide
  :bind (:map tide-mode-map
              ("M-?" . 'tide-references)
              ("M-." . 'tide-jump-to-definition)
              ("C-c s s" . 'tide-restart-server)
              ("C-c s r" . 'tide-rename-symbol)
              ("C-c s i" . 'tide-jump-to-implementation)
              ("C-c s a" . 'tide-refactor))
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . me/tide-before-save)
         (after-save . me/tide-after-save)))

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
  (ivy-mode 1))

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
  (defun me/counsel-M-x ()
    "Call counsel-M-x without showing ^ as initial-input."
    (interactive)
    (counsel-M-x "")))

(use-package paredit
  :diminish " 🄿"
  :hook
  (emacs-lisp-mode . paredit-mode))

(use-package nginx-mode :mode "\\nginx.*.confg'")

(use-package elpy
  :custom
  (elpy-rpc-python-command "python3")
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

(when (display-graphic-p)
  (use-package pretty-mode
    :config (global-pretty-mode t)))

(use-package eglot
  :commands (eglot eglot-ensure)
  :hook
  (python-mode . eglot-ensure)
  (csharp-mode . eglot-ensure)
  :bind
  ("C-c s s" . 'eglot-reconnect)
  ("C-c s a" . 'eglot-code-actions)
  ("C-c s i" . 'eglot-find-implementation)
  ("C-c s f f" . 'eglot-format-buffer)
  ("C-c s f F" . 'eglot-format)
  ("C-c s r" . 'eglot-rename)
  :config
  (add-to-list 'eglot-server-programs
               `(csharp-tree-sitter-mode . ("/home/marcos/.emacs.d/.cache/omnisharp/server/v1.37.5/run" "-lsp"))))

(use-package tree-sitter :ensure t)
(use-package tree-sitter-langs :ensure t)

(use-package csharp-mode
  :mode "\\.cake\\'"
  :bind
  (:map csharp-tree-sitter-mode-map
        ("C-c t f" . 'me/nunit-add-category-testfixture)
        ("C-c t c" . 'me/nunit-add-category-testcase)
        ("C-c t 2 m" . 'me/nunit2-run-tests-me)
        ("C-c t 3 m" . 'me/nunit3-run-tests-me)
        ("C-c t 2 a" . 'me/nunit2-run-tests-all)
        ("C-c t 3 a" . 'me/nunit3-run-tests-all)
        ("C-c t m" . 'me/nunit3-run-tests-me)
        ("C-c t a" . 'me/nunit3-run-tests-all)
        ("C-c b p" . 'me/csharp-build-csproj)
        ("C-c b s" . 'me/csharp-build-sln)
        ;; ("C-c s s" . 'me/start-language-server)
        ;; ("C-c s a" . 'omnisharp-run-code-action-refactoring)
        ;; ("C-c s i" . 'omnisharp-find-implementations)
        ;; ("C-c s f f" . 'omnisharp-code-format-entire-file)
        ;; ("C-c s f F" . 'omnisharp-code-format-region)
        ;; ("C-c s r" . 'omnisharp-rename)
        )
  :hook
  (csharp-mode . me/fix-csharp-mode)
  :config
  (which-key-add-key-based-replacements "C-c t" "C# Tests")
  (which-key-add-key-based-replacements "C-c b" "C# Build")
  (which-key-add-key-based-replacements "C-c s" "C# Language Server")
  (which-key-add-key-based-replacements "C-c s f" "Auto Formatting")
  (add-to-list 'auto-mode-alist '("\\.cs\\'" . csharp-tree-sitter-mode))
  (setq buffer-save-without-query t)
  (use-package omnisharp
    :disabled
    :after company
    :custom
    (omnisharp-server-executable-path "/home/marcos/.emacs.d/.cache/omnisharp/server/v1.37.5/run")
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
  :custom
  (which-key-idle-delay 0.1)
  (which-key-frame-max-height 50)
  (which-key-is-verbose t)
  (which-key-side-window-max-height 0.4)
  (which-key-sort-order 'which-key-description-order)
  :config
  (which-key-add-key-based-replacements "C-c w" "Windows/WhichKey")
  (which-key-add-key-based-replacements "C-c S" "Slack")
  (which-key-add-key-based-replacements "C-c S m" "Messages")
  (which-key-add-key-based-replacements "C-c v" "Vim")
  (which-key-add-key-based-replacements "C-c E" "Emacs")
  (which-key-add-key-based-replacements "C-c j" "Avy jump")
  (which-key-add-key-based-replacements "C-c b" "Build")
  (which-key-add-key-based-replacements "C-c m" "Multiple cursors")
  (which-key-add-key-based-replacements "C-c y" "Yasnippets")
  (which-key-add-key-based-replacements "C-c 5" "Frames"))

(use-package company
  :defer nil
  :diminish ""
  :custom
  (company-dabbrev-downcase nil)
  (company-idle-delay 0.1)
  :config
  (global-company-mode 1)
  (use-package company-jedi
    :mode
    ("\\.py\\'" . python-mode)
    ;; :config
    ;; (add-to-list 'company-backends 'company-omnisharp)
    )
  (use-package company-box
    :diminish ""
    :disabled t
    :hook (company-mode . company-box-mode)))

(use-package jq-mode
  :bind
  ("C-c C-j" . 'jq-interactively))

(use-package slack
  :straight (:host github :repo "maurelio1234/emacs-slack"
                   :branch "master")
  :custom
  (slack-buffer-emojify t)
  (slack-prefer-current-team t)
  (slack-message-custom-notifier 'me/slack-message-custom-nofifier)
  :bind
  ("C-c S r" . 'slack-select-rooms)
  ("C-c S u" . 'slack-all-unreads)
  ("C-c S m l" . 'me/switch-to-alerts-buffer)
  ("C-c S m r" . 'slack-message-add-reaction)
  ("C-c S m e" . 'slack-message-edit)
  ("C-c S m j" . 'slack-insert-emoji)
  ("C-c S m m" . 'slack-message-embed-mention)
  ("C-c S m t" . 'slack-thread-show-or-create)
  :config
  (setq slack-modeline t)
  (me/init-alerts-buffer))

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

(use-package evil
  :bind ("<f12>" . 'evil-local-mode)
  :custom
  ;; use C-b to scroll instead
  (evil-want-C-u-scroll nil))

(message "init.el successfully loaded!")

(provide 'init)

;;; init.el ends here
