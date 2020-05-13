;;; package --- summary -*- lexical-binding: t; -*-
;;; Commentary:
;;; my init.el file

;;; Code:
;;;; Initialization
(require 'package)

;; Straight bootstrapper
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;; Basic dependencies
(add-to-list 'load-path "/usr/share/emacs/site-lisp")

(setq package-enable-at-startup nil)
(setq package-archives '(("org"   . "http://orgmode.org/elpa/")
                         ("gnu"   . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

(straight-use-package 'use-package)
(require 'straight)
(require 'use-package)
(setq use-package-always-ensure nil)
(setq straight-use-package-by-default t)

(straight-use-package '(org :type built-in))
(use-package youtube-dl)
(use-package jq-mode)
(use-package ob-http)
(use-package htmlize)
(use-package ox-reveal)
(use-package youdao-dictionary)
(use-package disable-mouse)
(use-package dash-docs
  :straight (:host github :repo "dash-docs-el/dash-docs"
                   :branch "master"))
(use-package counsel-dash)
(use-package counsel-projectile)
(use-package omnisharp
  :config
  (setq omnisharp-server-executable-path
        (concat
         "/home/"
         (user-login-name)
         "/.emacs.d/.cache/omnisharp/server/v1.34.5/run"))
  (omnisharp--install-server nil t))
(use-package company-jedi)
(use-package helm-company)
(use-package phi-search)
(use-package cc-cedict)
(use-package vterm
  :init (setq vterm-always-compile-module t))
(use-package diminish)
(use-package speed-type)
(use-package undo-tree)
(use-package kubernetes)
(use-package deft)
(use-package yoshi-theme)
(use-package guru-mode)
(use-package eshell)
(use-package better-defaults
  :straight (:host github :repo "technomancy/better-defaults"
                   :branch "master"))
(use-package powerline
  :straight (:host github :repo "Dewdrops/powerline"
                   :branch "master"))
(use-package emacs-surround
  :straight (:host github :repo "ganmacs/emacs-surround"
                   :branch "master"))
(use-package naysayer-theme
  :straight (:host github :repo "nickav/naysayer-theme.el"
                   :branch "master"))
(use-package vue-mode)
(use-package key-chord)
(use-package aggressive-indent)
(use-package disk-usage)
(use-package tide)
(use-package web-mode)
(use-package flycheck)
(use-package avy)
(use-package anzu)
(use-package wgrep)
(use-package ivy-posframe)
(use-package ivy)
(use-package counsel)
(use-package doom-themes)
(use-package no-littering)
(use-package projectile)
(use-package paredit)
(use-package nginx-mode)
(use-package pdf-tools
  :config
  (when
      (string-equal "true" (getenv "BOOTSTRAPING"))
    (pdf-tools-install t nil t)))
(use-package elpy)
(use-package terraform-mode)
(use-package magit)
(use-package pretty-mode)
(use-package csharp-mode)
(use-package typescript-mode)
(use-package restclient)
(use-package ace-window)
(use-package which-key)
(use-package company)
(use-package jq-mode)
(use-package helpful)
(use-package multiple-cursors)
(use-package markdown-mode)
(use-package yaml-mode
  :straight (:host github :repo "HParker/yaml-mode"
                   :branch "master"))
(use-package docker-tramp)
(use-package docker)
(use-package dockerfile-mode)

(provide 'bootstrap)
;;; bootstrap.el ends here
