(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(add-to-list 'load-path (locate-user-emacs-file "el-get/el-get"))
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))


(el-get-bundle init-loader)
(el-get-bundle markdown-mode)
(el-get-bundle plantuml-mode)
(el-get-bundle quickrun)
(el-get-bundle scratch-log)
(el-get-bundle tabbar)
(el-get-bundle textile-mode)

(el-get-bundle auto-complete)
(el-get-bundle company-mode)
(el-get-bundle ac-company)
(el-get-bundle gtags)

;; (el-get-bundle helm)

;;; init-loader
(require 'init-loader)
;;; init-loader のエラーのみログに表示.
(setq init-loader-show-log-after-init 'error-only)

;;; 設定ファイルのあるフォルダを指定
(setq inits_dir (expand-file-name "~/.emacs.d/inits/"))

(init-loader-load inits_dir)

