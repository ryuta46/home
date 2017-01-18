;; kill-ring と OS のクリップボードを共有する
(setq x-select-enable-clipboard t)

;; ツールバー非表示
(tool-bar-mode 0)

;; 起動時に最大化
(set-frame-parameter nil 'fullscreen 'maximized)

;; 起動時のタイトル画面を消す
(setq inhibit-startup-message t)
(setq initial-scratch-message nil)

;; 複数ウィンドウでの表示を禁止(tabbarと合わせて使うことで基本タブエディタのようにする)
(setq ns-pop-up-frames nil)

;; バックアップファイルと自動保存ファイルを backup ディレクトリ以下に作成
(setq backup-directory-alist
  (cons (cons ".*" (expand-file-name "~/.emacs.d/backup"))
        backup-directory-alist))

(setq auto-save-file-name-transforms
  `((".*", (expand-file-name "~/.emacs.d/backup/") t)))

;; スクラッチバッファをログとして保存する.
(require 'scratch-log)

;; オートインデントでスペースを使う
(setq-default indent-tabs-mode nil)


;; 起動時のバッファ復元
(desktop-save-mode t)


;; バッファの再読み込み
(defun revert-buffer-no-confirm (&optional force-reverting)
  "Interactive call to revert-buffer. Ignoring the auto-save
 file and not requesting for confirmation. When the current buffer
 is modified, the command refuses to revert it, unless you specify
 the optional argument: force-reverting to true."
  (interactive "P")
  ;;(message "force-reverting value is %s" force-reverting)
  (if (or force-reverting (not (buffer-modified-p)))
      (revert-buffer :ignore-auto :noconfirm)
    (error "The buffer has been modified")))

(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)
