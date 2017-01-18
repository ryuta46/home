;; tabbar モード
;; tabbar モードを有効
(tabbar-mode 1)
;; tabbar の外観設定
(setq tabbar-separator '(1.5))
;; デフォルト
(set-face-attribute
 'tabbar-default nil
 :family "Monaco"
 :background "black"
 :foreground "gray72"
 :height 1.3)

;; アクティブなタブ
(set-face-attribute 
 'tabbar-selected nil
  :family "Monaco"
  :background "black"
  :foreground "skyblue1"
  :weight 'bold)


;; タブ上でマウスホイール操作無効
(tabbar-mwheel-mode -1)
;; グループ化しない
(setq tabbar-buffer-groups-function nil)
;; タブリスト関数を定義して、'*' で始まるバッファ( *Messages* など )を表示しないようにする
(defun no-system-buffer-list ()
  (remove-if
   (lambda (buffer)
     (find (aref (buffer-name buffer) 0) " *"))
   (buffer-list)))
(setq tabbar-buffer-list-function 'no-system-buffer-list)

;; 左に表示されるボタンを無効化
(dolist (btn '(tabbar-buffer-home-button
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
  (set btn (cons (cons "" nil)
                 (cons "" nil))))

;; 次のタブ、前のタブをキーに割り当てる
(global-set-key (kbd "C-x n") 'tabbar-forward-tab)
(global-set-key (kbd "C-\>") 'tabbar-forward-tab)
(global-set-key (kbd "C-x p") 'tabbar-backward-tab)
(global-set-key (kbd "C-\<") 'tabbar-backward-tab)

