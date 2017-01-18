;; org-mode と PlantUML の設定

;;(require 'plantuml-mode) ; autoload で対応. plantuml-mode.el 読み込み時に Message バッファがアクティブになってしまうため.
(autoload 'plantuml-mode "plantuml-mode" "PlantUML mode" t)
(add-to-list 'auto-mode-alist '("\\.uml\\'" . plantuml-mode))


(defun plantuml-execute ()
  (interactive)
  (when (buffer-modified-p)
    (map-y-or-n-p "Save this buffer before executing PlantUML?"
                  'save-buffer (list (current-buffer))))
  (let ((code (buffer-string))
        out-file
        cmd)
    (when (string-match "^\\s-*@startuml\\s-+\\(\\S-+\\)\\s*$" code)
      (setq out-file (match-string 1 code)))
    (setq cmd (concat
               "java -jar " plantuml-java-options " "
               (shell-quote-argument plantuml-jar-path) " "
               (and out-file (concat "-t" (file-name-extension out-file))) " "
               plantuml-options " "
               (buffer-file-name)))
    (message cmd)
    (shell-command cmd)
    (message "done")))

(setq plantuml-jar-path (expand-file-name "~/.emacs.d/el-get/plantuml-mode/plantuml.jar"))
(setq plantuml-java-options "")
(setq plantuml-options "-charset UTF-8")
(setq plantuml-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-.") 'plantuml-execute)
        map))
