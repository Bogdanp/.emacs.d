;; Modeline
;; ~~~~~~~~
(line-number-mode +1)
(column-number-mode +1)


;; Diminish
;; ~~~~~~~~
(defconst my-diminished-minor-modes
  '(eldoc-mode
    git-gutter-mode
    magit-auto-revert-mode
    undo-tree-mode
    yas-minor-mode)
  "A list of minor modes to hide from the modeline.")

(use-package diminish
  :ensure t
  :config
  (progn
    (mapc #'diminish my-diminished-minor-modes)))


(provide 'init-modeline)
