;; Modeline
;; ~~~~~~~~
(line-number-mode +1)
(column-number-mode +1)
(display-time-mode +1)

;; Diminish global minor modes.
(defconst my-diminished-minor-modes
  '(auto-complete-mode
    eldoc-mode
    git-gutter-mode
    undo-tree-mode
    yas-minor-mode)
  "A list of minor modes to hide from the modeline.")

(mapc #'diminish my-diminished-minor-modes)


(provide 'init-modeline)
