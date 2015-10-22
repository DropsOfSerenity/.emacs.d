(require 'dash)

;; change command to meta, and ignore option to use weird Norwegian keyboard
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)
(setq ns-function-modifier 'hyper)

;; mac friendly font
(when window-system
  (setq magnars/default-font "-apple-Monaco-medium-normal-normal-*-14-*-*-*-m-0-iso10646-1")
  (setq magnars/presentation-font "-apple-Monaco-medium-normal-normal-*-21-*-*-*-m-0-iso10646-1")
  (set-face-attribute 'default nil :font magnars/default-font))

;; keybinding to toggle full screen mode
(global-set-key (quote [f10]) (quote toggle-frame-fullscreen))

;; Don't open files from the workspace in a new frame
(setq ns-pop-up-frames nil)

(provide 'mac)
