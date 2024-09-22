;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
 (setq user-full-name "Pascual M.G."
       user-mail-address "info@pascualmg.dev")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 12 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 13))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;; (setq doom-theme 'doom-one)

;;
;;

(when (display-graphic-p)
  ;;set a base size
  (setq base-font-size 32)

  (setq doom-font (font-spec :family "Hack Nerd Font" :size base-font-size)
        doom-variable-pitch-font (font-spec :family "DejaVu Sans" :size 13)
        doom-big-font (font-spec :family "Hack Nerd Font" :size 18))

  ;; Configuración de respaldo si Hack no está disponible
  (unless (find-font (font-spec :name "Hack Nerd Font"))
    (setq doom-font (font-spec :family "DejaVu Sans Mono" :size 14)
          doom-big-font (font-spec :family "DejaVu Sans Mono" :size 18)))

  ;; Configuración específica para Haskell (opcional)
  (add-hook 'haskell-mode-hook
            (lambda ()
              (setq buffer-face-mode-face '(:family "DejaVu Sans Mono"))
              (buffer-face-mode +1))))

  (use-package all-the-icons)


;; Opcional: configurar una fuente específica para los símbolos si es necesario
(setq doom-symbol-font (font-spec :family "Symbola" :size 14))

;; Mensaje para verificar la configuración actual de la fuente
(message "Valor actual de doom-font: %s" doom-font)

(push '(fullscreen . maximized) initial-frame-alist)

;; Mensaje para verificar la configuración actual de la fuente
(message "Valor actual de doom-font: %s" doom-font)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;;https://www.orgroam.com/manual.html
(setq org-roam-directory (file-truename "~/org/roam"))
(org-roam-db-autosync-mode)


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Configuración de PHP con Nix
(after! php
  (setq php-mode-coding-style 'psr2) ; Estilo de codificación PSR-2

  ;; Ruta al ejecutable de PHP proporcionado por Nix
  ;;(setq php-mode-program "/nix/store/wgpvi1n58dsh10d7g087v21i2cw13ixj-php-with-extensions-8.3.3/bin/php")
  ;;(setq dap-php-debug-program '("/nix/store/wgpvi1n58dsh10d7g087v21i2cw13ixj-php-with-extensions-8.3.3/bin/php" "-dxdebug.remote_enable=1" "-dxdebug.remote_mode=req" "-dxdebug.remote_port=9000" "-dxdebug.remote_host=127.0.0.1" "-dxdebug.remote_connect_back=0")
  ;;)
)

(setq telega-server-libs-prefix "/usr/local/lib/tdlib")

(use-package! doom-themes
  :config
  (setq doom-themes-org-font "-apple-Pacifico-Regular-normal-normal-*-14-*-*-*-m-0-iso10646-1"))


;;para que se exporten en los htmls los results en los org
(setq org-export-allow-bind-keywords t)


(set-frame-parameter (selected-frame) 'alpha '(85 . 85))
(add-to-list 'default-frame-alist '(alpha . (85 . 85)))

(defun toggle-transparency ()
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (set-frame-parameter
     nil 'alpha
     (if (eql (cond ((numberp alpha) alpha)
                    ((numberp (cdr alpha)) (cdr alpha))
                    ;; También manejar casos no numéricos
                    ((listp alpha) (cadr alpha)))
              100)
         '(85 . 85) '(100 . 100)))))

;; Keybinding para alternar la transparencia
(map! :leader
      :desc "Toggle transparency"
      "t t" #'toggle-transparency)

(after! haskell-mode
  (setq haskell-interactive-popup-errors nil))
  (setq haskell-process-type 'stack-ghci)

(defun copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard"
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max))
  (message "Buffer copied to clipboard"))

(map! :leader
      :desc "Copy whole buffer to clipboard"
      "y b" #'copy-whole-buffer-to-clipboard)

   (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (haskell . t)))

(setq org-babel-haskell-compiler "stack ghc --")

(use-package gptel
  :ensure t
  :config
  (setq gptel-backend (gptel-make-ollama "Ollama"
                                         :host "localhost:11434"
                                         :stream t
                                         :models '("llama3.1:latest")))
  (setq gptel-model "llama3.1:latest")
  (setq gptel-default-mode 'org-mode)  ; O 'markdown-mode si prefieres
  (setq gptel-directives '((default . "Eres un asistente AI especializado en Emacs Doom asi que tienes que proporcionar sus atajos , Haskell, y configuración de entornos de desarrollo para programadores. Tu objetivo es ayudar a los usuarios a aprender y dominar estas tecnologías, así como a integrarlas eficientemente con XMonad. Tienes conocimientos profundos sobre: 1. Emacs Doom: configuración y personalización, atajos de teclado y comandos útiles, plugins y extensiones populares, integración con lenguajes de programación. 2. Haskell: conceptos fundamentales de programación funcional, sintaxis y estructuras de datos, bibliotecas y frameworks comunes, patrones de diseño en Haskell. 3. XMonad: configuración básica y avanzada, integración con Emacs y otros programas, gestión eficiente de ventanas y espacios de trabajo. 4. Flujo de trabajo de desarrollo: mejores prácticas para combinar Emacs Doom, Haskell y XMonad, consejos de productividad y optimización del entorno. Proporciona explicaciones claras y concisas, ofrece ejemplos prácticos cuando sea apropiado, y sugiere recursos adicionales para aprendizaje. Adapta tus respuestas al nivel de experiencia del usuario, desde principiante hasta avanzado. Estás listo para responder preguntas, ofrecer tutoriales paso a paso, y ayudar a resolver problemas específicos relacionados con estas tecnologías"))))

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))


;; Enable automatic highlighting of the symbol under cursor
(add-hook! 'prog-mode-hook #'highlight-symbol-mode)

;; Set a shorter delay before the symbol gets highlighted
(setq highlight-symbol-idle-delay 0.3)
