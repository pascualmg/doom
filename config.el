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

;;; ~/.doom.d/fonts-config.el -*- lexical-binding: t; -*-

(when (display-graphic-p)
  ;; Establecer un tamaño base
  (setq base-font-size 18)

  ;; Función auxiliar para verificar si una fuente está disponible
  (defun font-available-p (font-name)
    (find-font (font-spec :name font-name)))

  ;; Determinar qué fuente usar
  (setq main-font-family (cond ((font-available-p "Monoid Nerd Font Mono") "Monoid Nerd Font Mono")
                               ((font-available-p "Monoid Nerd Font") "Monoid Nerd Font")
                               (t "Monoid")))

  ;; Configuración principal de fuentes
  (setq doom-font (font-spec :family main-font-family :size base-font-size)
        doom-variable-pitch-font (font-spec :family "DejaVu Sans" :size 13)
        doom-big-font (font-spec :family main-font-family :size (* base-font-size 1.5)))

  ;; Configurar las variantes de la fuente
  (setq doom-italic-font (font-spec :family main-font-family :style "Italic" :size base-font-size)
        doom-bold-font (font-spec :family main-font-family :style "Bold" :size base-font-size))

  ;; Configuración específica para Haskell (si aún la necesitas)
  (add-hook 'haskell-mode-hook
            (lambda ()
              (setq buffer-face-mode-face `(:family ,main-font-family))
              (buffer-face-mode +1))))

;; Habilitar ligaduras
(use-package! ligature
  :config
  (ligature-set-ligatures 'prog-mode '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
                                       ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
                                       "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
                                       "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
                                       "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
                                       "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
                                       "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
                                       "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
                                       "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
                                       "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
  (global-ligature-mode t))

;; Mensaje para verificar la configuración actual de la fuente
(message "Fuente principal utilizada: %s" main-font-family)
(message "Valor actual de doom-font: %s" doom-font)
(message "Valor actual de doom-big-font: %s" doom-big-font);; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; Esto para que cuando se exporte a HTML desde org se aplique la plantilla.
(after! ox-html
  (setq org-html-postamble nil)  ; Desactiva el postamble predeterminado
  (setq org-html-head-include-default-style nil)  ; Opcional: desactiva el CSS predeterminado
  (setq org-html-head-include-scripts nil)        ; Opcional: desactiva los scripts predeterminados
  (setq org-html-htmlize-output-type 'css)        ; Usa CSS para el resaltado de sintaxis
  (setq org-html-template "~/org/template_spacemacs.html"))

;; Asegúrate de que el archivo de plantilla existe
(unless (file-exists-p "~/org/template_spacemacs.html")
  (warn "El archivo de plantilla HTML no existe: ~/org/template_spacemacs.html"))
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



;;Ellama! escupe la flama

;; Asegúrate de que ellama esté instalado
;; Añade 'ellama a tu lista de paquetes en packages.el si aún no lo has hecho

(use-package! ellama
  :config
  ;; Configura el idioma para las respuestas
  (setq ellama-language "Spanish")

  ;; Configura la ruta al binario de Ollama (ajusta según tu sistema)
  (setq ellama-ollama-binary "/ruta/a/tu/ollama")

  ;; Configura el proveedor LLM para usar tu modelo llama3.1
  (setq ellama-provider
        (make-llm-ollama :chat-model "llama3.1:latest" :embedding-model "llama3.1:latest"))

  ;; Desactiva los keybindings por defecto de Ellama
  (setq ellama-enable-keymap nil)

  ;; Configura el directorio para guardar las sesiones
  (setq ellama-sessions-directory (expand-file-name "~/ellama-sessions"))

  ;; Activa el guardado automático de sesiones
  (setq ellama-session-auto-save t)

  ;; Configura el modo principal para los buffers de Ellama
  (setq ellama-major-mode 'org-mode)

  ;; Configura el tipo de spinner (indicador de carga)
  (setq ellama-spinner-type 'progress-bar)

  ;; Función para listar modelos de Ollama
  (defun my/list-ollama-models ()
    "List available Ollama models."
    (interactive)
    (with-output-to-temp-buffer "*Ollama Models*"
      (princ "Available Ollama models:\n\n")
      (dolist (line (cdr (process-lines ellama-ollama-binary "list")))
        (princ (format "%s\n" line)))))

  ;; Función para seleccionar un modelo de Ollama interactivamente
  (defun my/select-ollama-model ()
    "Select an Ollama model interactively."
    (interactive)
    (let* ((models (mapcar (lambda (line) (car (split-string line)))
                           (cdr (process-lines ellama-ollama-binary "list"))))
           (selected-model (completing-read "Select Ollama model: " models)))
      (setq ellama-provider
            (make-llm-ollama :chat-model selected-model :embedding-model selected-model))
      (message "Selected model: %s" selected-model)))

  ;; Configuración de keybindings para Doom
  (map! :leader
        (:prefix ("A" . "AI/Ellama")
         :desc "Chat with Ellama" "c" #'ellama-chat
         :desc "Ask about" "a" #'ellama-ask-about
         :desc "Translate" "t" #'ellama-translate
         :desc "Summarize" "s" #'ellama-summarize
         :desc "Code review" "r" #'ellama-code-review
         :desc "List Ollama models" "l" #'my/list-ollama-models
         :desc "Select Ollama model" "m" #'my/select-ollama-model)))

;; Si quieres usar Ellama en lugar de gptel, puedes comentar o eliminar la configuración de gptel
;; y usar esta configuración de Ellama en su lugar.
