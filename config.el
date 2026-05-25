;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;;; Commentary:
;; Configuración personal de Doom Emacs
;; Organizada en secciones lógicas para mejor mantenimiento

;;; Code:

;; ════════════════════════════════════════════════════════════════════════════
;; CONFIGURACIÓN BÁSICA
;; ════════════════════════════════════════════════════════════════════════════

(setq user-full-name "Pascual M.G."
      user-mail-address "info@pascualmg.dev")

;; Silenciar warning de PGTK en X11 (funciona bien, solo avisa)
(setq warning-minimum-level :emergency)

;; ════════════════════════════════════════════════════════════════════════════
;; SISTEMA Y PATHS (Nix Integration)
;; ════════════════════════════════════════════════════════════════════════════

;; Configuración de PATH para Nix
(defun setup-nix-paths ()
  "Configura los paths necesarios para integración con Nix."
  (let ((nix-paths (list
                    (concat (getenv "HOME") "/.nix-profile/bin")
                    "/run/current-system/sw/bin")))
    ;; Actualizar PATH del sistema
    (setenv "PATH" (concat (string-join nix-paths ":") ":" (getenv "PATH")))
    ;; Actualizar exec-path de Emacs
    (setq exec-path (append nix-paths exec-path))))

(setup-nix-paths)

;; Función auxiliar para verificar comandos esenciales
(defun verify-essential-commands ()
  "Verifica la disponibilidad de comandos esenciales."
  (interactive)
  (let ((commands '("gzip" "git" "ripgrep" "fd" "php" "composer" "stack")))
    (dolist (cmd commands)
      (message "%s: %s" cmd
               (if (executable-find cmd)
                   (propertize "✓ FOUND" 'face 'success)
                 (propertize "✗ NOT FOUND" 'face 'error))))))

;; ════════════════════════════════════════════════════════════════════════════
;; INTERFAZ Y APARIENCIA
;; ════════════════════════════════════════════════════════════════════════════

;; --- Transparencia (compatible PGTK/Wayland y X11) ---
(defun set-frame-transparency (value)
  "Set frame transparency to VALUE (0-100).
En PGTK usa 'alpha-background, en X11 usa 'alpha."
  (if (featurep 'pgtk)
      ;; PGTK/Wayland: usa alpha-background con valor simple
      (progn
        (set-frame-parameter (selected-frame) 'alpha-background value)
        (add-to-list 'default-frame-alist `(alpha-background . ,value)))
    ;; X11: usa alpha con cons cell (active . inactive)
    (let ((alpha-value (cons value value)))
      (set-frame-parameter (selected-frame) 'alpha alpha-value)
      (add-to-list 'default-frame-alist `(alpha . ,alpha-value)))))

(set-frame-transparency 85)

(defun toggle-transparency ()
  "Toggle between transparent (85) and opaque (100)."
  (interactive)
  (let* ((param (if (featurep 'pgtk) 'alpha-background 'alpha))
         (current (frame-parameter nil param))
         (current-value (if (listp current) (car current) current)))
    (set-frame-transparency
     (if (and current-value (< current-value 100)) 100 85))))

;; --- Configuración de Fuentes ---
(require 'url)
(require 'auth-source)

(defvar my-nerd-fonts
  '("Hack Nerd Font"          ; principal (la primera disponible se elige)
    "JetBrainsMono Nerd Font"
    "VictorMono Nerd Font"    ; tambien usada para italicas (font-lock-comment)
    "FiraCode Nerd Font"
    "Iosevka Nerd Font"
    "CaskaydiaCove Nerd Font")
  "Nerd Fonts preferidas en orden de prioridad.
La primera disponible en el sistema se usa como `doom-font'.
Para anadir mas para el selector interactivo: `change-font'.")

(defvar safe-fonts
  '("Hack Nerd Font" "DejaVu Sans Mono" "Courier New" "Consolas" "Monospace")
  "Lista de fuentes seguras como fallback.")

(defun font-available-p (font-name)
  "Check if FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun find-first-available-font (font-list)
  "Find first available font from FONT-LIST."
  (cl-find-if #'font-available-p font-list))

(defun download-and-install-nerd-font (font-name)
  "Download and install FONT-NAME from Nerd Fonts releases."
  (let* ((github-font-name (replace-regexp-in-string " Nerd Font$" "" font-name))
         (font-url (format "https://github.com/ryanoasis/nerd-fonts/releases/download/v3.2.1/%s.zip"
                           github-font-name))
         (temp-file (make-temp-file "nerd-font-" nil ".zip"))
         (font-dir (expand-file-name "~/.local/share/fonts/")))
    (message "Downloading %s..." font-name)
    (url-copy-file font-url temp-file t)
    (make-directory font-dir t)
    (call-process "unzip" nil nil nil "-o" temp-file "-d" font-dir)
    (delete-file temp-file)
    (when (eq system-type 'gnu/linux)
      (call-process "fc-cache" nil nil nil "-f" "-v"))
    (message "Successfully installed %s" font-name)))

(defun select-and-install-nerd-font ()
  "Interactively select and install a Nerd Font."
  (interactive)
  (let ((chosen-font (completing-read "Choose a Nerd Font to install: " my-nerd-fonts)))
    (when (yes-or-no-p (format "Download and install %s?" chosen-font))
      (download-and-install-nerd-font chosen-font))))

(defun setup-fonts ()
  "Setup main font configuration."
  (let* ((base-font-size 18)
         (nerd-font (find-first-available-font my-nerd-fonts))
         (safe-font (find-first-available-font safe-fonts))
         (main-font-family (or nerd-font safe-font))
         (variable-pitch-family "DejaVu Sans"))
    (when main-font-family
      (setq doom-font (font-spec :family main-font-family :size base-font-size)
            doom-variable-pitch-font (font-spec :family variable-pitch-family :size base-font-size)
            doom-big-font (font-spec :family main-font-family :size (* base-font-size 1.5))
            doom-italic-font (font-spec :family "VictorMono Nerd Font" :slant 'italic :size base-font-size)
            doom-bold-font (font-spec :family main-font-family :weight 'bold :size base-font-size))
      (when (display-graphic-p)
        (set-face-attribute 'default nil :font doom-font)
        (set-face-attribute 'fixed-pitch nil :font doom-font)
        (set-face-attribute 'variable-pitch nil :font doom-variable-pitch-font))
      (message "Font configured: %s" main-font-family))))

(setup-fonts)

;; Comentarios y keywords en cursiva con Victor Mono.
;; Linea de ejecucion en dape: magenta Spacemacs, fondo solido,
;; texto blanco bold. :extend t para que ocupe toda la linea.
(custom-set-faces!
  '(font-lock-comment-face :family "VictorMono Nerd Font" :slant italic)
  '(font-lock-doc-face :family "VictorMono Nerd Font" :slant italic)
  '(font-lock-keyword-face :slant italic)
  '(dape-source-line-face
    :background "#bc6ec5" :foreground "#ffffff"
    :weight bold :extend t))

(defun change-font ()
  "Interactively change the current font."
  (interactive)
  (when (display-graphic-p)
    (let* ((chosen-font (completing-read "Choose a font: " my-nerd-fonts))
           (base-font-size 14))
      (if (font-available-p chosen-font)
          (progn
            (setq doom-font (font-spec :family chosen-font :size base-font-size)
                  doom-big-font (font-spec :family chosen-font :size (* base-font-size 1.5))
                  doom-variable-pitch-font (font-spec :family "DejaVu Sans" :size base-font-size))
            (set-face-attribute 'default nil :font doom-font)
            (set-face-attribute 'fixed-pitch nil :font doom-font)
            (set-face-attribute 'variable-pitch nil :font doom-variable-pitch-font)
            (message "Font changed to %s" chosen-font))
        (when (yes-or-no-p (format "%s not installed. Download it?" chosen-font))
          (download-and-install-nerd-font chosen-font)
          (change-font))))))

(defun reload-fonts ()
  "Reload font configuration."
  (interactive)
  (when (display-graphic-p)
    (setup-fonts)
    (doom/reload-font)))

;; --- Ligaduras ---
(when (display-graphic-p)
  (use-package! ligature
    :config
    (ligature-set-ligatures
     't '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\" "{-" "::"
          ":::" ":=" "!!" "!=" "!==" "-}" "----" "-->" "->" "->>"
          "-<" "-<<" "-~" "#{" "#[" "##" "###" "####" "#(" "#?" "#_"
          "#_(" ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*" "/**"
          "/=" "/==" "/>" "//" "///" "&&" "||" "||=" "|=" "|>" "^=" "$>"
          "++" "+++" "+>" "=:=" "==" "===" "==>" "=>" "=>>" "<="
          "=<<" "=/=" ">-" ">=" ">=>" ">>" ">>-" ">>=" ">>>" "<*"
          "<*>" "<|" "<|>" "<$" "<$>" "<!--" "<-" "<--" "<->" "<+"
          "<+>" "<=" "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<"
          "<~" "<~~" "</" "</>" "~@" "~-" "~>" "~~" "~~>" "%%"))
    (global-ligature-mode t)))

;; --- Tema ---
(use-package! doom-themes
  :config
  (setq doom-themes-org-font "-apple-Pacifico-Regular-normal-normal-*-14-*-*-*-m-0-iso10646-1"))

;; ════════════════════════════════════════════════════════════════════════════
;; ORG MODE
;; ════════════════════════════════════════════════════════════════════════════

(setq org-directory "~/org/"
      org-roam-directory "~/org/roam")

;; Configuración de exportación HTML
(with-eval-after-load 'ox-html
  (setq org-html-postamble nil
        org-html-head-include-default-style nil
        org-html-head-include-scripts nil
        org-html-doctype "html5"
        org-html-html5-fancy t
        org-html-validation-link nil
        org-html-template-default-path "/home/passh/org/template_spacemacs.html"))

;; Configuración adicional de Org
(after! org
  (setq org-startup-with-inline-images t
        org-export-allow-bind-keywords t)

  ;; Org-crypt para encriptación
  (require 'org-crypt)
  (org-crypt-use-before-save-magic)
  (setq org-tags-exclude-from-inheritance '("crypt")
        org-crypt-key "85E4C775557B92E4")

  ;; Cifra los heredia :crypt: antes de cada save de cualquier buffer org-mode.
  ;; Antes el hook estaba en kill-buffer-hook (cifraba solo al matar buffer):
  ;; el fichero podia quedar en disco con contenido en claro entre saves.
  (add-hook! 'before-save-hook
    (defun +org-encrypt-before-save ()
      (when (derived-mode-p 'org-mode)
        (org-encrypt-entries)))))

;; Org-roam
(org-roam-db-autosync-mode)

;; Org Journal
(use-package! org-journal
  :config
  (setq org-journal-dir "~/org/journal/"
        org-journal-file-type 'monthly
        org-journal-file-format "%Y%m.org"
        org-journal-date-format "%A, %d %B %Y"
        org-journal-time-format "%H:%M "
        org-journal-file-header "#+TITLE: Journal de passh %Y-%m\n"
        org-journal-carryover-items "TODO=\"TODO\"|TODO=\"IN-PROGRESS\"|TODO=\"IN-REVIEW\"|TODO=\"IN-TEST\"|TODO=\"DONE\"|TODO=\"CANCELLED\"|TODO=\"RELEASED\"|TODO=\"MERGED\"|TODO=\"DEPLOYED\"|TODO=\"CLOSED\"|TODO=\"REOPENED\"|TODO=\"REJECTED\""
        org-journal-enable-agenda-integration t
        org-journal-enable-encryption nil
        org-journal-hide-entries-p nil)

  (make-directory org-journal-dir t)
  (setq org-agenda-files (append org-agenda-files (list org-journal-dir))))

;; Función para insertar imágenes desde el portapapeles.
;; Detecta Wayland (PGTK) -> wl-paste; resto -> xclip.
(defun brutalist-clipboard-png-insert ()
  "Insert PNG image from clipboard into Org file."
  (interactive)
  (let* ((timestamp (format-time-string "%Y%m%d_%H%M%S"))
         (image-dir (expand-file-name "~/org/images/"))
         (image-name (concat timestamp ".png"))
         (image-path (expand-file-name image-name image-dir))
         (wayland-p (or (featurep 'pgtk)
                        (getenv "WAYLAND_DISPLAY")))
         (cmd (if wayland-p
                  '("wl-paste" "--type" "image/png")
                '("xclip" "-selection" "clipboard" "-t" "image/png" "-o")))
         (tool (car cmd)))

    (unless (executable-find tool)
      (error "%s no esta instalado (necesario para %s)"
             tool (if wayland-p "Wayland" "X11")))

    (unless (file-directory-p image-dir)
      (make-directory image-dir t))

    (let ((clip-output
           (with-temp-buffer
             (let ((coding-system-for-read 'binary)
                   (coding-system-for-write 'binary))
               (if (zerop (apply #'call-process (car cmd) nil t nil (cdr cmd)))
                   (progn
                     (write-region (point-min) (point-max) image-path nil 'silent)
                     (buffer-string))
                 (error "Failed to get PNG data from clipboard (via %s)" tool))))))

      (when (and clip-output (> (length clip-output) 0))
        (insert (format "[[file:%s]]" image-path))
        (message "Image inserted: %s" image-path)))))

;; ════════════════════════════════════════════════════════════════════════════
;; LENGUAJES DE PROGRAMACIÓN
;; ════════════════════════════════════════════════════════════════════════════

;; --- Babel ---
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (haskell . t)
   (php . t)
   (nix . t)))

;; --- PHP & Intelephense ---
(after! php-mode
  (setq php-mode-coding-style 'psr2))

;; Lee la licencia de Intelephense. Orden: agenix -> pass -> warning.
;; NUNCA hardcodear la key aqui (el repo es publico).
;;   1. agenix: ~/dotfiles/secrets/intelephense-license.age (declarativo,
;;      replica clone-first, descifra con ssh key del usuario).
;;   2. pass:   personal/intelephense-license (gpg, fuente activa).
;;   3. nil + message de aviso.
(defun my/get-intelephense-key ()
  "Obtiene la licencia de Intelephense (agenix -> pass -> nil + warn)."
  (let* ((secrets-dir (expand-file-name "~/dotfiles/secrets/"))
         (age-file (expand-file-name "intelephense-license.age" secrets-dir))
         (key (or
               ;; 1. agenix (necesita cwd = dir de secrets.nix)
               (and (executable-find "agenix")
                    (file-readable-p age-file)
                    (let* ((default-directory secrets-dir)
                           (out (string-trim
                                 (shell-command-to-string
                                  "agenix -d intelephense-license.age 2>/dev/null | head -1"))))
                      (and (not (string-empty-p out)) out)))
               ;; 2. pass
               (and (executable-find "pass")
                    (let ((out (string-trim
                                (shell-command-to-string
                                 "pass show personal/intelephense-license 2>/dev/null | head -1"))))
                      (and (not (string-empty-p out)) out))))))
    (unless key
      (message "[intelephense] Key no encontrada (ni agenix ni pass). LSP arrancara sin licencia premium."))
    key))

(after! lsp-mode
  ;; Rendimiento LSP: chunks grandes para outputs voluminosos (intelephense).
  ;; gc-cons-threshold lo gestiona gcmh-mode (dinamico, ya activo en Doom),
  ;; NO forzar valor estatico aqui (rompe gcmh y peligroso en RAM baja).
  (setq read-process-output-max (* 1024 1024)   ; 1MB chunks
        lsp-idle-delay 0.5
        lsp-log-io nil)  ; Activar solo para debug

  ;; Configuración de Intelephense
  (setq lsp-intelephense-licence-key (or (my/get-intelephense-key) "")
        lsp-intelephense-storage-path (expand-file-name "~/.config/emacs/.local/cache/intelephense")
        lsp-intelephense-files-max-size 5000000  ; 5MB
        lsp-intelephense-max-memory 4096         ; 4GB
        lsp-intelephense-multi-root nil
        lsp-intelephense-environment-php-version "8.3")

  ;; Optimizaciones de rendimiento
  (setq lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-links nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-text-document-color nil)

  ;; Personalización de completado
  (setq lsp-intelephense-completion-insert-use-declarations t
        lsp-intelephense-completion-fully-qualify-global-constants-and-functions nil)

  ;; Configuración de diagnóstico
  (setq lsp-intelephense-diagnostics-undefined-methods t
        lsp-intelephense-diagnostics-undefined-functions t
        lsp-intelephense-diagnostics-undefined-constants t
        lsp-intelephense-diagnostics-undefined-classes t
        lsp-intelephense-diagnostics-undefined-properties t)

  ;; Telemetría
  (setq lsp-intelephense-telemetry-enabled nil
        lsp-intelephense-format-enable t))

;; Funciones de utilidad para PHP/LSP
(defun force-intelephense-restart ()
  "Force restart Intelephense server."
  (interactive)
  (when (lsp-workspaces)
    (lsp-workspace-shutdown (lsp-workspaces)))
  (delete-directory (expand-file-name "~/.config/emacs/.local/cache/intelephense") t)
  (garbage-collect)
  (lsp))

(defun toggle-intelephense-debug ()
  "Toggle Intelephense debug mode."
  (interactive)
  (setq lsp-log-io (not lsp-log-io))
  (message "Intelephense debug: %s" (if lsp-log-io "ON" "OFF")))

;; Hook para PHP
(add-hook! php-mode
  (setq-local lsp-enable-file-watchers nil
              lsp-enable-indentation nil)
  (lsp-deferred))

;; ════════════════════════════════════════════════════════════════════════════
;; HERRAMIENTAS DE DESARROLLO
;; ════════════════════════════════════════════════════════════════════════════

;; --- Projectile ---
;; search-path: ~/src/ recursivo + ~/dotfiles. Vocento esta en ~/src/vocento
;; (subdir, ya detectado). NUNCA poner ~/ aqui (escanearia todo el home).
(setq projectile-project-search-path '("~/src/" "~/dotfiles/"))

;; Salvaguarda: $HOME tiene .git por los dotfiles, ignorar como proyecto.
(after! projectile
  (add-to-list 'projectile-ignored-projects (expand-file-name "~/")))

;; --- PlantUML ---
;; Usar el ejecutable de NixOS en lugar del jar
(after! plantuml-mode
  (setq plantuml-executable-path "plantuml"
        plantuml-default-exec-mode 'executable))

;; --- Telega ---
;; Usar variable de entorno para evitar hardcodear path de Nix store
(setq telega-server-libs-prefix (getenv "TDLIB_PREFIX"))
;; --- GitHub Copilot ---
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :config
  ;; idle-delay 0.3: balance entre responsividad y ruido/coste de API.
  ;; 0.1 disparaba peticion cada 100ms, demasiado agresivo.
  (setq copilot-idle-delay 0.3
        copilot-max-char 100000)
  ;; Desactivar warnings de manera correcta
  (add-to-list 'warning-suppress-types '(copilot))
  (add-to-list 'warning-suppress-log-types '(copilot)))

;; --- Persistencia de sesion (workspaces autoload) ---
;; Doom autoguarda la sesion (persp-mode) al matar Emacs, pero NO la carga
;; al arrancar. Este hook lo hace en GUI -- en daemon puro espera al primer
;; frame. Solo si existe sesion guardada previamente.
;; Guardado manual: `SPC q s` o M-x +workspace/save-session.
;; Carga manual:    `SPC q l` o M-x +workspace/load-session.
;;
;; Defensivo: persp-mode es lazy en Doom y persp-auto-save-fname/save-dir
;; no estan bound hasta primer uso. require + boundp + condition-case para
;; que un fallo NO reviente doom-after-init-hook (y rompa keybindings).
(defun +my/auto-load-session-h ()
  "Cargar ultima sesion de workspaces si existe."
  (when (display-graphic-p)
    (condition-case err
        (when (and (require 'persp-mode nil t)
                   (boundp 'persp-auto-save-fname)
                   (boundp 'persp-save-dir)
                   (fboundp '+workspace/load-session)
                   (file-exists-p
                    (expand-file-name persp-auto-save-fname persp-save-dir)))
          (+workspace/load-session))
      (error (message "[autoload-session] no se pudo cargar sesion: %s"
                      (error-message-string err))))))

(add-hook 'doom-after-init-hook #'+my/auto-load-session-h)

;; --- GPTel (Ollama) ---
(use-package! gptel
  :config
  (setq gptel-backend (gptel-make-ollama "Ollama"
                        :host "localhost:11434"
                        :stream t
                        :models '("llama3.1:latest"))
        gptel-model "llama3.1:latest"
        gptel-default-mode 'org-mode
        gptel-directives
        '((default . "Eres un asistente AI especializado en Emacs Doom, Haskell, y configuración de entornos de desarrollo. Proporciona atajos de Doom Emacs cuando sea relevante."))))

;; ════════════════════════════════════════════════════════════════════════════
;; FUNCIONES PERSONALIZADAS
;; ════════════════════════════════════════════════════════════════════════════

(defun copy-whole-buffer-to-clipboard ()
  "Copy entire buffer to clipboard."
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max))
  (message "Buffer copied to clipboard"))

;; ════════════════════════════════════════════════════════════════════════════
;; KEYBINDINGS
;; ════════════════════════════════════════════════════════════════════════════

(map! :leader
      ;; Toggle
      (:prefix ("t" . "toggle")
       :desc "Change font" "a" #'change-font
       :desc "Reload fonts" "A" #'reload-fonts
       :desc "Install Nerd Font" "I" #'select-and-install-nerd-font
       :desc "Toggle transparency" "t" #'toggle-transparency)

      ;; Journal
      (:prefix ("j" . "journal")
       :desc "New entry" "j" #'org-journal-new-entry
       :desc "Next entry" "n" #'org-journal-open-next-entry
       :desc "Previous entry" "p" #'org-journal-open-previous-entry
       :desc "Search" "s" #'org-journal-search)

      ;; Yank
      :desc "Copy buffer" "y b" #'copy-whole-buffer-to-clipboard

      ;; Diagnostics
      :desc "Verify commands" "d v" #'verify-essential-commands)

;; Keybindings locales para PHP
(map! :after php-mode
      :map php-mode-map
      :localleader
      (:prefix ("l" . "lsp")
               "R" #'force-intelephense-restart
               "d" #'toggle-intelephense-debug
               "i" #'lsp-intelephense-index-workspace))

;; Keybinding global para insertar imágenes
(global-set-key (kbd "S-<print>") 'brutalist-clipboard-png-insert)

;; ════════════════════════════════════════════════════════════════════════════
;; CONFIGURACIÓN HASKELL + HLS
;; ════════════════════════════════════════════════════════════════════════════

;; Configuración mínima: Doom maneja todo automáticamente con +lsp +tree-sitter
;; Ver guía completa de comandos en: ~/src/pensando-en-haskell/README.org
;; Busca la sección "🎯 Doom Emacs: Guía Completa de Comandos"

(after! lsp-haskell
  ;; Solo configuramos fourmolu como formateador
  (setq lsp-haskell-formatting-provider "fourmolu"))

(setq haskell-interactive-popup-errors nil) ; Deshabilitar popups de errores

;; Flycheck haskell-stack-ghc no usa nuestro proyecto (cabal-based) y
;; ademas peta con caracteres unicode en el codigo. Desactivado.
(after! flycheck
  (add-to-list 'flycheck-disabled-checkers 'haskell-stack-ghc))
;; ════════════════════════════════════════════════════════════════════════════
;; NIX-MODE FIX
;; ════════════════════════════════════════════════════════════════════════════
;; Fix: font-lock no se activa automáticamente en algunos casos
(add-hook 'nix-mode-hook #'font-lock-ensure)

;; ════════════════════════════════════════════════════════════════════════════
;; DAPE - Debug Adapter Protocol
;; ════════════════════════════════════════════════════════════════════════════

;; Función para instalar vscode-php-debug automáticamente
(defun dape-ensure-php-debug-adapter ()
  "Instala vscode-php-debug adapter si no existe."
  (let* ((adapter-dir (expand-file-name "~/.config/doom/debug-adapters/php-debug"))
         (adapter-file (expand-file-name "extension/out/phpDebug.js" adapter-dir))
         (download-url "https://github.com/xdebug/vscode-php-debug/releases/download/v1.35.0/php-debug-1.35.0.vsix")
         (temp-file (make-temp-file "php-debug-" nil ".vsix")))

    (unless (file-exists-p adapter-file)
      (message "📦 Instalando vscode-php-debug adapter...")

      ;; Crear directorio
      (make-directory (expand-file-name "~/.config/doom/debug-adapters") t)

      ;; Descargar .vsix
      (url-copy-file download-url temp-file t)
      (message "✓ Descargado adaptador")

      ;; Descomprimir
      (call-process "unzip" nil nil nil "-q" temp-file "-d" adapter-dir)
      (delete-file temp-file)
      (message "✓ Adaptador instalado en %s" adapter-dir)

      ;; Verificar instalación
      (if (file-exists-p adapter-file)
          (message "✅ vscode-php-debug adapter instalado correctamente")
        (error "❌ Error instalando el adaptador")))

    ;; Retornar true si existe
    (file-exists-p adapter-file)))

;; Atajos dape estilo IntelliJ IDEA. Globales y unbound previamente.
;; Solo hacen algo cuando hay sesion de debug activa - si no, dan
;; "No active debug session" sin romper nada.
(map! "<f7>"   #'dape-step-in              ; IntelliJ F7      (entra en funcion)
      "<f8>"   #'dape-next                 ; IntelliJ F8      (step over)
      "<S-f8>" #'dape-step-out             ; IntelliJ Shift+F8 (sale de funcion)
      "<f9>"   #'dape-continue             ; IntelliJ F9      (resume)
      "<C-f8>" #'dape-breakpoint-toggle    ; IntelliJ Ctrl+F8 (toggle breakpoint)
      "<C-f2>" #'dape-quit                 ; IntelliJ Ctrl+F2 (stop)
      "<M-f8>" #'dape-evaluate-expression) ; IntelliJ Alt+F8  (evaluate expression)

(use-package! dape
  :config
  ;; hdb (Haskell Debugger) en el primer launch tarda 15-30s descubriendo
  ;; flags via hie-bios + cabal path + ghc --print-libdir. Default 10s
  ;; mata la conexion antes de que hdb conteste.
  (setq dape-request-timeout 60)
  ;; PHP con Xdebug 3.x usando vscode-php-debug
  ;; NOTA: No usamos 'ensure' automático porque causa errores al cargar la config.
  ;; Para instalar/reinstalar el adaptador manualmente: M-x dape-ensure-php-debug-adapter
  (add-to-list 'dape-configs
               `(phpListen
                 modes (php-mode)
                 command "node"
                 command-args (,(expand-file-name "~/.config/doom/debug-adapters/php-debug/extension/out/phpDebug.js"))
                 :type "php"
                 :request "launch"
                 :mode "listen"
                 :port 9003))

  ;; Haskell con hdb (DAP debugger de Well-Typed, requiere GHC 9.14+)
  ;;
  ;; Como funciona (replica lo que la extension VSCode oficial hace):
  ;;   1. dape pide un puerto libre (autoport).
  ;;   2. Lanza `hdb-dap server --port <port>` (wrapper que entra en
  ;;      `nix develop` antes de hdb, asegurando GHC 9.14 en PATH).
  ;;   3. dape conecta TCP a localhost:<port>.
  ;;   4. Manda launch request con projectRoot + entryFile + entryPoint.
  ;;
  ;; Instalacion previa (una sola vez):
  ;;   cd <proyecto-haskell-con-flake> && nix develop
  ;;   cabal install haskell-debugger --installdir=$HOME/.local/bin
  ;;   (el wrapper hdb-dap esta en dotfiles, ya en PATH)
  (add-to-list 'dape-configs
               `(haskell-hdb
                 modes (haskell-mode haskell-ts-mode haskell-cabal-mode)
                 ensure (lambda (config)
                          (unless (executable-find "hdb")
                            (user-error
                             "hdb no encontrado en PATH. Verifica ~/.local/bin/hdb")))
                 fn (dape-config-autoport)
                 host "localhost"
                 port :autoport
                 command "hdb"
                 command-args ("server" "--port" :autoport)
                 :type "haskell-debugger"
                 :request "launch"
                 :projectRoot dape-cwd-fn
                 :entryFile dape-buffer-default
                 :entryPoint "main"
                 :entryArgs []
                 :extraGhcArgs []
                 ;; internal-interpreter = true: usa el interprete GHCi
                 ;; embebido en el proceso hdb en lugar de spawnar un
                 ;; subprocess externo. Cada launch arranca mas rapido
                 ;; al saltarse la sincronizacion via pipes con el proceso
                 ;; externo. Trade-off: el debuggee comparte la heap del
                 ;; debugger, lo cual va bien para programas pequenos
                 ;; (este Debugging101 lo es). Para programas grandes
                 ;; volver a nil.
                 :internalInterpreter t)))

;; Comando interactivo para reinstalar el adaptador
(defun dape-reinstall-php-debug-adapter ()
  "Reinstala vscode-php-debug adapter forzosamente."
  (interactive)
  (let ((adapter-dir (expand-file-name "~/.config/doom/debug-adapters/php-debug")))
    (when (file-exists-p adapter-dir)
      (delete-directory adapter-dir t)
      (message "🗑️  Adaptador anterior eliminado"))
    (dape-ensure-php-debug-adapter)))

;;; config.el ends here
