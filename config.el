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
  '("Hack Nerd Font" "JetBrainsMono Nerd Font" "FiraCode Nerd Font"
    "Iosevka Nerd Font" "CaskaydiaCove Nerd Font" "VictorMono Nerd Font"
    "0xProto Nerd Font" "3270 Nerd Font" "Agave Nerd Font"
    "AnonymicePro Nerd Font" "Arimo Nerd Font" "AurulentSansMono Nerd Font"
    "BigBlueTerminal Nerd Font" "Bitstream Vera Sans Mono Nerd Font"
    "BlexMono Nerd Font" "Cascadia Mono Nerd Font"
    "CodeNewRoman Nerd Font" "ComicShannsMono Nerd Font" "CommitMono Nerd Font"
    "Cousine Nerd Font" "D2Coding Nerd Font" "DaddyTimeMono Nerd Font"
    "DejaVuSansMono Nerd Font" "DroidSansMono Nerd Font" "EnvyCodeR Nerd Font"
    "FantasqueSansMono Nerd Font" "FiraMono Nerd Font"
    "GeistMono Nerd Font" "GoMono Nerd Font" "Gohu Nerd Font"
    "Hasklug Nerd Font" "HeavyData Nerd Font" "Hurmit Nerd Font"
    "iMWriting Nerd Font" "Inconsolata Nerd Font" "InconsolataGo Nerd Font"
    "InconsolataLGC Nerd Font" "IntelOne Mono Nerd Font"
    "IosevkaTerm Nerd Font" "Lekton Nerd Font" "LiterationMono Nerd Font"
    "Lilex Nerd Font" "MartianMono Nerd Font" "Meslo Nerd Font"
    "Monaspace Nerd Font" "Monofur Nerd Font" "Monoid Nerd Font"
    "Mononoki Nerd Font" "MPlus Nerd Font" "Noto Nerd Font"
    "OpenDyslexic Nerd Font" "Overpass Nerd Font" "ProFont Nerd Font"
    "ProggyClean Nerd Font" "RobotoMono Nerd Font" "ShareTechMono Nerd Font"
    "SourceCodePro Nerd Font" "SpaceMono Nerd Font" "Terminess Nerd Font"
    "Tinos Nerd Font" "Ubuntu Nerd Font" "UbuntuMono Nerd Font")
  "Lista de Nerd Fonts disponibles.")

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
            doom-italic-font (font-spec :family main-font-family :slant 'italic :size base-font-size)
            doom-bold-font (font-spec :family main-font-family :weight 'bold :size base-font-size))
      (when (display-graphic-p)
        (set-face-attribute 'default nil :font doom-font)
        (set-face-attribute 'fixed-pitch nil :font doom-font)
        (set-face-attribute 'variable-pitch nil :font doom-variable-pitch-font))
      (message "Font configured: %s" main-font-family))))

(setup-fonts)

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

  (add-hook! 'kill-buffer-hook
    (defun +org-encrypt-on-save ()
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

;; Función para insertar imágenes desde el portapapeles
(defun brutalist-clipboard-png-insert ()
  "Insert PNG image from clipboard into Org file."
  (interactive)
  (let* ((timestamp (format-time-string "%Y%m%d_%H%M%S"))
         (image-dir (expand-file-name "~/org/images/"))
         (image-name (concat timestamp ".png"))
         (image-path (expand-file-name image-name image-dir)))

    (unless (file-directory-p image-dir)
      (make-directory image-dir t))

    (let ((xclip-output
           (with-temp-buffer
             (let ((coding-system-for-read 'binary)
                   (coding-system-for-write 'binary))
               (if (zerop (call-process "xclip" nil t nil
                                        "-selection" "clipboard"
                                        "-t" "image/png" "-o"))
                   (progn
                     (write-region (point-min) (point-max) image-path nil 'silent)
                     (buffer-string))
                 (error "Failed to get PNG data from clipboard"))))))

      (when (and xclip-output (> (length xclip-output) 0))
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

(after! lsp-mode
  ;; Rendimiento crítico
  (setq gc-cons-threshold (* 2 1024 1024 1024)  ; 2GB durante LSP
        read-process-output-max (* 1024 1024)   ; 1MB chunks
        lsp-idle-delay 0.5
        lsp-log-io nil)  ; Activar solo para debug

  ;; Configuración de Intelephense
  (setq lsp-intelephense-licence-key "002K3PRSEO670TI"
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
(setq projectile-project-search-path '("~/src/" "~/src/vocento" "~/"))
(projectile-add-known-project "~/src/")
(projectile-add-known-project "~/src/vocento")

;; Evitar que $HOME sea reconocido como project root (tiene .git)
(after! projectile
  (setq projectile-project-root-files-bottom-up
        (remove ".git" projectile-project-root-files-bottom-up)))

;; --- PlantUML ---
;; Usar el ejecutable de NixOS en lugar del jar
(after! plantuml-mode
  (setq plantuml-executable-path "plantuml"
        plantuml-default-exec-mode 'executable))

;; --- Telega ---
(setq telega-server-libs-prefix "/nix/store/v314k2bmm149brgsmvaji1y9jl9x9n2p-tdlib-1.8.47")
;; --- GitHub Copilot ---
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word))
  :config
  (setq copilot-idle-delay 0.1
        copilot-max-char 100000)
  ;; Desactivar warnings de manera correcta
  (add-to-list 'warning-suppress-types '(copilot))
  (add-to-list 'warning-suppress-log-types '(copilot)))

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
;; ════════════════════════════════════════════════════════════════════════════
;; NIX-MODE FIX
;; ════════════════════════════════════════════════════════════════════════════
;; Fix: font-lock no se activa automáticamente en algunos casos
(add-hook 'nix-mode-hook #'font-lock-ensure)

;; ════════════════════════════════════════════════════════════════════════════
;; CONFIGURACIÓN DE COMPRESIÓN
;; ════════════════════════════════════════════════════════════════════════════

(setq compression-file-name-handler-alist
      '(("\\.gz\\'" . gzip-file-handler)
        ("\\.bz2\\'" . bzip2-file-handler)
        ("\\.xz\\'" . xz-file-handler)
        ("\\.zip\\'" . zip-file-handler)
        ("\\.Z\\'" . compress-file-handler)))


;; ════════════════════════════════════════════════════════════════════════════
;; DAP-MODE (DESHABILITADO - Usando dape en su lugar)
;; ════════════════════════════════════════════════════════════════════════════
;; (after! dap-mode
;;   (setq dap-auto-configure-mode nil)
;;   (setq dap-external-terminal '("alacritty" "--hold" "-e" ))
;;   (setq dap-default-terminal-kind "external")
;;   (require 'dap-php)
;;   (setq dap-php-debug-port 9003)
;;   (setq dap-php-executable "php"))
;; (add-hook 'php-mode-hook #'dap-mode)


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

(use-package! dape
  :config
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
                 :port 9003)))

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
