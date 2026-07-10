;;; brain-search.el --- Aflorar el tercer cerebro por significado -*- lexical-binding: t; -*-

;; Brain-en-Doom: busca el roam (~/org/roam) por SIGNIFICADO desde Emacs,
;; tirando del buscador semantico del enjambre (brain-search.py, embeddings
;; bge-m3 en Ollama local). Presenta los nodos con completing-read (vertico
;; lo viste solo) y abre el elegido.
;;
;; Comandos:
;;   my/brain-search   (SPC n b s)  busca por significado y abre el nodo
;;   my/brain-reindex  (SPC n b r)  (re)embebe nodos nuevos/cambiados
;;   my/brain-dups     (SPC n b d)  pares de nodos casi-duplicados
;;
;; Todo asincrono (make-process): la primera busqueda puede indexar y tardar,
;; pero Emacs no se congela. Los errores (script ausente, Ollama caido) salen
;; como mensaje claro, no como backtrace.
;;
;; Se carga desde config.org (seccion BRAIN) con (load! "brain-search").

;;; Code:

(defvar my/brain-search-script
  (expand-file-name "~/dotfiles/scripts/brain-search.py")
  "Ruta del buscador semantico del enjambre.")

(defvar my/brain-bin
  (expand-file-name "~/dotfiles/skills/ambrosio/brain/bin/brain")
  "Ruta del wrapper brain (para dups y demas subcomandos).")

(defvar my/brain-search-topn 10
  "Cuantos nodos pedir al buscador semantico.")

(defvar my/brain-search--stderr-buffer " *brain-search-stderr*"
  "Buffer oculto donde va el stderr del buscador (progreso e indexado).")

(defun my/brain-search--check ()
  "Aborta con mensaje claro si falta alguna pieza del pipeline."
  (unless (executable-find "python3")
    (user-error "brain: no encuentro python3 en el PATH de Emacs"))
  (unless (file-exists-p my/brain-search-script)
    (user-error "brain: no existe el script %s" my/brain-search-script)))

(defun my/brain-search--parse (output)
  "Parsea la salida de brain-search.py.
Devuelve una lista ordenada de (TITULO . RUTA-ABSOLUTA).
El formato es: linea de titulo (con posible marcador de palabra clave)
seguida de una linea indentada con la ruta ~/org/roam/<fichero>.org."
  (let (results title)
    (dolist (line (split-string output "\n"))
      (if (string-match "^[ \t]+\\(~/org/roam/.+\\.org\\)[ \t]*$" line)
          (when title
            (push (cons title (expand-file-name (match-string 1 line))) results)
            (setq title nil))
        (let ((trimmed (string-trim line)))
          (unless (or (string-empty-p trimmed)
                      ;; cabecera del script: contiene la query entre comillas
                      ;; angulares y "top N"
                      (and (string-match-p "top [0-9]+" trimmed)
                           (string-match-p "«" trimmed)))
            ;; quitar el marcador de solape lexico (candado, U+1F511) del final
            (setq title (string-trim
                         (replace-regexp-in-string "[ \t]*\U0001F511[ \t]*$" ""
                                                   trimmed)))))))
    (nreverse results)))

(defun my/brain-search--select-and-open (nodes)
  "Presenta NODES ((TITULO . RUTA)...) con completing-read y abre el elegido.
Respeta el orden de relevancia del buscador (no reordena alfabeticamente)."
  (let* ((candidates nodes)
         (table (lambda (str pred action)
                  (if (eq action 'metadata)
                      `(metadata
                        (display-sort-function . identity)
                        (cycle-sort-function . identity)
                        (annotation-function
                         . ,(lambda (cand)
                              (when-let ((path (cdr (assoc cand candidates))))
                                (concat "  " (file-name-nondirectory path))))))
                    (complete-with-action action (mapcar #'car candidates)
                                          str pred))))
         (choice (completing-read "Nodo del cerebro: " table nil t))
         (path (cdr (assoc choice candidates))))
    (when path
      (find-file path))))

(defun my/brain-search (query)
  "Busca el tercer cerebro (org-roam) por SIGNIFICADO y abre el nodo elegido.
Lanza brain-search.py (embeddings bge-m3 via Ollama local) de forma
asincrona; al terminar ofrece los resultados con `completing-read'."
  (interactive (list (read-string "Buscar en el cerebro (significado): ")))
  (my/brain-search--check)
  (when (string-empty-p (string-trim query))
    (user-error "brain: dame una consulta"))
  (let ((stdout-buf (generate-new-buffer " *brain-search*"))
        (stderr-buf (get-buffer-create my/brain-search--stderr-buffer)))
    (with-current-buffer stderr-buf (erase-buffer))
    (message "brain: buscando \"%s\"... (si hay nodos nuevos, indexa y tarda un poco)"
             query)
    (make-process
     :name "brain-search"
     :buffer stdout-buf
     :stderr stderr-buf
     :noquery t
     :command (list "python3" my/brain-search-script
                    "-n" (number-to-string my/brain-search-topn) query)
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (unwind-protect
             (if (/= (process-exit-status proc) 0)
                 (message "brain: fallo el buscador (%s). Ultimo stderr: %s"
                          (process-exit-status proc)
                          (with-current-buffer stderr-buf
                            (string-trim
                             (buffer-substring-no-properties
                              (max (point-min) (- (point-max) 300))
                              (point-max)))))
               (let ((nodes (my/brain-search--parse
                             (with-current-buffer stdout-buf (buffer-string)))))
                 (if (null nodes)
                     (message "brain: sin resultados para \"%s\"" query)
                   (condition-case nil
                       (my/brain-search--select-and-open nodes)
                     (quit (message "brain: busqueda cancelada"))))))
           (kill-buffer stdout-buf)))))))

(defun my/brain-reindex (&optional full)
  "Embebe los nodos nuevos o cambiados del roam (asincrono, incremental).
Con prefijo \\[universal-argument] (FULL) fuerza re-embeber TODO el grafo
(tarda ~2 min; solo hace falta si cambia el modelo de embeddings)."
  (interactive "P")
  (my/brain-search--check)
  (message "brain: %s..."
           (if full
               "re-embebiendo TODO el grafo (tarda ~2 min)"
             "indexando nodos nuevos/cambiados"))
  (let ((buf (get-buffer-create " *brain-reindex*")))
    (with-current-buffer buf (erase-buffer))
    (make-process
     :name "brain-reindex"
     :buffer buf
     :noquery t
     :command (append (list "python3" my/brain-search-script)
                      (when full (list "--reindex")))
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (if (zerop (process-exit-status proc))
             (message "brain: indice de embeddings al dia")
           (message "brain: fallo el reindex (Ollama caido? mira %s)"
                    (buffer-name buf))))))))

(defun my/brain-dups ()
  "Muestra pares de nodos casi-duplicados del roam en un buffer (asincrono)."
  (interactive)
  (unless (file-exists-p my/brain-bin)
    (user-error "brain: no existe el wrapper %s" my/brain-bin))
  (message "brain: buscando casi-duplicados...")
  (let ((buf (get-buffer-create "*brain-dups*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t)) (erase-buffer)))
    (make-process
     :name "brain-dups"
     :buffer buf
     :noquery t
     :command (list my/brain-bin "dups")
     :sentinel
     (lambda (proc _event)
       (when (memq (process-status proc) '(exit signal))
         (if (zerop (process-exit-status proc))
             (with-current-buffer buf
               (special-mode)
               (pop-to-buffer buf))
           (message "brain: fallo brain dups (Ollama caido?)")))))))

;; Atajos: SPC n b (notes -> brain). Verificado libre en esta config
;; (lookup-key doom-leader-map "nb" -> nil) el 2026-07-10.
(map! :leader
      (:prefix ("n b" . "brain")
       :desc "Buscar por significado" "s" #'my/brain-search
       :desc "Reindexar embeddings"   "r" #'my/brain-reindex
       :desc "Casi-duplicados"        "d" #'my/brain-dups))

(provide 'brain-search)
;;; brain-search.el ends here
