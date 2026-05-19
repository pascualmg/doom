# Doom Emacs - Instrucciones para Claude

Configuracion personal de Doom Emacs de Pascual (`pascualmg/doom`).
Repo publico. Cualquier cosa que vaya aqui sera visible en GitHub.

## REGLA DE ORO

**NUNCA hardcodear secretos en este repo.** Es publico. Si necesitas una
clave/token/licencia, leela en runtime desde:

1. `agenix`: `~/dotfiles/secrets/<nombre>.age` (declarativo, clone-first)
2. `pass`: `<categoria>/<nombre>` (gpg, fuente activa)
3. Si falta, avisar con `message` y arrancar sin esa feature.

Patron de referencia: `my/get-intelephense-key` en `config.el`.
Incidente que motivo esta regla: licencia Intelephense hardcodeada y
publicada (2026-05-19). Quemada, no se rotara - asumido.

## Estructura del repo

```
~/src/doom/             (symlink desde ~/.config/doom)
|-- init.el             Modulos Doom activados (doom! macro)
|-- config.el           Configuracion personal (~620 lineas)
|-- custom.el           Generado por Custom (NO editar a mano normalmente)
|-- packages.el         Paquetes extra (telega, copilot, gptel, ...)
|-- debug-adapters/     Adaptadores DAP descargados (php-debug)
|-- dape-howto-noobs.org   Notas propias sobre dape
`-- README.md
```

Binario Doom: `~/.config/emacs/bin/doom` (vendor en `~/.config/emacs/`).

## Aplicar cambios

```bash
~/.config/emacs/bin/doom sync       # tras editar packages.el o init.el
~/.config/emacs/bin/doom doctor     # diagnostico
M-x doom/reload                     # tras editar config.el (sin reiniciar)
```

## Control del daemon vivo (clave para Claude)

Pascual tiene Emacs como daemon (`emacs --daemon`, normalmente lanzado por
systemd user service o al login). Esto significa que **puedo verificar
cambios en vivo via `emacsclient --eval`** sin pedirle que reinicie nada.

### Comandos canonicos

```bash
# Ping al server
emacsclient --eval "(emacs-version)"

# Cargar el config.el modificado (recarga las funciones nuevas)
emacsclient --eval "(load (expand-file-name \"config.el\" doom-user-dir))"

# Comprobar que una funcion existe
emacsclient --eval "(fboundp 'my/funcion)"

# Ejecutar y ver resultado
emacsclient --eval "(my/funcion arg)"

# Leer los ultimos N caracteres de *Messages* (errores, warnings)
emacsclient --eval "(with-current-buffer \"*Messages*\" \
  (buffer-substring-no-properties \
    (max (point-min) (- (point-max) 2000)) (point-max)))"

# Forzar carga de un paquete lazy
emacsclient --eval "(require 'lsp-mode)"

# Ver si un paquete esta cargado
emacsclient --eval "(featurep 'lsp-mode)"

# Abrir un fichero sin robar foco (para forzar arranque LSP, p.ej.)
emacsclient -n /ruta/al/fichero.php
```

### CUIDADO con `doom/reload`

`emacsclient --eval "(doom/reload)"` **NO funciona como esperarias** desde
shell: lanza un proceso async de compilacion y vuelve antes de terminar.
Para verificar cambios en `config.el` usar siempre el `(load ...)` directo.

`doom/reload` solo es util cuando hay cambios estructurales (init.el o
packages.el) y necesita reiniciarse el daemon de todas formas.

### Loop de verificacion canonico (Claude)

Tras editar `config.el`:

1. Hacer el cambio con Edit/Write.
2. `emacsclient --eval "(load (expand-file-name \"config.el\" doom-user-dir))"`
   - Si devuelve `t`: cargo OK.
   - Si devuelve string con error: leer y corregir.
3. `emacsclient --eval "(fboundp 'my/funcion-cambiada)"` -> `t`.
4. `emacsclient --eval "(my/funcion-cambiada ...)"` -> verificar valor.
5. Leer ultimos 2KB de `*Messages*` por warnings.
6. Reportar OK/NOK a Pascual.

Tras editar `init.el` o `packages.el`:

1. Hacer el cambio.
2. `~/.config/emacs/bin/doom sync` (puede tardar minutos).
3. Avisar a Pascual: necesita reiniciar daemon
   (`systemctl --user restart emacs` o `pkill emacs && emacs --daemon`).
4. Tras reinicio: loop de verificacion como arriba.

### Lo que NO puedo hacer

- **Ver la pantalla**: no se que tiene visible. Si necesito saberlo,
  preguntar por buffer/window: `(buffer-name)`, `(window-list)`,
  `(frame-list)`.
- **Interactuar con minibuffer prompts**: si una funcion pide input
  interactivo (y--n--p, completing-read), se queda colgada y emacsclient
  no devuelve.
- **Deshacer cagadas en buffers vivos**: si meto la pata en un buffer
  abierto, Pascual tiene que `C-/` el mismo.
- **Reiniciar el daemon sin avisar**: tira todos los frames. Solo con
  permiso explicito.

## Modulos Doom relevantes

Lista completa en `init.el`. Lo importante:

- **Completion**: `company +childframe` + `vertico`
- **UI**: doom theme, dashboard, treemacs +lsp, modeline, ligatures, minimap,
  vc-gutter, workspaces, zen, tabs
- **Editor**: `evil +everywhere`, snippets, fold, format-on-save
- **Tools**: magit, lsp, tree-sitter, **dape** (+lsp), direnv, eval+overlay,
  lookup, vterm
- **Lang con LSP**: php (+tree-sitter +dap), haskell (+test), nix, js
  (+tree-sitter), web, python, cc, sh, json/yaml, markdown, plantuml, rest
- **Org**: `+babel +lsp +roam +journal`

Extras via `packages.el`: telega, copilot, gptel (Ollama backend),
ellama, restclient, verb, ob-http, all-the-icons, org-gcal,
ewal-spacemacs-themes, spaceline, org-present.

## Integraciones criticas

### Secretos (Intelephense)

`my/get-intelephense-key` en `config.el` lee la licencia con orden:

1. `agenix -d ~/dotfiles/secrets/intelephense-license.age` (requiere ssh key)
2. `pass show personal/intelephense-license`
3. nil + warning -> Intelephense arranca en modo free.

### dape (Debug Adapter Protocol)

Reemplaza dap-mode. Adapters configurados en `config.el`:

- **phpListen**: PHP + Xdebug 3 (puerto 9003). Adapter en
  `debug-adapters/php-debug/`. Instalacion automatica:
  `M-x dape-ensure-php-debug-adapter`.
- **haskell-hdb**: Haskell Debugger de Well-Typed. Requiere GHC 9.14+,
  wrapper `hdb-dap` en PATH (en dotfiles), `hdb` instalado via
  `cabal install haskell-debugger` desde dentro de `nix develop`.

### Org

- `org-directory`: `~/org/`
- `org-roam-directory`: `~/org/roam`
- `org-journal-dir`: `~/org/journal/` (formato mensual `%Y%m.org`)
- Crypt: `org-crypt-key "85E4C775557B92E4"`, cifra en `kill-buffer-hook`
  (OJO: el nombre `+org-encrypt-on-save` MIENTE - es on-kill).

### LLMs en Emacs

- **copilot.el**: hook en `prog-mode`. Tab acepta, idle-delay 0.1
  (agresivo, baja a 0.3-0.5 si molesta).
- **gptel**: backend Ollama en `localhost:11434`, modelo `llama3.1:latest`.
  Default-mode `org-mode`. Directives en espanol.
- **ellama**: paquete instalado pero sin configurar.

### Fuentes

`setup-fonts` en `config.el` busca la primera Nerd Font disponible de una
lista enorme (~60). Fallback: `safe-fonts`. Italicas con `VictorMono Nerd Font`.
Funcion `download-and-install-nerd-font` descarga e instala fonts via curl
(antinix - en NixOS las fonts deberian venir del flake).

### Transparencia

Detecta PGTK vs X11. `set-frame-transparency 85` por defecto.
`toggle-transparency` (SPC t t) alterna 85<->100.

## Auditoria de problemas (2026-05-19)

Estado tras eliminar la licencia hardcodeada. Pendientes ordenados por
severidad:

### CRITICO

- [x] **`org-agenda-files` en `custom.el` pisa la config**. RESUELTO en
  commit `46f72c6` (Fase 1, 2026-05-19). Linea eliminada de custom.el.

### ALTO

- [x] **`gc-cons-threshold` a 2GB en `after! lsp-mode`**. RESUELTO en
  `46f72c6`. Control devuelto a gcmh-mode. Tras restart daemon: 16MB.

- [x] **`compression-file-name-handler-alist` no existe**. RESUELTO en
  `46f72c6`. Bloque borrado. Tambien borrado bloque DAP-mode comentado.

- [x] **Clipboard PNG asume xclip**. RESUELTO en `46f72c6`. Detecta
  `(featurep 'pgtk)` o `WAYLAND_DISPLAY` -> wl-paste; resto -> xclip.
  Avisa si la tool no esta instalada.

- [x] **`+org-encrypt-on-save` MIENTE en el nombre**. RESUELTO en
  `46f72c6`. Renombrado a `+org-encrypt-before-save` y movido a
  `before-save-hook` (mas seguro: cifra cada save, no solo al matar).

### MEDIO

- [ ] **`projectile-project-search-path` redundante/peligroso**
  (config.el:405). `~/` escanea todo el home. `~/src/vocento` es subdir
  de `~/src/`. Reducir a `'("~/src/" "~/dotfiles/")`.

- [ ] **Bloque DAP-mode comentado** (config.el:529-538). 10 lineas
  inertes con label "DESHABILITADO". Borrar (queda en git history).

- [ ] **`packages.el`: `lsp-mode` y `php-mode` declarados explicitamente**.
  Ya vienen con modulos Doom (`:tools lsp`, `:lang php`). Sobran.

- [ ] **`custom.el` `package-selected-packages` con paquetes fantasma**.
  Lista `geben`, `base16-theme`, `claude-code`, `claude-shell`,
  `mermaid-mode`, etc. que NO estan en `packages.el`. Doom usa straight,
  esto no se aplica - pero confunde. Limpiar.

- [ ] **`my-nerd-fonts` con 60+ fuentes**. `find-first-available-font`
  siempre devuelve "Hack Nerd Font" (la tienes). Las otras 59 nunca se
  evaluan. Reducir a 5-6 fallbacks reales.

- [ ] **`copilot-idle-delay 0.1`**. Dispara cada 100ms. Subir a 0.3-0.5
  para reducir ruido y coste API.

- [ ] **`flycheck haskell-stack-ghc` desactivado en sitio raro**
  (config.el:578, dentro del bloque DAPE). Mover a seccion Haskell.

### BAJO / SUGERENCIAS

- [ ] **`[yas] yas-snippet-dirs: /home/passh/.config/doom/snippets/ is not a directory`**.
  Warning preexistente detectado durante verificacion Fase 1 (2026-05-19).
  Soluciones: `mkdir -p ~/.config/doom/snippets/`, o quitar modulo
  `:editor snippets` si no se usan, o redirigir a otra ruta con
  `yas-snippet-dirs`.

- [ ] **`download-and-install-nerd-font`**: imperativo en NixOS. Si las
  fonts vienen del flake, esta funcion sobra. Decidir.

- [ ] **Modulos Doom desactivados a considerar**:
  - `:checkers (spell +flyspell)` - utiles para org/blog ES/EN
  - `:ui window-select` - saltar entre ventanas con `SPC w w` letra
  - `:ui hydra` - menus transient

- [ ] **Modulos Doom activados a revisar**:
  - `minimap` - lento en archivos grandes. ?Se usa?
  - `tabs` - con workspaces ya hay navegacion

- [ ] **`gptel` solo con Ollama**. Potencial: anadir backends para
  Claude API (Anthropic), OpenAI, etc. La key iria por agenix+pass
  como Intelephense.

- [ ] **Duplicacion org-journal**: el flag `:lang (org +journal)`
  ya carga `org-journal`. Tu `use-package! org-journal` puede ser solo
  configuracion (`:config`). Verificar que no reinstala.

- [ ] **Modulo `+roam` vs `+roam2`**: Doom moderno tiene los dos. El
  ultimo commit cambio de `+roam2` a `+roam`. Confirmar que es la
  version querida.

## Roadmap de mejoras

### Fase 1 - Limpieza (HECHA - commit 46f72c6, 2026-05-19)

1. [x] Arreglar `org-agenda-files` en custom.el.
2. [x] Quitar `gc-cons-threshold` 2GB.
3. [x] Quitar `compression-file-name-handler-alist` (codigo muerto).
4. [x] Arreglar clipboard PNG para Wayland.
5. [x] Renombrar/mover hook de cifrado org (now `+org-encrypt-before-save`).

Bonus: borrado bloque DAP-mode comentado y `doom upgrade` ejecutado.
Verificado en vivo via emacsclient contra daemon reiniciado limpio.

Pendiente en Fase 1.5 (limpieza minor):
- Reducir `projectile-project-search-path` (quitar `~/` y subdirs).
- Limpiar `packages.el` (sacar `lsp-mode`, `php-mode` redundantes).
- Limpiar `custom.el` `package-selected-packages` con paquetes fantasma.
- Reducir `my-nerd-fonts` a 5-6 fallbacks reales.
- Subir `copilot-idle-delay` a 0.3-0.5.
- Mover `flycheck haskell-stack-ghc` desactivado a su seccion correcta.
- Decidir snippets dir (warning yas-snippet-dirs).

### Fase 2 - Decisiones de modulos

Decidir activar/desactivar:
- spell +flyspell (probable SI)
- window-select (probable SI)
- hydra (mirar)
- minimap (probable NO)
- tabs (mirar)

### Fase 3 - LLMs

- Configurar gptel con multiples backends (Claude API + Ollama).
- Key de Claude por agenix+pass.
- Evaluar `claude-shell` o `claude-code.el` si surge.
- Atajos `SPC SPC` para chat rapido.

### Fase 4 - Workflow org/roam

- Capture templates utiles (journal, todo, blog Cohete).
- Integracion con `~/org/journal/` y blog Cohete.
- Roam workflow definido (zettelkasten? notas tecnicas?).

### Fase 5 - Telega + comms

- Configurar telega con TDLIB_PREFIX bien.
- Atajos comodos para mensajear sin salir del flow.

### Fase 6 - LSP/DAP por lenguaje

- Afinar PHP+Intelephense+dape (Vocento es PHP).
- Haskell + hdb (ya empezado).
- Nix LSP (nil/nixd?).
- TypeScript/JS si toca.

## Workflows utiles

### Anadir un nuevo modulo Doom

1. Descomentar/anadir en `init.el`.
2. `~/.config/emacs/bin/doom sync`.
3. Reiniciar Emacs.

### Anadir un paquete extra

1. `(package! <nombre>)` en `packages.el`.
2. `~/.config/emacs/bin/doom sync`.
3. Configurar con `use-package!` en `config.el`.
4. `M-x doom/reload`.

### Rotar una clave/secreto

1. Actualizar en pass: `pass edit <categoria>/<nombre>`.
2. Regenerar el .age:
   ```bash
   cd ~/dotfiles/secrets
   pass show <categoria>/<nombre> | head -1 | agenix -e <nombre>.age
   ```
3. Commit en dotfiles.
4. No necesita tocar este repo (la funcion lee dinamicamente).

### Debug LSP

- `SPC l` prefix (lsp menu).
- `M-x lsp-describe-session`.
- `toggle-intelephense-debug` (atajo local en php-mode).
- `force-intelephense-restart` borra cache y rearranca.

### Debug con dape

- `M-x dape` -> elegir config.
- PHP: arrancar codigo con xdebug listener.
- Haskell: hdb autoport launch.
- Notas detalladas en `dape-howto-noobs.org`.

## Convenciones de codigo en config.el

- Secciones con banner de `═` y titulos en mayusculas.
- Funciones publicas con prefijo `my/` (nuevas) o nombre descriptivo
  (legacy). Hacia adelante: prefijo `my/` siempre para no chocar con
  Doom/paquetes.
- `after!` para configuracion lazy de paquetes Doom.
- `use-package!` para paquetes propios de packages.el.
- Comentarios en espanol informal pero sin acentos en el codigo
  (para evitar problemas de encoding viejos - los strings de UI
  si llevan acentos).

## Notas para Claude

- **Tienes daemon vivo a tu alcance**: usa `emacsclient --eval` para
  verificar TODO lo que toques (ver seccion "Control del daemon vivo").
  No reportes "deberia funcionar" sin verificar - se puede comprobar.
- Cuando edites `config.el`, ten en cuenta que Doom carga `custom.el`
  DESPUES. Setq en custom.el pisa lo de config.el.
- Si tocas init.el o packages.el, recordar a Pascual ejecutar
  `~/.config/emacs/bin/doom sync` antes de reiniciar.
- Para recargar config.el en vivo: `(load (expand-file-name "config.el"
  doom-user-dir))` via emacsclient. NO uses `(doom/reload)` desde shell.
- No commitear ni pushear sin pedir explicitamente.
- Stealth mode: commits SIN `Co-Authored-By` ni firmas de Claude.
- En PRs/issues del repo doom, Pascual es el unico author publico.

---

Ultima auditoria: 2026-05-19
Ultima actualizacion: 2026-05-19 (Fase 1 completa + doom upgrade)
Estado: licencia Intelephense sacada, Fase 1 cerrada y verificada en
daemon limpio. Doom upgrade pasado sin errores.
