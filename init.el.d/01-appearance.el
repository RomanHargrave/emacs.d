(use-package doom-themes
  :demand t
  :config
  (doom-themes-org-config))

(use-package doom-modeline
  :demand t
  :hook
  (after-init-hook . doom-modeline-mode)
  (after-make-frame-functions . (lambda (frame)
                                  (setq doom-modeline-icon t)))
  :config
  (setq doom-modeline-spc
        (propertize " " 'face '(doom-modeline)))

  (doom-modeline-def-segment buffer-position
    (concat
     (propertize (concat (format-mode-line '("%4l:%2c"))
                         " (" (number-to-string (point)) ")")
                 'face 'doom-modeline)))

  (customize-set-variable 'doom-modeline-minor-modes t)
  (customize-set-variable 'doom-modeline-enable-word-count t)
  (customize-set-variable 'doom-modeline-height 22)

  (add-to-list 'doom-modeline-continuous-word-count-modes 'latex-mode)

  ;; i'm going to replace the main modeline because i'm that person
  (doom-modeline-def-modeline 'main
    '(bar workspace-name window-number modals matches buffer-info remote-host buffer-position word-count parrot selection-info)
    '(objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs))

  (doom-modeline-def-modeline 'minimal
    '(bar matches buffer-info-simple)
    '(media-info major-mode))

  (doom-modeline-def-modeline 'special
    '(bar window-number modals matches buffer-info buffer-position word-count parrot selection-info)
    '(objed-state misc-info battery irc-buffers debug minor-modes input-method indent-info buffer-encoding major-mode process))

  (doom-modeline-def-modeline 'project
    '(bar window-number buffer-default-directory)
    '(misc-info battery irc mu4e gnus github debug minor-modes input-method major-mode process))

  (doom-modeline-def-modeline 'dashboard
    '(bar window-number buffer-default-directory-simple)
    '(misc-info battery irc mu4e gnus github debug minor-modes input-method major-mode process))

  (doom-modeline-def-modeline 'vcs
    '(bar window-number modals matches buffer-info buffer-position parrot selection-info)
    '(misc-info battery irc mu4e gnus github debug minor-modes buffer-encoding major-mode process))

  (doom-modeline-def-modeline 'package
    '(bar window-number package)
    '(misc-info major-mode process))

  (doom-modeline-def-modeline 'info
    '(bar window-number buffer-info info-nodes buffer-position parrot selection-info)
    '(misc-info buffer-encoding major-mode))

  (doom-modeline-def-modeline 'media
    '(bar window-number buffer-size buffer-info)
    '(misc-info media-info major-mode process vcs))

  (doom-modeline-def-modeline 'message
    '(bar window-number modals matches buffer-info-simple buffer-position word-count parrot selection-info)
    '(objed-state misc-info battery debug minor-modes input-method indent-info buffer-encoding major-mode))

  (doom-modeline-def-modeline 'pdf
    '(bar window-number matches buffer-info pdf-pages)
    '(misc-info major-mode process vcs))

  (doom-modeline-def-modeline 'org-src
    '(bar window-number modals matches buffer-info-simple buffer-position word-count parrot selection-info)
    '(objed-state misc-info debug lsp minor-modes input-method indent-info buffer-encoding major-mode process))

  (doom-modeline-def-modeline 'timemachine
    '(bar window-number matches git-timemachine buffer-position word-count parrot selection-info)
    '(misc-info minor-modes indent-info buffer-encoding major-mode)))

(use-package rainbow-delimiters
  :hook prog-mode-hook)

(use-package all-the-icons
  :demand t)

(use-package ligature
  :demand t
  :config
  (ligature-set-ligatures 't '("www"))
  (ligature-set-ligatures 'prog-mode
                          '(;; == === ==== => =| =>>=>=|=>==>> ==< =/=//=// =~
                            ;; =:= =!=
                            ("=" (rx (+ (or ">" "<" "|" "/" "~" ":" "!" "="))))
                            ;; ;; ;;;
                            (";" (rx (+ ";")))
                            ;; && &&&
                            ("&" (rx (+ "&")))
                            ;; !! !!! !. !: !!. != !== !~
                            ("!" (rx (+ (or "=" "!" "\." ":" "~"))))
                            ;; ?? ??? ?:  ?=  ?.
                            ("?" (rx (or ":" "=" "\." (+ "?"))))
                            ;; %% %%%
                            ("%" (rx (+ "%")))
                            ;; |> ||> |||> ||||> |] |} || ||| |-> ||-||
                            ;; |->>-||-<<-| |- |== ||=||
                            ;; |==>>==<<==<=>==//==/=!==:===>
                            ("|" (rx (+ (or ">" "<" "|" "/" ":" "!" "}" "\]"
                                            "-" "=" ))))
                            ;; \\ \\\ \/
                            ("\\" (rx (or "/" (+ "\\"))))
                            ;; ++ +++ ++++ +>
                            ("+" (rx (or ">" (+ "+"))))
                            ;; :: ::: :::: :> :< := :// ::=
                            (":" (rx (or ">" "<" "=" "//" ":=" (+ ":"))))
                            ;; // /// //// /\ /* /> /===:===!=//===>>==>==/
                            ("/" (rx (+ (or ">"  "<" "|" "/" "\\" "\*" ":" "!"
                                            "="))))
                            ;; .. ... .... .= .- .? ..= ..<
                            ("\." (rx (or "=" "-" "\?" "\.=" "\.<" (+ "\."))))
                            ;; -- --- ---- -~ -> ->> -| -|->-->>->--<<-|
                            ("-" (rx (+ (or ">" "<" "|" "~" "-"))))
                            ;; *> */ *)  ** *** ****
                            ("*" (rx (or ">" "/" ")" (+ "*"))))
                            ;; www wwww
                            ("w" (rx (+ "w")))
                            ;; <> <!-- <|> <: <~ <~> <~~ <+ <* <$ </  <+> <*>
                            ;; <$> </> <|  <||  <||| <|||| <- <-| <-<<-|-> <->>
                            ;; <<-> <= <=> <<==<<==>=|=>==/==//=!==:=>
                            ;; << <<< <<<<
                            ("<" (rx (+ (or "\+" "\*" "\$" "<" ">" ":" "~"  "!"
                                            "-"  "/" "|" "="))))
                            ;; >: >- >>- >--|-> >>-|-> >= >== >>== >=|=:=>>
                            ;; >> >>> >>>>
                            (">" (rx (+ (or ">" "<" "|" "/" ":" "=" "-"))))
                            ;; #: #= #! #( #? #[ #{ #_ #_( ## ### #####
                            ("#" (rx (or ":" "=" "!" "(" "\?" "\[" "{" "_(" "_"
                                         (+ "#"))))
                            ;; ~~ ~~~ ~=  ~-  ~@ ~> ~~>
                            ("~" (rx (or ">" "=" "-" "@" "~>" (+ "~"))))
                            ;; __ ___ ____ _|_ __|____|_
                            ("_" (rx (+ (or "_" "|"))))
                            ;; Fira code: 0xFF 0x12
                            ("0" (rx (and "x" (+ (in "A-F" "a-f" "0-9")))))
                            ;; Fira code:
                            "Fl"  "Tl"  "fi"  "fj"  "fl"  "ft"
                            ;; The few not covered by the regexps.
                            "{|"  "[|"  "]#"  "(*"  "}#"  "$>"  "^="))
  (global-ligature-mode t))

(global-display-line-numbers-mode)
(setq jit-lock-stealth-time 30)
(setq column-number-mode 1)
(setq frame-resize-pixelwise t)

(add-hook 'after-init-hook
	  (lambda ()
	    ;; Set up for clean frames
	    (toggle-scroll-bar -1)
	    (scroll-bar-mode -1)
	    (tool-bar-mode -1)
	    (menu-bar-mode -1)

	    ;; And configure theme and font
	    (load-theme 'doom-pine t)))

(add-hook 'after-make-frame-functions
	  (lambda (frame)
	    ;; Further frame cleanup
	    (modify-frame-parameters frame '((vertical-scroll-bars . nil)
					     (horizontal-scroll-bars . nil)
					     ;; (alpha . 98)
                                             ))

	    ;; Let terminal emulator choose the default bg
	    (unless (display-graphic-p (selected-frame))
	      (set-face-background 'defult "unspecified-bg" (selected-frame)))))
