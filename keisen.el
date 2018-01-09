;; keisen.el -- provide facility for drawing ruled-line
;;
;; Copyright (C) 1990-2004 増井俊之  masui@pitecan.com
;;
;; This is a Public Domain Software.
;; Everyone is granted permission to copy, modify and redistribute
;; this program freely.
 
;; .emacsに以下のような記述を入れると矢印キーで罫線が引ける
;;
;; 文字端末の矢印キーで罫線を引く場合
;; (global-set-key "\eOA" 'keisen-up-move)
;; (global-set-key "\eOB" 'keisen-down-move)
;; (global-set-key "\eOD" 'keisen-left-move)
;; (global-set-key "\eOC" 'keisen-right-move)
;;
;; 文字端末のMeta-Pなどで罫線を引く場合
;; (global-set-key "\M-p" 'keisen-up-move)
;; (global-set-key "\M-n" 'keisen-down-move)
;; (global-set-key "\M-b" 'keisen-left-move)
;; (global-set-key "\M-f" 'keisen-right-move)
;;
;; Control+矢印キーで罫線を引く場合
;; (global-set-key [C-right] 'keisen-right-move)
;; (global-set-key [C-left]  'keisen-left-move)
;; (global-set-key [C-up]    'keisen-up-move)
;; (global-set-key [C-down]  'keisen-down-move)
;;
;; (autoload 'keisen-up-move "keisen" nil t)
;; (autoload 'keisen-down-move "keisen" nil t)
;; (autoload 'keisen-left-move "keisen" nil t)
;; (autoload 'keisen-right-move "keisen" nil t)
 
;;; 92.7.6   modified for Mule Ver.0.9.5 by T.Shingu <shingu@cpr.canon.co.jp>
;;; 92.7.13  modified for Mule Ver.0.9.5 by K.Handa <handa@etl.go.jp>
;;; 93.8.5   modified for dmacro.el by T.Masui <masui@shpcsl.sharp.co.jp>
;;; 93.8.5   modified for Mule Ver.1.1 by K.Handa <handa@etl.go.jp>
;;;	To be used also with Nemacs.
;;; 2004.5.6 modified for Emacs21 by T.Masui <masui@pitecan.com>

(provide 'keisen)
(require 'picture)
 
(defconst keisen-right 1)
(defconst keisen-up 2)
(defconst keisen-left 4)
(defconst keisen-down 8)
 
(defconst keisen-table "\
＊＊＊└＊─┘┴＊┌│├┐┬┤┼\
＊＊＊＊＊＊＊＊＊＊┝＊＊＊＊＊\
＊＊＊＊＊┸＊＊＊＊＊＊＊＊＊＊\
┗＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊\
＊＊＊＊＊＊＊＊＊＊┥＊＊＊＊＊\
━＊┷＊＊＊＊＊┯＊┿＊＊＊＊＊\
┛＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊\
┻＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊\
＊＊＊＊＊┰＊＊＊＊＊＊＊＊＊＊\
┏＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊\
┃┠＊＊┨╂＊＊＊＊＊＊＊＊＊＊\
┣＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊\
┓＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊\
┳＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊\
┫＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊\
╋＊＊＊＊＊＊＊＊＊＊＊＊＊＊＊"
  "罫線キャラクタの各方向の枝の有無を8ビットで表現する。
インデックスの上位4ビットは太い線の有無を示し、下位4ビットが
細い線の有無を示す。")
 
(defvar keisen-width 1
  "罫線の太さ。1のとき細く、2以上のとき太い。")
 
(defun keisen-toggle-width ()
  "罫線の太さを切り換える"
  (interactive)
  (cond
   ((> keisen-width 1) (message "細い罫線を使用します") (setq keisen-width 1))
   (t (message  "太い罫線を使用します") (setq keisen-width 2))
   ))
 
(defun keisen-opposite-direction (dir)
  (cond
   ((= dir keisen-right) keisen-left)
   ((= dir keisen-left) keisen-right)
   ((= dir keisen-up) keisen-down)
   ((= dir keisen-down) keisen-up)
   (t 0)
   ))
 
(defun keisen-direction (command)
  (cond
   ((eq command 'keisen-right-move) keisen-right)
   ((eq command 'keisen-left-move) keisen-left)
   ((eq command 'keisen-up-move) keisen-up)
   ((eq command 'keisen-down-move) keisen-down)
   ((eq command t) keisen-last-direction) ; 93.8.5 by T.Masui
   (t 0)))
 
(defun keisen-new-string ()		; 92.7.13 by K.Handa -- Big change
  (let (pos factor str old-direction new-direction)
    (setq old-direction (keisen-direction last-command))
    (setq new-direction (keisen-direction this-command))
    (setq keisen-last-direction new-direction) ; 93.8.5 by T.Masui
    (setq factor (if (> keisen-width 1) 16 1))
    (setq str (if (eobp) " "
		(buffer-substring (point) (+ (point) 1))))
    (setq pos (string-match str keisen-table))
    (if (null pos)
	(progn
	  (setq pos 0)
	  (if (= old-direction (keisen-opposite-direction new-direction))
	      (setq old-direction new-direction))
	  (if (= old-direction 0) (setq old-direction new-direction))
      ))
    (setq pos (logior pos
		      (* (keisen-opposite-direction old-direction) factor)
		      (* new-direction factor)))
    (substring keisen-table pos (+ pos 1))
    ))

(defun keisen-move (v h)
  (setq picture-vertical-step v)
  (setq picture-horizontal-step h)
  (setq picture-desired-column (current-column))
  (picture-insert (string-to-char (keisen-new-string)) 1)
  )

(defun keisen-right-move ()
  "罫線を引きながら右方向に移動する" 
  (interactive)
  (keisen-move 0 1))
			  
(defun keisen-left-move ()
  "罫線を引きながら左方向に移動する"
  (interactive)
  (keisen-move 0 -1))

(defun keisen-up-move ()
  "罫線を引きながら上方向に移動する"
  (interactive)
  (keisen-move -1 0))

(defun keisen-down-move ()
  "罫線を引きながら下方向に移動する"
  (interactive)
  (keisen-move 1 0))
