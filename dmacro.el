;;
;;      dmacro.el - キー操作の繰返し検出 & 実行
;;
;;	Version 2.0
;;
;;      1993 4/14        original idea by 増井俊之＠シャープ
;;                         implemented by 太和田誠＠長岡技科大
;;                          refinement by 増井俊之＠シャープ
;;	1995 3/30 modified for Emacs19 by 増井俊之＠シャープ
;;
;;	2002 3              XEmacs対応 by 小畑英司 obata@suzuki.kuee.kyoto-u.ac.jp
;;                                        峰伸行 zah07175@rose.zero.ad.jp
;;
;;

;;
;; dmacro.el は、繰り返されるキー操作列から次の操作を予測し実行させる
;; ためのプログラムです。操作の繰返しの検出とその実行を指令するために
;; *dmacro-key* で指定する特別の「繰返しキー」を使用します。
;; 
;; 例えばユーザが
;;     abcabc
;; と入力した後「繰返しキー」を押すと、dmacro.el は "abc" の入力操作の
;; 繰返しを検出してそれを実行し、その結果テキストは
;;     abcabcabc
;; となります。また、
;;     abcdefab
;; と入力した後「繰返しキー」を押すと、dmacro.el はこれを "abcdef" の
;; 入力の繰返しと判断し、繰返しの残りの部分を予測実行して "cdef" を入力し、
;; テキストは
;;     abcdefabcdef
;; となります。ここでもう一度「繰返しキー」を押すと、"abcdef" の入力
;; が繰り返されて、テキストは
;;     abcdefabcdefabcdef
;; となります。
;; 
;; あらゆるキー操作の繰返しが認識、実行されるため、例えば
;;     line1
;;     line2
;;     line3
;;     line4
;; というテキストを
;;     % line1
;;     % line2
;;     line3
;;     line4
;; のように編集した後「繰返しキー」を押すとテキストは
;;     % line1
;;     % line2
;;     % line3
;;     line4
;; のようになり、その後押すたびに次の行頭に "% "が追加されていきます。
;; 
;; このような機能は、繰返しパタンの認識によりキーボードマクロを自動的に
;; 定義していると考えることもできます。キーボードマクロの場合は操作を
;; 開始する以前にそのことをユーザが認識してマクロを登録する必要があり
;; ますが、dmacro.el では実際に繰返し操作をしてしまった後でそのことに
;; 気がついた場合でも「繰返しキー」を押すだけでその操作をまた実行させる
;; ことができます。またマクロの定義方法(操作の後で「繰返しキー」を押す
;; だけ)もキーボードマクロの場合(マクロの開始と終了を指定する)に比べて
;; 単純になっています。
;;  
;; ● 使用例
;;  
;; ・文字列置換
;; 
;; テキスト中の全ての「abc」を「def]に修正する場合を考えてみます。
;; 「abc」を検索するキー操作は "Ctrl-S a b c ESC" で、これは
;; "DEL DEL DEL d e f" で「def」に修正することができます。
;; 引き続き次の「abc」を検索する "Ctrl-S a b c ESC" を入力した後で
;; 「繰返しキー」を押すと "DEL DEL DEL d e f" が予測実行され、新たに
;; 検索された「abc」が「def」に修正されます。ここでまた「繰返しキー」
;; を押すと次の「abc」が「def」に修正されます。
;; このように「繰返しキー」を押していくことにより順々に文字列を
;; 置換していくことができます。
;; 
;; ・罫線によるお絵書き
;; 
;; 繰返しを含む絵を簡単に書くことができます。例えば、
;;   ─┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐┌┐
;;     └┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘└┘
;; のような絵を書きたい場合は、keisen.el などを使って
;;   ─┐┌┐
;;     └┘
;; と書いた後で「繰返し」キーを押すと、
;;   ─┐┌┐
;;     └┘└┘
;; となり、もう一度「繰返しキー」を押すと
;;   ─┐┌┐┌┐
;;     └┘└┘└┘
;; となります。同様に
;;  ┌─┐┌─┐┌─┐┌─┐┌─┐┌─┐┌─┐┌─┐
;;  └─┘└─┘└─┘└─┘└─┘└─┘└─┘└─┘
;; のような絵も
;;  ┌─┐  ─
;;  └─┘
;; だけ入力した後「繰返しキー」を連続して押すだけで描くことができます。
;;  
;; ● 繰返し予測の方法
;;  
;; 入力の繰返しの予測手法はいろいろ考えられますが、dmacro.elでは
;; 以下のような優先度をもたせています。
;; 
;;  (1) 同じ入力パタンが予測の直前に2度繰返されている場合はそれを
;;      優先する。繰返しパタンが複数ある場合は長いものを優先する。
;; 
;;      例えば、「かわいいかわいい」という入力では「かわいい」と
;;      いうパタンが繰り返されたという解釈と、「い」というパタンが
;;      繰り返されたという解釈の両方が可能ですが、この場合
;;      「かわいい」を優先します。
;; 
;;  (2) (1)の場合にあてはまらず、直前の入力列<s>がそれ以前の入力列の
;;      一部になっている場合(直前の入力が<s> <t> <s>のような形に
;;      なっている場合)は、まず<t>を予測し、その次から<s> <t>を予測
;;      する。このとき<s>の長いものを優先し、その中では<t>が短いもの
;;      を優先する。
;; 
;;      例えば「abracadabra」という入力では、<s>=「abra」が最長なので
;;      <s> <t>=「cadabra」の予測が優先されます。
;;
;; ● XEmacs 対応、Super, Hyper, Alt キーの対応について
;;
;; この版では XEmacs にも対応しました。
;; 現在のところ GNU Emacs 18, 19, 20, 21, XEmacs 21 で
;; 動作することが確認できています。
;; また従来の dmacro では Super, Hyper, Alt のキー入力を
;; 正しく扱うことができませんでしたが、このバージョンでは
;; 扱えるようになっています。
;; 繰り返しのキーとして *dmacro-key* に Super, Hyper, Alt, Meta
;; を含めたキーを使うこともできますが、ただしその際は
;; 以下の注意に従って下さい。
;;
;; ● *dmacro-key* の指定
;;
;; GNU Emacs の場合
;;   Modifier key として Control のみが使われる場合は "\C-t" のような
;;   文字列として指定できます。Meta, Super, Hyper, Alt を利用する場合には
;;   それぞれ [?\M-t], [?\s-t], [?\H-t], [?\A-t] のように指定して下さい。
;;
;; XEmacs の場合
;;   Meta key を使う場合でも上記のような制限はありません。Super 等を使う
;;   場合には [(super t)] のように指定して下さい。
;;
;; ● 設定方法
;;
;;  .emacsなどに以下の行を入れて下さい。
;;
;; (defconst *dmacro-key* "\C-t" "繰返し指定キー")
;; (global-set-key *dmacro-key* 'dmacro-exec)
;; (autoload 'dmacro-exec "dmacro" nil t)
;;
;; オリジナルの連絡先:
;; 増井俊之
;; シャープ株式会社 ソフトウェア研究所
;; masui@shpcsl.sharp.co.jp
;;
;; 2002/6/3現在の連絡先:
;; 増井俊之
;; (株)ソニーコンピュータサイエンス研究所
;; masui@acm.org
;;

(defvar dmacro-array-type
  (if (and (boundp 'emacs-major-version)
	   (>= emacs-major-version 19))
      'vector 'string)
  "dmacro の内部で処理する配列の種類。
emacs 19 以上ならデフォルトで vector にする。
string では hyper, super, alt を含んだ入力の繰り返しが
正しく処理できないので注意。
GNU Emacs 18 (Nemacs) を使っている方以外は vector で問題ありません。")

(fset 'dmacro-concat
      (cond ((eq dmacro-array-type 'string) 'concat)
	    ((eq dmacro-array-type 'vector) 'vconcat)))
	    
(fset 'dmacro-subseq
      (cond ((featurep 'xemacs) 'subseq)
            ((and (eq dmacro-array-type 'vector)
                  (boundp 'emacs-major-version)
                  (eq emacs-major-version 19))
             (require 'cl)
             'subseq)
            (t 'substring)))
 
(defvar *dmacro-arry* nil "繰返しキー配列")
(defvar *dmacro-arry-1* nil "繰返しキーの部分配列")

(setq dmacro-key
      (cond ((eq dmacro-array-type 'string)
             *dmacro-key*)
            (t
             (let ((key *dmacro-key*))
               (cond ((featurep 'xemacs)
                      (if (arrayp key)
                          (mapvector 'character-to-event key)
                        (vector (character-to-event key))))
                     (t
                      (vconcat key)))))))

(setq dmacro-keys (dmacro-concat dmacro-key dmacro-key))

(defun dmacro-exec ()
  "キー操作の繰返しを検出し実行する"
  (interactive)
  (let ((s (dmacro-get)))
    (if (null s)
	(message "操作の繰返しが見つかりません")
      (execute-kbd-macro s)
      )
    ))

(defun dmacro-event (e)
  (cond
   ((integerp e) e)
   ((eq e 'backspace) 8)
   ((eq e 'tab) 9)
   ((eq e 'enter) 13)
   ((eq e 'return) 13)
   ((eq e 'escape) 27)
   ((eq e 'delete) 127)
   (t 0)
   ))

(defun dmacro-recent-keys ()
  (cond ((eq dmacro-array-type 'vector) (recent-keys))
	((eq dmacro-array-type 'string)
	 (let ((s (recent-keys)) )
	   (if (stringp s) s
	     (concat (mapcar 'dmacro-event s))
	     )))))

(defun dmacro-get ()
  (let ((rkeys (dmacro-recent-keys)) arry)
    (if (if (featurep 'xemacs)
            (let ((keys (vconcat dmacro-key
                                 (or *dmacro-arry-1* *dmacro-arry*)
                                 dmacro-key)))
              (equal keys
                     (subseq rkeys (- (length keys)))))
          (equal dmacro-keys (dmacro-subseq rkeys (- (length dmacro-keys)))))
        (progn
          (setq *dmacro-arry-1* nil)
          *dmacro-arry*)
      (setq arry (dmacro-search (dmacro-subseq rkeys 0 (- (length dmacro-key)))))
      (if (null arry)
          (setq *dmacro-arry* nil)
        (let ((s1 (car arry)) (s2 (cdr arry)))
          (setq *dmacro-arry* (dmacro-concat s2 s1)
                *dmacro-arry-1* (if (equal s1 "") nil s1))
          (setq last-kbd-macro *dmacro-arry*)
          (if (equal s1 "") *dmacro-arry* s1))
        ))))

(defun dmacro-search (array)
  (let* ((arry (dmacro-array-reverse array))
         (sptr  1)
         (dptr0 (dmacro-array-search (dmacro-subseq arry 0 sptr) arry sptr))
         (dptr dptr0)
         maxptr)
    (while (and dptr0
                (not (dmacro-array-search dmacro-key (dmacro-subseq arry sptr dptr0))))
      (if (= dptr0 sptr)
          (setq maxptr sptr))
      (setq sptr (1+ sptr))
      (setq dptr dptr0)
      (setq dptr0 (dmacro-array-search (dmacro-subseq arry 0 sptr) arry sptr))
      )
    (if (null maxptr)
        (let ((predict-arry (dmacro-array-reverse (dmacro-subseq arry (1- sptr) dptr))))
          (if (dmacro-array-search dmacro-key predict-arry)
              nil
            (cons predict-arry (dmacro-array-reverse (dmacro-subseq arry 0 (1- sptr)))))
          )
      (cons "" (dmacro-array-reverse (dmacro-subseq arry 0 maxptr)))
      )
    ))

(defun dmacro-array-reverse (arry)
  (dmacro-concat (reverse (mapcar 'identity arry))))

(defun dmacro-array-search (pat arry &optional start)
  (let* ((len (length pat))
	 (max (- (length arry) len))
	 p found
	 )
    (setq p (if start start 0))
    (while (and (not found) (<= p max))
      (setq found (equal pat (dmacro-subseq arry p (+ p len))))
      (if (not found) (setq p (1+ p)))
      )
    (if found p nil)
    ))


