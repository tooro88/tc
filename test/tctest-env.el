;;; tctest-env.el --- Setup environment for tctest. -*- lexical-binding: nil -*-

;; isearch 用設定を変えながら、T-Code 用 ert テストを繰り返し実行する
;; 際の、「.tc を書き換えて emacs 起動」の手間を減らすためのスクリプト。
;;
;; 環境変数 TCTEST_ENV=keyword1:keyword2:... の設定に従って T-Code の
;; 初期設定を行なう。ert テストの手動実行、または run.bashと共に、バッ
;; チテストの起動に用いる。
;;
;; 実行例: (run.bash の実行例も参照。)
;;  $ TCTEST_ENV=isadvice emacs --batch -L ~/tc -L ~/tc/test -l tctest-env
;;      動作 : (setq tcode-use-isearch 'advice) を設定した上でディレク
;;             トリ ~/tc 下の tc-setup、test/tctest をロードし、ert の
;;             テストをバッチモードで実行する。
;;
;;  $ TCTEST_ENV=isoverwrite emacs -Q -L ~/tc -L ~/tc/test -l tctest-env
;;      動作 : (setq tcode-use-isearch 'overwrite) を設定した上で
;;             tc-setup、tctest をロードする。--batch を指定していない
;;             ので、テストのバッチ実行は行なわれない。M-x ert で手動
;;             実行できる。(この場合、個人用設定がテストに影響しないよ
;;             う、-Q 推奨。)
;;
;; 必須キーワード(どれか一つを設定する):
;;   isnil       : tcode-use-isearch を nil        に設定する。
;;   isoverwrite : tcode-use-isearch を 'overwrite に設定する。
;;   isadvice    : tcode-use-isearch を 'advice    に設定する。
;;   isim        : tcode-use-isearch を 'im        に設定する。
;;   notc        : tc-setup をロードしない。
;;
;; オプションキーワード:
;;   nobatch  : バッチテストを実行しない。(デフォルトでは、emacs を
;;              --batch で起動するとバッチテストを自動で実行する。)
;;   debug    : debug-on-error を t に設定する。
;;   notctest : tctest.el をロードしない。
;;   uim      : tcode-use-input-method を t に設定する。
;;   nodefim  : tcode-use-as-default-input-method を nil に設定する。
;;   quiet    : バッチテストでの表示量を減らす。具体的には、ERT を
;;              quiet モード(summary だけを表示するモード。emacs-27 以
;;              降のみ)にした上で、ERT 以外のメッセージを極力表示しな
;;              いようにする。
;;
;;  FIXME: バッチモードで、単体テストを実行できるようにしたい。
;;  --batch で起動した emacs は、対話モードと動作が異なる場合がある
;;  (sit-for がsleep-for の動作になるなど)。テストのデバッグ用に単体実
;;  行機能が必要。
;;
;;  FIXME: .tc ファイルや ~/tcode ディレクトリの初期設定機能が必要かも。
;;  現状はユーザー任せ。tctest.el のテスト内容は、プレーンな設定を仮定
;;  している。

(defvar tcode-use-isearch)
(defvar tcode-use-input-method)
(defvar tcode-use-as-default-input-method)
(defvar tcode-verbose-message)
(defvar tctest-play-default-params)
(defvar ert-quiet)
(declare-function tctest-load-tc "tctest-play")

(defconst tctest-env-mandatory-keywords
  '("isim" "isadvice" "isoverwrite" "ist" "isnil" "notc")
  "isearch 実装の種別。TCTEST_ENV には、これらのうち一つを必ず指定する。")

(defun tctest-env-chk-mandatory (keywords)
  "必要なキーワードがセットされているかチェックする。"
  (let ((count 0))
    (dolist (w tctest-env-mandatory-keywords)
      (when (member w keywords)
	(setq count (1+ count))))
    (when (/= count 1)
      (error "Exactly one of %s must be specified as keyword"
	     tctest-env-mandatory-keywords))))

(defun tctest-env-msg (fmt &rest args)
  "バッファ、またはバッチモードの場合標準エラーにメッセージを出力する。"
  (let ((msg (apply #'format fmt args)))
    (if noninteractive
	(message msg) ; to stderr
      (insert msg "\n"))))

(defun tctest-env-quiet-p (keywords)
  (member "quiet" keywords))

(defun tctest-env-load-tc (keywords)
  "変数設定をした上で、tc-setup をロードする。"
  ;; tctest-env-chk-mandatory によるチェックにより、キーワード isim,
  ;; isadvice, isoverwrite, isnil, ist, notc は、これらのうちちょうど1
  ;; つだけが指定されている。
  (when (member "isim" keywords)
    (setq tcode-use-isearch 'im))
  (when (member "isadvice" keywords)
    (setq tcode-use-isearch 'advice))
  (when (member "isoverwrite" keywords)
    (setq tcode-use-isearch 'overwrite))
  (when (member "ist" keywords)
    (setq tcode-use-isearch t))
  (when (member "isnil" keywords)
    (with-eval-after-load ".tc"
      (setq tcode-use-isearch nil)))
  (when (member "nodefim" keywords)
    (with-eval-after-load ".tc"
      (setq tcode-use-as-default-input-method nil)))
  (when (member "uim" keywords)
    (setq tcode-use-input-method t))
  (when (tctest-env-quiet-p keywords)
    ;; .tc-record の読み込みメッセージを非表示に。
    (setq tcode-verbose-message nil)
    ;; この値が非 nil だと、文字入力のたびに tcode-input-method 内で
    ;; tcode-verbose-message が t にセットされてしまう。
    (setq input-method-verbose-flag nil))
  (unless (member "notc" keywords)
    (require 'tc-setup))
  (when (member "nodefim" keywords)
    ;; batch モードで [IMON] 時に input method を聞かれるのを防ぐ。
    (set-input-method "japanese-T-Code")))

(defun tctest-env-show-exprs (exprs)
  "リスト EXPRS の各要素を評価して値を表示する。"
  (dolist (expr exprs)
    (let ((val (if (symbolp expr)
		   (if (boundp expr)
		       (symbol-value expr)
		     'UNDEF)
		 (eval expr))))
      (tctest-env-msg "%S: %S" expr val))))

(defun tctest-env-run-test (keywords)
  "ert のバッチテストを実行する。"
  (when (and noninteractive
	     (not (member "nobatch" keywords)))
    (when (tctest-env-quiet-p keywords)
      (setq ert-quiet t)  ; ERT を quiet モードに (emacs-27 以降で有効)
      (puthash :quiet t tctest-play-default-params) ; テスト中の表示オフ
      (let ((inhibit-message t))
	;; tc-tbl.el のロードを、表示オフの状態で済ませおく。
	(tctest-load-tc)))
    (ert-run-tests-batch-and-exit)))

(defun tctest-env-main ()
  (let* ((keywords-packed (or (getenv "TCTEST_ENV")
			      (error "TCTEST_ENV not specified")))
	 (keywords (split-string keywords-packed ":")))
    (tctest-env-chk-mandatory keywords)
    (when (member "debug" keywords)
      (setq debug-on-error t))
    (let ((inhibit-message (tctest-env-quiet-p keywords)))
      (tctest-env-msg "dir: %s" default-directory)
      (tctest-env-msg "emacs-version: %s" emacs-version)
      (tctest-env-msg "keywords: %S" keywords)
      (tctest-env-load-tc keywords)
      (unless (or (member "notc" keywords)
		  (member "notctest" keywords))
	(require 'tctest))
      (tctest-env-show-exprs '(tcode-data-directory
			       tcode-use-isearch
			       tcode-use-input-method
			       tcode-use-as-default-input-method
			       (locate-library "tc-setup")
			       (featurep 'tc-is22)
			       (featurep 'tc-ishelper))))
    (tctest-env-run-test keywords)))

(tctest-env-main)
