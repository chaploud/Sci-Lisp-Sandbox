# 開発メモ

そもそもどこからscilispで記述すべきなのか想像がついてない
Rustの機能はフルに活かしたい
そこまで厳格な型システムを要求しないが、ポリモーフィズムを達成するために何かしらの工夫が必要

まず言語化できていない

激しい道のり
wasmのスタック操作までさかのぼって自作する
すべての型をwasmの範疇で定義する
⇒やりたくない

中程度の道のり
基本型に関してはRustのものを流用する
関数に関してもそれに乗っかる
Sci-Lispとの接続が難しい
RustInterOpを用意しておく
Rustの関数を呼び出して楽をする
コンパイルされて最適化されてアクセス不能にならないか？

- Code As Dataだからといって、いろいろな機能を高速に提供しようとしたら、普通の言語のようにキーワード特化のAST構成、パースをしなければならない。ので、うまい統一的なやり方ではなく個別撃破していくのだ。

- 今一度Doraの先読みをして、全体像を掴んでおくことは大切
- 説明を書き出してみよう
- 結合テストを先に書き出してみようか => 設計ができる



## やりたいことの言語化

- Rustで基本関数等定義をできるようにしておく
- WIT形式で取り扱いを始める
- Sci-Lispの世界へ入る

## JITの教科書

https://github.com/dinfuehr/dora

- 16万行
- 命令セットにいたるまですべて自前で実装している感じ
- bytecodeをwasmにすればいいと思う

### 処理の流れ

- どのように処理実行しているかの流れだけでもつかめ

- dora/src/main.rs
  - driver::start()
- dora/src/driver/start.rs
  - start()
    - cmd::parse_arguments()
    - compile_into_program()
      - Sema::new() ... Semaphoreかな? -> sm
      - Semaがメモリ上でのデータ構造すべてもっている感じか
    - language::check_program(); -> ast
    - language::generate_bytecode()
    - language::emit_program() -> program
      - language::emit_bytecode()
    - encode_and_decode_for_testing()
    - set_vm()
    - run_main()

- これをSci-Lispの実行に置き換えてみよう
- 実行時引数
  - REPL
  - Execute File
- semaphoreの上にlanguage::check_program -> ast
- semaphoreからgenerate_bytecode -> wasm
- wasmetimeで実行

- メモリアリーナ・アリーナメモリアロケータ
- まずはsemaを読んで、どのようにASTを保持したいのかを理解してみる

- 再帰の高速化、スタックオーバーフローしにくさ
  - 内部でloopに変換かな

## 必要なデータ構造・オブジェクト

入念に読んで理解してから実装に移ろう

- プログラムへの引数
- interner: インターンするもの
- source_file: ソースファイル情報
- diag: エラー分析して出すもの
- known: 既出のオブジェクト
  - functions
  - enums
  - structs
  - macros
- aliases: モジュールエイリアス、型エイリアス
- consts: constによるもの
- structs: structsによるもの
- extensions: わからん
- fcts: defnによるもの
- enums: enumによるもの
- globals: グローバル変数
- uses: importに該当するな
  - export表現がないからどうするか
- packages: 依存関係を表すパッケージ(まだ先か)
- package_names: パッケージの名前、ID
- prelude_module_id: Rustでpreludeというと、そのライブラリをがっと使えるようにするやつ
- stdlib_module_id: モジュールID
- program_module_id: プログラムのモジュールID->REPLでの表現は?一プロセスあたりに割り振られるかな？
- boots_module_id: コンパイラの方式
- stdlib_package_id: こっちはパッケージ
- program_package_id: 〃
- boots_package_id: 〃

## Arena

```rust
pub struct Arena<T, A = DefaultArenaBehavior<T>> {
  arena_id: u32,
  items: Vec<T>,
  _phantom: PhantomData<fn() -> A>
}
```

Sema構造体内のArenaからデータをidで高速に取り出すことができるっぽい
- メモリ上の構造を表す


## REPLのスペシャルコマンド

- %e: 直前のエラー
- %run: 外部モジュールの実行。結果格納
