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

