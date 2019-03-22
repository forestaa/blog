---
title: "LiquidHaskell 入門 その1"
date: 2019-03-22T15:53:19+09:00
draft: false
tags: ["Haskell", "形式検証"]
---

今回から何回かに分けて **LiquidHaskell**と**F***の比較記事を書きたいと思います。
数か月前にいつか触ろうと思っていたLiquidHaskellを試す機会がありました。
やりたかったことがLiquidHaskellだけでは達成できなかったのでF*も試してみました。
その時に見つけた差異を記事にしてみます。
先にネタバレしておくと、HoareモナドはLiquidHaskellではできない?けどF*なら当たり前のように使えるよねっていう話なので、面白そうと思う方は読んでいただけると嬉しいです。
今回の記事では、LiquidHaskellの基礎をまとめることにします。
Haskellを触ったことがある人なら読める内容にしているつもりです。
<!--more-->

...というのが当初の予定だったのですが、HoareモナドもLiquidHaskellで書けることが分かりました。
比較をどうするかは今後考えますが、とりあえず数回LiquidHaskellの入門記事を書きます。

# LiquidHaskellとは
LiquidHaskellとはRefinement Typeを用いたHaskell用の検証ツールです。
Refinement Typeとは簡単に言うと、型に述語論理を組み合わせたようなものです。
簡単な具体例を見てみましょう。
以下は0でない整数の型をRefinement Typeを用いて表現したものです。

{{< highlight Haskell >}}
data NonZero = {v: Int | v /= 0}
{{< /highlight >}}

このように、内包表記によって既存の型に述語論理式を組み合わせて、Haskellの型より細かい型を与えることができます。
特に、Haskellの関数の上に事前条件、事後条件を記述することができるようになり、より厳密な検証を可能にします。
述語論理部分はz3などの、既存のSMTソルバーを用いて検証を行っています。
また、LiquidHaskellは新しい言語ではなく、Haskellのコメントとしてアノテーションを書くため、既存のコードを壊さずに導入することができます。

LiquidHaskellの公式レポジトリは [https://github.com/ucsd-progsys/liquidhaskell](https://github.com/ucsd-progsys/liquidhaskell) です。
ここに(分かりやすいかどうかは置いておいて)様々な情報が書いてあるので、もっと詳しく知りたくなったらここを参照してください。

# インストール
基本的には[INSTALL.md](https://github.com/ucsd-progsys/liquidhaskell/blob/develop/INSTALL.md)に従えばよいです。
ここでは同じことを書いておきます。

LiquidHaskellを使うためにはLiquidHaskell本体のほかにSMTソルバーが必要です。
今回は[z3](https://github.com/Z3Prover/z3)をインストールします。
Arch Linuxでは以下のコマンドでインストールできます。
各OS, ディストリビューションでのインストール方法は各自ググってぐださい。
[releaseページ](https://github.com/Z3Prover/z3/releases)にバイナリも置いてあります。
{{< highlight bash >}}
$ sudo pacman -S z3
$ z3 --version 
Z3 version 4.8.4 - 64 bit 
{{< /highlight>}}

LiquidHaskellのインストールはソースからビルドするのが安定です。
{{< highlight bash >}}
$ git clone --recursive https://github.com/ucsd-progsys/liquidhaskell.git
$ cd liquidhaskell
$ stack install
$ stack exec -- liquid --version 
LiquidHaskell Version 0.8.4.0, Git revision ea8217bcdd78e550ef2b39237dc83fe2f1122b69 
Copyright 2013-18 Regents of the University of California. All Rights Reserved.
{{< /highlight >}}

# 例1: 整数の除算
簡単な例から始めてみましょう。
題材としては```div```関数を用います。

{{< highlight Haskell >}}
div :: Int -> Int -> Int
{{< /highlight>}}

この関数は整数の除算を行いますが、第2引数が0のときはランタイムエラーを吐いてしまいます。
ランタイムエラーはできるだけ避けたいため、以下の型を持つ```safeDiv```を使用する場合もあるかもしれません。

{{< highlight Haskell >}}
safeDiv :: Int -> Int -> Maybe Int
{{< /highlight>}}

これによってランタイムエラーを避けることができます。
その代わりに、エラーが起きることを考慮してコードを書く必要ができ、負担が増加します。
できるだけ安全なコードを書きたいのですが、第2引数に0が来ないことが静的に分かる場合でも```safeDiv```を使うことは避けたいです。
しかし、黙って```div```を使うのも後ろめたいですし、そもそも第2引数に0が来ないことの正しさは別に示す必要があります。

このような場合にLiquidHaskellを使ってみましょう。
とりあえず実験用のディレクトリを```stack new liquid-tutorial```で作って```liquid-tutorial/src/Lib.hs```に以下の関数を加えてください。

{{< highlight Haskell >}}
{-@ checkedDiv :: Int -> {v: Int | v /= 0} -> Int @-}
checkedDiv :: Int -> Int -> Int
checkedDiv = div
{{< /highlight >}}

下2行は普通のHaskellのコードです。
1行目の```{-@ ... @-}```がLiquidHaskell用のコードです。
Haskellでは```{- ... -}```はコメントと見なされるので、通常のコンパイルには問題ないです。
1行目は```checkedDiv```に対してRefinement Typeを用いて型を与えています。
第2引数の```{v: Int | v /= 0}```に注目してください。
これは先ほどの例で用いた0でない整数を表しています。
このように、関数```checkedDiv```は第2引数は0でないということを事前条件として要求します。

それでは実際にLiquidHaskellで検査してみます。0除算を行う以下の関数を```liquid-tutorial/src/Lib.hs```に加えてください。

{{< highlight Haskell >}}
test = 1 `checkedDiv` 0
{{< /highlight >}}

それでは、```stack exec -- liquid src/Lib.hs```のコマンドを実行してソースコードを検査してみてください。
(```liquid src/Lib.hs```でもいいのですが、stackプロジェクトの場合は```stack exec```を通すことによってstackプロジェクトのライブラリが参照可能になります。)

{{< highlight bash >}}
$ stack exec -- liquid src/Lib.hs
...

**** RESULT: UNSAFE ************************************************************


 /src/Lib.hs:8:9-21: Error: Liquid Type Mismatch

 8 | test = 1 `checkedDiv` 0
             ^^^^^^^^^^^^^

   Inferred type
     VV : {v : GHC.Types.Int | v == 0}

   not a subtype of Required type
     VV : {VV : GHC.Types.Int | VV /= 0}

{{< /highlight >}}

UNSAFEという結果が出ました。
第2引数は```{v: GHC.Types.Int | v == 0}```という型に推論されていて、```{VV : GHC.Types.Int | VV /= 0}```という型の部分型にならないと言われています。
ここでいう部分型というのは、$\\{ x: a \mid P\,x \\} \le \\{ x: a \mid Q\,x \\} \iff  P\,x \Rightarrow Q\,x$でだいたい定義されています。
このように```checkedDiv```に```0```を適用していることを、ランタイムエラーではなく型検査エラーとして検出することができます。
おかげで黙って```div```を使う代わりに```checkedDiv```を使ってLiquidHaskellで検証することによって、```safeDiv```を使わずに安全性を保証することができました。

# 例2: 長さ付きベクトル
次の例は長さ付きベクトルです。
```head```のように空リストに対して適用するとランタイムエラーを起こすような関数に対する解決策としてよく出てきます。
依存型のあるプログラミング言語の説明でもよく出てくる例ですね。
Haskellでも型レベル自然数を使うと実装することもできます。
LiquidHaskellを使ってもできるということを見ていきましょう。
LiquidHaskellを用いれば、既存のコードそのままで長さが必要なところだけアノテーションをつけて検証することができます。
長さ付きベクトルは以下のように定義できます。


{{< highlight Haskell >}}
{-@ type ListN a N = {v:[a] | len v = N} @-}
{{< /highlight>}}

ここではLiquidHaskellの型エイリアスを用いて定義しています。
ここで```len```のような関数は、LiquidHaskellでは```measure```と呼ばれています。
```measure```は通常のHaskellの関数で、以下のように```measure```と宣言することで述語論理式の中に書くことができるようになります。

{{< highlight Haskell >}}
{-@ measure len @-}
{-@ len :: forall a. [a] -> GHC.Types.Int @-}
len :: [a] -> Int
len []     = 0
len (y:ys) = 1 + len ys
{{< /highlight >}}

従って、先ほど定義した```ListN a N```は長さNのリストを表しています。
これを用いていくつか関数を定義してみます。

{{< highlight Haskell >}}
{-@ map :: (a -> b) -> l: [a] -> ListN b (len l) @-}
map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (a:as) = f a : map f as

{-@ reverse :: as: [a] -> ListN a (len as) @-}
reverse :: [a] -> [a]
reverse xs = go [] xs
  where
    {-@ go :: as: [a] -> bs: [a] -> {cs: [a] | len as + len bs = len cs } @-}
    go acc [] = acc
    go acc (x:xs) = go (x:acc) xs

{-@ dot :: Num a => v: [a] -> ListN a (len v) -> a @-}
dot :: Num a => [a] -> [a] -> a
dot [] [] = 0
dot (a:as) (b:bs) = a * b + dot as bs
{{< /highlight >}}

```map```関数は引数のリストと同じ長さのリストを返すという条件が事後条件として指定されています。
```map :: (a -> b) -> l: [a] -> ListN b (len l)```という型に注目してください。
依存型のある言語と同様に、返り値の型が引数の項に依存しているような型を与えることもできます。(依存関数型と呼ばれます。)
```reverse```も引数のリストと同じ長さのリストを返すことが事後条件として指定されています。
```dot```はベクトルの内積を取る関数です。
この関数は事前条件として引数のリストは同じ長さであることを要求します。
また、この関数はパターンマッチが不十分ですが(引数の片方のみ空リストの場合がない)、LiquidHaskellによってそのようなパターンマッチは起きることがなく、問題がないことが保証されます。
つまり、ここでも余計なMaybeモナド等を付ける必要がなくなりました。
型で安全性が保障された行列の積も同様に定義することができます。
```dot```関数を```test2 = dot [1, 2, 3] [4, 5]```でテストしてみます。
以下のように確かに型検査が失敗したことが分かります。
{{< highlight bash >}}
$ stack exec -- liquid src/Lib.hs
...

**** RESULT: UNSAFE ************************************************************
...

{{< /highlight >}}

# 例3: 停止性検査
LiquidHaskellはデフォルトでは関数に対して停止性を保証する必要があります。
いくつかの場合では自動で停止性を検証しようとします。
上で用いたリストの関数の例は、再帰呼び出しのたびに引数のリストの長さが単調減少するため自動で停止性が検査されていました。
ここでは自動検証に失敗した場合にどうしたらいいかを見ていきます。
以下の、2つの自然数を受け取ってそれらの最大公約数を返す関数```gcd```について考えてみましょう。

{{< highlight Haskell >}}
{-@ gcd :: m: Nat -> n: Nat -> Nat @-}
gcd :: Int -> Int -> Int
gcd m n
  | n == 0    = m
  | m >= n    = gcd n (m `mod` n)
  | otherwise = gcd n m
{{< /highlight >}}

これをLiquidHaskellでコンパイルすると以下のエラーを受けます。
{{< highlight bash >}}
$ stack exec -- liquid src/Lib.hs
...

**** RESULT: UNSAFE ************************************************************

src/Lib.hs:54:17-21: Error: Liquid Type Mismatch

 54 |   | m >= n    = gcd n (m `mod` n)
                      ^^^^^

   Inferred type
     VV : {v : GHC.Types.Int | v >= 0
                               && v == n}

   not a subtype of Required type
     VV : {VV : GHC.Types.Int | VV >= 0
                                && VV < m}

   In Context
     m : {m : GHC.Types.Int | m >= 0}

     n : {n : GHC.Types.Int | n >= 0}
...

{{< /highlight >}}

今、型として書かれているのは```gcd :: m: Nat -> n: Nat -> Nat```だけなので、本来ならば自然数であることが示せていれば十分なはずですが、身に覚えのないエラーが出ています。
このエラーは停止性の検査に関わるエラーです。
身に覚えのないエラーが出てきたら、まずLiquidHaskellが停止性を検証しようとして失敗したのではないかとに疑ってみましょう。
ちなみに```{-@ lazy gcd @-}```と書いておくと停止性の検査をスキップすることができます。

それでは改めて先ほどのエラーメッセージを読んでみましょう。
```Inferred type```に書かれているのは第1引数の型の推論結果です。
成り立っている条件が全部書かれています。
```not a subtype of Required type```に書かれているのは第1引数が満たさなければいけない型ですが、よく見ると```W < m ```と書かれています。
LiquidHaskellはデフォルトでは、再帰関数の引数の一番初めの自然数が単調減少していないかどうか検査します。
つまり、今回は```n```が```m```より小さくなっていないかどうかを検査しています。
```m == n```の場合がありうるので、検査が通っていなかったわけです。

それでは単調減少するメトリックを与えてみましょう。
先ほどのコードは少し考えると第2引数が単調減少していることが分かります。
```gcd```の型として```{-@ gcd :: m: Nat -> n: Nat -> Nat / [n] @-}```を与えてみましょう。
右端の```/ [n]```がメトリックを与えています。
```n```には適当な項が書けます。(例えば```m+n```とか)
これでコンパイルすると、
{{< highlight bash >}}
$ stack exec -- liquid src/Lib.hs
...

 

**** RESULT: SAFE **************************************************************
...

{{< /highlight >}}
無事に停止性の検査に成功し、コンパイルできました。

# 補足: 構文について
ここで、気になる人のために述語部分に書くことができる論理式の構文を確認しておきます。
線形算術、未解釈関数を含む、量化子のない一階述語述語論理式を書くことができます。
自動検証できるよう決定可能な範囲に絞っているため、証明は基本的に必要がありません。
以下の構文のソースは[README.md](https://github.com/ucsd-progsys/liquidhaskell)です。

{{< highlight bnf >}}
c ::= 0, 1, 2, ...
v ::= x, y, z, ...          
e ::= v                     -- variable
    | c                     -- constant
    | e + e                 -- addition
    | e - e                 -- subtraction
    | c * e                 -- multiplication by constant
    | v e1 ... en           -- uninterpreted function application
    | if p then e else e    -- if-then-else
r ::= ==
    | /=
    | >=
    | <=
    | >
    | <
p ::= true
    | false
    | e r e                 -- atomic binary relation
    | v e1 ... en           -- predicate application
    | p && p                -- and
    | p || p                -- or
    | p => p                -- implies
    | not p                 -- negation
{{< /highlight >}}


# まとめ
今回はLiquidHaskellの基礎を確認しました。
具体例を列挙する形で説明したので、最後にポイントを以下にまとめます。

- Refinement Typeにより、LiquidHaskellはHaskellの型より細かい検証を行える。
  - 特に関数の事前条件・事後条件の指定
- Haskellのコードにコメントとしてアノテーションを付けて検証できるため、既存のコードに導入できる。
- 依存関数型も書ける・停止性の検査も可能
- 言語を決定可能な範囲にしているため、自動検証が可能で基本的に証明を書く必要がない。

次回は今回説明したことを用いて、スコープを持つ簡単な言語の評価関数を作ってみたいと思います。
