---
title: "LiquidHaskell 入門 その3"
date: 2019-04-29T18:37:53+09:00
draft: false
tags: ["Haskell", "形式検証"]
---

[前回の記事]({{< relref "liquidhaskell2" >}})ではLiquidHaskellを用いて簡単な言語の評価機を作ってみました。
今回はそれをStateモナドを用いて書き直すために必要な新しい機能である**Abstarct Refinement**, **Bounded Refinmenet**の解説をしていきたいと思います。

<!--more-->

# Abstract Refinement
[LiquidHaskell入門その1]({{< relref "liquidhaskell1" >}})で述べたように、LiquidHaskellでは量化子のない一階述語論理式を書くことができますが、それだけでは表現力が足りないケースが出てきます。
[公式ブログ](https://ucsd-progsys.github.io/liquidhaskell-blog/2013/06/03/abstracting-over-refinements.lhs/)にある例を引っ張ってみます。
次の関数```maxInt```を考えてみます。
{{< highlight Haskell >}}
maxInt :: Int -> Int -> Int
maxInt = if x >= y then x else y
{{< /highlight >}}
この関数のRefinement Typeとして、以下の全ての型は正しいです。
{{< highlight Haskell >}}
maxInt :: Nat -> Nat -> Nat
maxInt :: {v: Int | v < 10} -> {v: Int | v < 10} -> {v: Int | v < 10}
maxInt : Even -> Even -> Even
{{< /highlight >}}
しかしながら、型を一つ指定してしまうと、他の型としては扱えません。
{{< highlight Haskell >}}
{-@ maxInt :: Nat -> Nat -> Nat @-}
{-@ test :: Even @-}
test :: Int
test = maxInt 0 2
{{< /highlight >}}
{{< highlight bash >}}
$ stack exec -- liquid src/Liquid/AbstractRefinement.hs
...

**** RESULT: UNSAFE ************************************************************

/src/Liquid/AbstractRefienement.hs:126:1-18: Error: Liquid Type Mismatch

 126 | test = maxInt 2 0
       ^^^^^^^^^^^^^^^^^^

   Inferred type
     VV : {v : GHC.Types.Int | v >= 0}

   not a subtype of Required type
     VV : {VV : GHC.Types.Int | VV mod 2 == 0}

{{< /highlight >}}
それではこの関数につけるべき正しい型は何でしょうか？
Abstrace Refinement[*1]({{< ref "#ART" >}})は述語に対する全称量化を提供してくれます。
つまり、```maxInt```に対しては以下のような型を付けることができます。
{{< highlight Haskell >}}
{-@ maxInt :: forall <p :: Int -> Bool>. Int<p> -> Int<p> -> Int<p> @-}
maxInt :: Int -> Int -> Int
maxInt x y = if x >= y then x else y
{{< /highlight >}}
```forall <p :: Int -> Bool>```が全称量化された述語を導入しており、```Int<p>```は(感覚的には)```{v: Int | p v}```と等価です。(実際に右の書き方をすると```Sort Error```と怒られます。)
つまり、述語```p```を満たす```Int```型の値を2つ受け取って、```p```を満たす```Int```型の値を返すという型が```maxInt```にはつきました。
実際に使ってみましょう。
本来ならば```maxInt```を使うとき、述語を指定する必要がありそうですが、うまいこと推論してくれます。
{{< highlight Haskell >}}
{-@ test2 :: Even @-}
test2 :: Int
test2 = maxInt 2 0
{{< /highlight >}}
{{< highlight bash >}}
$ stack exec -- liquid src/Liquid/AbstractRefinement.hs
...

**** RESULT: SAFE **************************************************************
{{< /highlight >}}
データ型の宣言にもAbstract Refinementが使えます。以下では```List```型の宣言にAbstract Refinementを用いた例です。
テストでは述語としてすべての値が等しいというものを与えているので、```testListUnSafe```のみエラーを報告されています。
{{< highlight Haskell >}}
{-@ 
data List a <p :: a -> a -> Bool> where
    Nil :: List <p> a
  | Cons :: h:a -> List <p> (a<p h>) -> List <p> a
@-}
data List a where
  Nil :: List a
  Cons :: a -> List a -> List a

{-@ testListSafe :: List <{\s t -> s == t}> Int @-}
testListSafe :: List Int
testListSafe = Cons 1 $ Cons 1 Nil

{-@ testListUnSafe :: List <{\s t -> s == t}> Int @-}
testListUnSafe :: List Int
testListUnSafe = Cons 2 $ Cons 1 Nil
{{< /highlight >}}
{{< highlight bash >}}
$ stack exec -- liquid src/Liquid/AbstractRefinement.hs
...

**** RESULT: UNSAFE ************************************************************


 /src/Liquid/AbstractRefinement.hs:39:1-36: Error: Liquid Type Mismatch

 39 | testListUnSafe = Cons 2 $ Cons 1 Nil
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

   Inferred type
     VV : GHC.Types.Int

   not a subtype of Required type
     VV : {VV : GHC.Types.Int | ?a == VV}

   In Context
     ?a : GHC.Types.Int
{{< /highlight >}}

# Bounded Refinement
有界量化や型クラスのように、全称量化を得たらそのドメインを制限して情報量を増やすような仕組みが欲しくなります。
述語に対する全称量化であったAbstract Refinementに対し、そのドメインを制限するのがBounded Refinement[*2]({{< ref "#BRT" >}})です。
早速具体例を見ていきましょう。
関数合成```(.)```は以下のように書けます。
引数として2つの関数```f: (y:b -> c<p y>)```、```g: (z:a -> b<q z>)```を受け取って新しい関数```f . g: (a -> c<r x>)```を返すのですが、```r```の条件として```{x::a, w::b<q x> |- c<p w> <: c<r x>}```という制限を与えています。
つまり、事後条件が連鎖していきます。
実際に使ってみましょう。
```incr```を2回適用する関数を合成した```doubleIncr```は確かに検査されました。
{{< highlight Haskell >}}
{-@
(.) :: forall < p :: b -> c -> Bool
              , q :: a -> b -> Bool
              , r :: a -> c -> Bool
              >.
       {x::a, w::b<q x> |- c<p w> <: c<r x>}
       f:(y:b -> c<p y>)
    -> g:(z:a -> b<q z>)
    -> x:a -> c<r x>
@-}
(.) f g x = f (g x)

-- example
{-@ incr :: x: Int -> {y: Int | y = x + 1} @-}
incr :: Int -> Int
incr = (+) 1

{-@ doubleIncr :: x: Int -> {y: Int | y = x + 2} @-}
doubleIncr = incr . incr
{{< /highlight >}}

{{< highlight bash >}}
$ stack exec -- liquid src/Liquid/AbstractRefinement.hs
...

**** RESULT: SAFE **************************************************************
{{< /highlight >}}

# 補足: 構文に関するいくつかの注意点
* Abstract Refinementではあくまで述語の全称量化しかできないので、```forall <p :: Int -> Int>```みたいな書き方をするとコンパイルが通りません。
古い情報だと```forall <p :: Int -> Prop>```のように書かれていますが、これもコンパイルが通りません。
* 全称量化された述語の具体するときの構文についてはまとまった情報がありません。(Parserを読む気にはなりませんでした。)
[公式レポジトリのテスト](https://github.com/ucsd-progsys/liquidhaskell/tree/develop/tests)を読むなりして適当に推測しましょう。
今回の記事と次回の記事でもいくつか例を出していきます。
* Bounded Refinement周りの構文についてもまとまった情報がありません。これも次回の記事でいくつか具体例を出してフォローしていくつもりです。

# まとめ
今回はAbstract RefinementとBounded Refinementについて解説しました。
LiquidHaskellでは量化子のない一階述語論理式しか書けませんが、Abstract Refinementは述語の全称量化、Bounded Refinementは述語の有界量化を提供します。
これらによってLiquidHaskellの表現力が広がることを確認しました。

[次回]({{< relref "liquidhaskell4" >}})はこれらを用いてHoareモナドを実装し、前回の評価機をHoareモナドを用いてよりHaskellらしいコードに書き直します。

# 参考文献
- <a name="ART"> [Niki Vazou, Patrick M. Rondon, and Ranjit Jhala. Abstract Refinement Types. ESOP'13](https://ranjitjhala.github.io/static/abstract_refinement_types.pdf) </a>
- <a name="BRT"> [Niki Vazou, Alexander Bakst, Ranjit Jhala. Bounded Refinmenet Types. ICFP'15](https://arxiv.org/pdf/1507.00385.pdf) </a>