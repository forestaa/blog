---
title: "LiquidHaskell 入門 その4"
date: 2019-04-29T18:38:36+09:00
draft: false
tags: ["Haskell", "形式検証"]
---


[前回の記事]({{< relref "liquidhaskell3" >}})はLiquidHaskellの表現力を高める機能であるAbstract RefinementとBounded Refinementの解説をしました。
今回は[前々回]({{< relref "liquidhaskell2" >}})のナイーブなコードを**Hoareモナド**を用いてHaskellらしいコードに書き直してみます。
Abstract RefinementとBounded Refinementをガンガン使っていきます。
<!--more-->

# LiquidHaskellとStateモナド
LiquidHaskellによってスコープ付き環境とそれらの関数に対してより細かい型をつけ、実際に誤った使い方をしたときに型検査に失敗するところまで確認しました。
LiquidHaskellは確かに有用であることが分かりました。
しかし、[前々回]({{< relref "liquidhaskell2" >}})のコードを見たHaskellerのみなさんはもどかしい気持ちになったのではないでしょうか。
```eval```の型をもう一度見直してみましょう。
{{< highlight Haskell >}}
eval :: (MonadWriter [Int] m, MonadError EvalException m) => Stm -> Env Int -> m ((), Env Int)
evalExp :: (MonadWriter [Int] m, MonadError EvalException m) => Exp -> Env Int -> m (Int, Env Int)
{{< /highlight>}}
この型はStateモナドですので、本来なら以下のように書きたいです。
{{< highlight Haskell >}}
eval :: (MonadWriter [Int] m, MonadError EvalException m) => Stm -> StateT (Env Int) m ()
evalExp :: (MonadWriter [Int] m, MonadError EvalException m) => Exp -> StateT (Env Int) m Int
{{< /highlight>}}
しかしながらそのままStateモナドを用いると、状態の事前条件と事後条件について何も言えません。
実際に```endScope```を呼んでみると、型検査に失敗します。
```endScope```を呼ぶための事前条件を状態が満たしていることを保証できないためです。
{{< highlight Haskell >}}
eval' :: (MonadWriter [Int] m, MonadError EvalException m) => Stm -> StateT (Env Int) m ()
...

evalExp' :: (MonadWriter [Int] m, MonadError EvalException m) => Exp -> StateT (Env Int) m Int
...
evalExp' (EScope s e) = do
  modify beginScope
  eval' s
  v <- evalExp' e
  modify endScope
  return v
{{< /highlight>}}
{{< highlight bash >}}
$ stack exec -- liquid src/Liquid/Env.hs
...
**** RESULT: UNSAFE ************************************************************


/src/Liquid/Env.hs:154:3-17: Error: Liquid Type Mismatch

 154 |   modify endScope
         ^^^^^^^^^^^^^^^

   Inferred type
     VV : {v : (Liquid.Env.Env GHC.Types.Int) | Liquid.Env.scopeNum v >= 0}

   not a subtype of Required type
     VV : {VV : (Liquid.Env.Env GHC.Types.Int) | Liquid.Env.scopeNum VV > 0}

{{< /highlight >}}

# Hoareモナド
Stateモナドに事前条件と事後条件をつけたようなものはNanevskiらの**Hoareモナド**として知られています。
詳しくはAleksandar Nanevski, Greg Morrisett and Lars Birkedal. Hoare Type Thoery, Polymorphism and Separation. JFP 2007.を参照してください。
LiquidHaskell上で実装すると以下のようになります。
前回解説したAbstract Refinementを用いています。
通常のStateモナドに2つの述語パラメータ```p :: s -> Bool```、```q :: a -> s -> s -> Bool```を加えています。
```p :: s -> Bool```が状態の事前条件、```q :: a -> s -> s -> Bool```が事後条件を指定しています。
事後の状態```s<q x v>```は事前の状態```v: s<p>```と返り値```x::a```に依存しているため、事前の状態に対する相対的な条件も指定することが可能です。
例えば、```v' = v + 1```みたいな条件を書くことができます。
{{< highlight Haskell >}}
{-@ data Hoare s a <p :: s -> Bool, q :: a -> s -> s -> Bool> = Hoare (v: s<p> -> (x::a, s<q x v>)) @-}
data Hoare s a = Hoare (s -> (a, s))
{{< /highlight >}}
Stateモナド周りの関数に対するRefinement Typeは以下のようになります。
```runHoare```はStateモナドの中身を抽出、```put```は状態を引数と同じにする、```get```は状態をそのままに状態を返すという条件を表しています。
```modify```は状態に対して引数```f```を適用し、事前条件・事後条件を```f```と同じものにするという条件を表しています。
```return```は状態を変更せず、返り値を引数にするという条件を表しています。
```(>>=)```は少し大変です。
```p :: s -> Bool```、```q :: a -> s -> s -> Bool```は通常のHoareモナドのパラメータです。
```pp :: a -> s -> Bool```は第二引数の事前条件に関するパラメータなのですが、第一引数の返り値に依存することができます。
```qq :: b -> s -> s -> Bool```は通常の事後条件に関するパラメータです。
また、Bounded Refinementを用いて述語パラメータに制限を加えています。
Bounded Refinementの制約は2つ以上書くことができます。
1つ目の制約は第一引数の事後条件が第二引数の事前条件を満たすことを表しています。
2つ目の制約は第二引数の事後条件が最終的な事後条件を満たすことを表しています。
{{< highlight Haskell >}}
{-@ runHoare :: forall <p :: s -> Bool, q :: a -> s -> s -> Bool>. Hoare <p, q> s a -> v:s<p> -> (x::a, s<q x v>) @-}
runHoare :: Hoare s a -> s -> (a, s)
runHoare (Hoare f) = f

{-@ put :: forall <p :: s -> Bool>. v:s -> Hoare <p, {\x s t -> v == t}> s () @-}
put :: s -> Hoare s ()
put s = Hoare (const ((), s))

{-@ get :: forall <p :: s -> Bool>. Hoare <p, {\x s t -> x == t && s == t}> s s @-}
get :: Hoare s s
get = Hoare (\s -> (s, s))

{-@ modify :: forall <p :: s -> Bool
                    , q :: s -> s -> Bool>. 
                 (x: s<p> -> {v: s<q x> | true }) 
              -> Hoare <p, \_ t -> { v: s<q t> | true }> s () @-}
modify :: (s -> s) -> Hoare s ()
modify f = Hoare (\s -> ((), f s))

{-@ return :: forall <p :: s -> Bool>. y:a -> Hoare <p, {\x s t -> x == y && s == t}> s a @-}
return :: a -> Hoare s a
return a = Hoare (\s -> (a, s))

{-@ 
(>>=) :: forall < p  :: s -> Bool
                , q  :: a -> s -> s -> Bool
                , pp :: a -> s -> Bool
                , qq :: b -> s -> s -> Bool
                , r  :: b -> s -> s -> Bool>. 
            {x::a, ss::s<p> |- s<q x ss> <: s<pp x>}
            {x::a, y::b, ss::s<p>, sss::s<q x ss> |- s<qq y sss> <: s<r y ss>}
            Hoare <p, q> s a
         -> (x:a -> Hoare <{ v: s<pp x> | true }, qq> s b)
         -> Hoare <p, r> s b
@-}
(>>=) :: Hoare s a -> (a -> Hoare s b) -> Hoare s b
m >>= k = Hoare (\s -> let (a, s') = runHoare m s in runHoare (k a) s')
{{< /highlight >}}

# evalのHoareモナドを用いたリファクタリング
それでは[第二回]({{< relref "liquidhaskell2" >}})の```eval```をHoareモナドを用いて書き直していきます。
monad transformerには適用ができなかったため、```SPrint```を削ったり、未定義の変数に対しては```undefined```を返してサボっています。
現状型クラスへの抽象化とも相性が悪いみたいです。
Monad型クラスのインスタンスにしてdo構文を使うと、せっかく定義したRefinement Typeを使えなくなってしまいます。(おそらくMonad型クラスの```(>>=)```のRefinement Typeを参照してしまっているためです。)
いくつかの難点はあるものの、Hoareモナドによって状態を隠ぺいしたコードに書き直すことに成功しました。
{{< highlight Haskell >}}
data Stm = SSeq Stm Stm
         | SAssign Id Exp
data Exp = EVar Id
         | EInt Int
         | EPlus Exp Exp
         | EScope Stm Exp

{-@ lazy eval @-}
{-@ eval :: forall <p :: Env Int -> Bool>. Stm -> Hoare <p, {\_ e0 e1 -> scopeNum e0 == scopeNum e1}> (Env Int) ()  @-}
eval :: Stm -> Hoare (Env Int) ()
eval (s1 `SSeq` s2) = eval s1 >>= \_ -> eval s2
eval (x `SAssign` e) =
  evalExp e >>= \v ->
  modify (insert x v)

{-@ evalExp :: forall <p :: Env Int -> Bool>. Exp -> Hoare <p, {\_ e0 e1 -> scopeNum e0 == scopeNum e1}> (Env Int) Int @-}
evalExp :: Exp -> Hoare (Env Int) Int
evalExp (EVar x) =
  get >>= \e ->
  case lookup x e of
    Just v -> return v
    Nothing -> undefined
evalExp (EInt n) = return n
evalExp (e1 `EPlus` e2) = 
  evalExp e1 >>= \v1 ->
  evalExp e2 >>= \v2 ->
  return (v1 + v2)
evalExp (EScope s e) =
  modify beginScope >>= \_ ->
  eval s            >>= \_ ->
  evalExp e         >>= \v ->
  modify endScope   >>= \_ ->
  return v
{{< /highlight >}}
最後にテストをしてみます。
以下のテスト関数は```a := 5; b := ({a := 10}; a); c := a```というプログラムをテストしています。
{{< highlight Haskell >}}
run :: Stm -> ((), Env Int)
run s = runHoare (eval s) empty

{-@ ignore testEval @-}
testEval :: IO ()
testEval = print $ run s
  where
    -- a := 5; b := ({a := 10}; a); c := a
    s =        ("a" `SAssign` EInt 5) 
        `SSeq` ("b" `SAssign` EScope ("a" `SAssign` EInt 10) (EVar "a")) 
        `SSeq` ("c" `SAssign` EVar "a")
{{< /highlight >}}
実行結果は以下です。
ちゃんとスコープを外れると```a```への束縛がもとに戻っていることが確認できます。
{{< highlight Haskell >}}
$ stack ghci
...
*Liquid.Hoare> testEval
((),Env {stack = [Push "c",Push "b",Push "a"], env = fromList [("a",[5]),("b",[10]),("c",[5])]})
{{< /highlight >}}

# 補足: Hoareモナドの圏論的定義
Hoareモナドの実装はStateモナドにパラメータを付加したようなものになっていますが、そのせいでどういうモナドなのか直感的には分かりにくいと思います。(僕には分かりませんでした。)
そのため、気になる人のためにHoareモナドの圏論的定義を確認します。
Bart Jacobs. Dijkstra and Hoare monads in monadic computation. Theoretical Computer Science, Volume 604, 2015, Pages 30-45によると、Hoareモナドは圏論的には以下のように定義されます。
簡単のためベースとなるモナドをIdentityモナドで固定します。

<div class="theoremlabel">
定義(Hoareモナド)
</div>
<div class="theorem">
  状態の集合を$S$で固定する。
  以下の三つ組$(\mathcal{H}, \eta^{\mathcal{H}}, (-)^*)$は$Set$上のKleisli tripleとなる。
  <ul>
      <li> 
        関手 $\mathcal{H}: Set \to Set$
        [[
          \begin{aligned}
            &\mathcal{H}X = \{ (P, h, Q) \mid P \subseteq S, h: \mathcal{S}X, Q \subseteq S \times S \times X \, s.t. \, \forall s. P(s) \Rightarrow Q(s, h(s)) \} \\
            &\mathcal{H}f(P, h, Q) = (P, \mathcal{S}f(h), \{ (s, s', f(x)) \mid Q(s, s', x) \})
          \end{aligned}
        ]]
      </li>
      <li>
        自然変換 $\eta^{\mathcal{H}}_X(x) = (S, \eta^{\mathcal{S}}_X(x), \{ (s, s, x) | s \in S \})$
      </li>
      <li>
        $f: X \to \mathcal{H}Y$に対して、$f^*: \mathcal{H}X \to \mathcal{H}Y$は、$f^*(P, h, Q) = (P', h', Q')$ where
        <ul>
          <li> $P' = \{s \mid P(s) \wedge \forall s', x. \, Q(s, s', x) \Rightarrow pre(f(x))(s')\}$ </li>
          <li> $h'(s) = prog(s')$ where $(s', (\_, prog, \_)) = \mathcal{S}f(h)(s)$ </li>
          <li> $Q = \{(s, t, y) \mid \exist s', x . \, Q(s, s', x) \wedge post(f(x))(s', t, y) \}$ </li>
        </ul>
      </li>
  </ul>
  ただし、$\mathcal{P}$は冪集合モナド, $\mathcal{S}$は$S$上の状態モナド、$\eta^\mathcal{S}$は状態モナドの$unit$、$pre(P, h, Q) = P$、 $post(P, h, Q) = Q$ である。
  これから誘導されるモナドをHoareモナドと呼ぶ。
</div>

これらがモナド則を満たすことは読者への演習問題とする(言ってみたかっただけ)。
直感的には、$\mathcal{H}X$は状態の事前条件、計算、事後条件の3つ組で、事前条件を満たしている上で計算をすると事後条件を満たすものたちの集合です。
事後条件$Q$は計算前の状態との相対的な条件を記述できるよう、$Q \subseteq S \times S \times X$となっています。
例えば、```v' = v + 1```のような条件を書くことができます。
$f^*(P, h, Q)$の事前条件は、$P(s)$が成り立っていて、かつ$Q(s, s', x)$が成り立つならば$f(x)$の事前条件が成り立つというものです。
計算部分は状態モナドと同じです。
事後条件は、$Q$と$post(f(x))$を繋げる$s'$が存在するというものです。
上で用いた実装では、$P$、$Q$が述語パラメータとなっていました。

# まとめ
今回はLiquidHaskellでHoareモナドを扱いました。
HoareモナドはStateモナドの状態に対して事前・事後条件を追加したものであることを見ました。
これによって前々回の実装した```eval```をHoareモナドによってHaskellらしいコードに書き直すことに成功しました。
また、前回説明したAbstract, Bounded Refinementをふんだんに使うことで、それらの具体例を補いました。

当初の予定通り、Hoareモナドまで終わったので、今回でLiquidHaskell入門の連載を終わりにしたいと思います。
ここまで見ていただきありがとうございました。

