---
title: "LiquidHaskell 入門 その4"
date: 2019-03-15T11:51:36+09:00
draft: true
tags: ["Haskell", "形式検証"]
---

前回はLiquidHaskellを実際に使ってみました。
今回は前回のナイーブなコードの状態モナドを用いたリファクタリングを通して、LiquidHaskellの限界を見ていきたいと思います。
<!--more-->

# LiquidHaskellとStateモナド
LiquidHaskellによってスコープ付き環境とそれらの関数に対してより細かい型をつけ、実際に誤った使い方をしたときに型検査に失敗するところまで確認しました。
LiquidHaskellは確かに有用であることが分かりました。
しかし、前回のコードを見たHaskellerのみなさんはもどかしい気持ちになったのではないでしょうか。
evalの型をもう一度見直してみましょう。

{{< highlight Haskell >}}
eval :: (MonadWriter [Int] m, MonadError EvalException m) => Stm -> Env Int -> m ((), Env Int)
evalExp :: (MonadWriter [Int] m, MonadError EvalException m) => Exp -> Env Int -> m (Int, Env Int)
{{< /highlight>}}

この型は状態モナドですので、本来なら以下のように書きたいです。

{{< highlight Haskell >}}
eval :: (MonadWriter [Int] m, MonadError EvalException m) => Stm -> StateT (Env Int) m ()
evalExp :: (MonadWriter [Int] m, MonadError EvalException m) => Exp -> StateT (Env Int) m Int
{{< /highlight>}}

しかしながらそのまま状態モナドを用いると、状態の事前条件と事後条件について何も言えません。
実際にendScopeを呼んでみると、型検査に失敗します。
endScopeを呼ぶための事前条件を状態が満たしていることを保証できないためです。
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
状態モナドに事前条件と事後条件をつけたようなものはNanevskiらのHoareモナドとして知られています。
詳しくはAleksandar Nanevski, Greg Morrisett and Lars Birkedal. Hoare Type Thoery, Polymorphism and Separation. JFP 2007.を参照してください。
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
例えば、$v' = v + 1$のような条件を書くことができます。
$f^*(P, h, Q)$の事前条件は、$P(s)$が成り立っていて、かつ$Q(s, s', x)$が成り立つならば$f(x)$の事前条件が成り立つというものです。
計算部分は状態モナドと同じです。
事後条件は、$Q$と$post(f(x))$を繋げる$s'$が存在するというものです。

# HoareモナドのLiquidHaskellでの実装?
それではHoareモナドをLiquidHaskellで実装してみましょう。
以下のような実装をできればしたいです。
状態モナドに事前条件、事後条件を型のパラメータとして与えることによって三つ組を表現しています。
(型パラメータの説明はここでする。)
(ちなみに型パラメータで述語を表現する理由について言及する。検証をLiquidHaskellで行うため)
(ついでに、Hoareモナドをわざわざ圏論的に説明した理由についても言及する)

{{< highlight Haskell "linenos=table,linenostart=1" >}}
{-@ data State s a <p :: s -> Bool, q :: a -> s -> s -> Bool>
       = State (v: s<p> -> (x::a, s<q x v>)) @-}
data State s a = State (s -> (a, s))

{-@ return :: forall <p :: s -> Bool>. 
              y:a 
           -> State <p, {\x s t -> x == y &&  s == t}> s a @-}
return :: a -> State s a
return a = State (\s -> (a, s))

{-@ (>>=) :: forall <p  :: s -> Bool
                   , q  :: a -> s -> s -> Bool
                   , p' :: a -> s -> Bool
                   , q' :: b -> s -> s -> Bool>. 
                   State <p, q> s a 
                -> (x:a -> State <p' x, q'> s b)
                -> State <{\s -> forall x, s'. q x s s' ==> p' x s'}
                        , {\y s t -> exists x, s'. q x s s' && q' y s' t}> s b @-}
(>>=) :: State s a -> (a -> State s b) -> State s b
m >>= k = State (\s -> let (a, s') = runState m s in runState (k a) s')
{{< /highlight >}}

しかしながら、このコードは構文検査に通りません。
LiquidHaskellは検証を決定可能にするため、量化子を含まない論理式しか書けず、上のような実装はできませんでした。

ためしにフルのHoareモナドを諦めて弱い形で実装してみようとしてみましたが駄目でした。

{{< highlight Haskell "linenos=table,linenostart=21">}}
{-@ data State s a <p :: s -> Bool, q :: a -> s -> Bool> = State (s<p> -> (x::a, s<q x>)) @-}
data State s a = State (s -> (a, s))

{-@ return :: forall <p :: a -> s -> Bool>. x:a -> State <p x, {\y s -> x == y && p y s}> s a @-}
return :: a -> State s a
return a = State (\s -> (a, s))
{{< /highlight >}}

このコードだと以下のエラーが出ました。
Abstract Predicateの部分適用はできないらしいです。
エラーに書いてあるissueの解決策をやってみてもダメでした。

{{< highlight bash >}}
/src/Liquid/State.hs:33:15-85: Error: Malformed predicate application

 33 | {-@ return :: forall <p :: a -> s -> Bool>. x:a -> State <p x, {\y s -> x == y && p y s}> s a @-}
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

     The 1st argument of Liquid.State.State<[]> is predicate `p`
     which expects 1 arguments but is given only 0

     Abstract predicates cannot be partially applied; for a possible fix see:
         https://github.com/ucsd-progsys/liquidhaskell/issues/594
{{< /highlight >}}
いくつか頑張ってみましたが、どうやら無理そうです。
ところでAbstract Refinement周りはイマイチ構文もよくわからないし、検査失敗して出てくるエラーもよくわからないので、もし詳しい人がいたら教えてください。

# まとめ
今回はLiquidHaskellではできないことを見ました。
前回のコードの状態モナドによるリファクタリングを通して、Hoareモナドの必要性を述べました。
また、Hoareモナドは現状のLiquidHaskellでは書けないことを見ました。

今回でLiquidHaskell入門はひとまず終了です。
次回はF*入門を書く予定です。
LiquidHaskellではできなかったHoareモナドでしたが、F*では可能となっており、今回用いたコードをそのままモナディックに書くことが可能です。
その点について書く予定です。
