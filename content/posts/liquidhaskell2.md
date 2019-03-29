---
title: "LiquidHaskell 入門 その2"
date: 2019-03-29T11:30:36+09:00
draft: false
tags: ["Haskell", "形式検証"]
---

[前回の記事](https://forestaa.github.io/blog/posts/liquidhaskell1/)ではLiquidHaskellの基礎を学びました。
今回はLiquidHaskellを用いて、とあるデータ型に対して、通常のHaskellの型の上に、実装者の暗黙の意図を反映したより細かい型をつけてみたいと思います。
<!--more-->

# スコープ付き環境
今回題材とするのはスコープ付きの言語の環境を表現する以下のようなデータ型です。
データとしては、環境に対するこれまでの操作を記録するスタックと、```id```と値のリストを結びつけるマップ```Env```を持っています。
操作```ScopeOp```としては、スコープの開始を表す```Begin```と環境への値の挿入を表す```Push```があります。
この環境は各```id```に対して値のリストを持っており、同じ記号に対する値の挿入が起きるとリストの先頭に値を挿入します。
スコープから出るときはスコープの開始前の環境に戻す必要があるのですが、これはスタックに積んだ操作を巻き戻すことによって実現します。
このデータ型は[Tiger本](https://www.amazon.co.jp/%E6%9C%80%E6%96%B0%E3%82%B3%E3%83%B3%E3%83%91%E3%82%A4%E3%83%A9%E6%A7%8B%E6%88%90%E6%8A%80%E6%B3%95-Andrew-W-Appel/dp/4798114685)から拝借しました。
このデータ型をここではスコープ付き環境と呼ぶことにします。
スコープ付き環境はスコープの概念のあるプログラミング言語のインタプリタでの変数の環境や、型検査時の型環境を表すことが可能です。
Tiger本ではTiger言語の型検査器の実装等に実際に用いられていました。

{{< highlight Haskell >}}
type Id = String
data ScopeOp = Begin | Push Id deriving (Eq, Show)
data Env a = Env { stack :: [ScopeOp], env :: Map.Map Id [a] } deriving (Eq, Show)
{{< /highlight >}}

このデータ構造を操作する関数として以下のようなものがあります。
```empty, insert, lookup```は通常のマップと同様な関数です。
ただし、```insert```時にはスタックに```Push id```を積むことで、このスコープ内で```id```に値が束縛されたということを記録します。
マップ標準の関数に加えて、```beginScope```と```endScope```という特徴的な関数があります。
これらは文字通りスコープの開始、終了を行う関数です。
```beginScope```はスタックに```Begin```を積みます。
```endScope```はスタックの操作を```Begin```まで巻き戻します。
スタックの先頭が```Push id```ならばマップから```id```のリストの先頭を```pop```し、リストが空になったらマップから```id```を消去します。
スタックの先頭が```Begin```ならば、それをスタックから```pop```して終了します。
{{< highlight Haskell >}}
empty :: Env a
empty = Env [] Map.empty
insert :: Id -> a -> Env a -> Env a
insert id a (Env s e) = Env { stack = Push id : s, env = Map.alter f id e }
  where
    f Nothing = Just [a]
    f (Just as) = Just (a:as)
lookup :: Id -> Env a -> Maybe a
lookup id (Env _ e) = case Map.lookup id e of
  Just (a:_) -> return a
  Nothing -> Nothing

beginScope :: Env a -> Env a
beginScope e = e { stack = Begin : stack e }
endScope :: Env a -> Env a
endScope (Env (Push id : rest) e) = endScope $ Env rest (Map.update pop id e)
  where
    pop [] = Nothing
    pop [_] = Nothing
    pop (_:as) = Just as
endScope (Env (Begin : rest) e) = Env rest e
{{< /highlight >}}

実際に使ってみましょう。
以下のテストコードを走らせます。
Stateモナドの状態にスコープ付き環境```Env Int```を与えています。
{{< highlight Haskell >}}
testEnv :: IO ()
testEnv = flip evalStateT empty $ do
  printState
  modify (insert "x" 10) >> printState
  modify beginScope      >> printState
  modify (insert "x" 12) >> printState
  modify endScope        >> printState
  where
    printState = get >>= lift . print
{{< /highlight >}}

結果は以下のようになります。
スコープから出るとスコープ内で挿入した```x = 12```が解放されていることが分かります。
また、スタックに行った操作が記録され、```endScope```が呼ばれると```Begin```まで解放されていることも分かります。
{{< highlight bash >}}
$ stack ghci
...
*Main Env> testEnv
Env {stack = [], env = fromList []}
Env {stack = [Push "x"], env = fromList [("x",[10])]}
Env {stack = [Begin,Push "x"], env = fromList [("x",[10])]}
Env {stack = [Push "x",Begin,Push "x"], env = fromList [("x",[12,10])]}
Env {stack = [Push "x"], env = fromList [("x",[10])]}
{{< /highlight >}}


# Refinement Typeをつけてみる
ところで先ほどのコードをコンパイルすると以下の警告が出ます。
警告は```Wincomplete-patterns```ですが、```lookup```に対しては```id```に束縛されたリストが空の場合、```endScope```に対してはスタックが空の場合のパターンが足りていないようです。
{{< highlight bash >}}
/src/Env.hs:6:1: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In a case alternative: Patterns not matched: (Just [])
   |
12 | lookup id (Env _ e) = case Map.lookup id e of
   |                       ^^^^^^^^^^^^^^^^^^^^^^^...

/src/Env.hs:6:1: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In an equation for endScope: Patterns not matched: (Env [] _)
   |
19 | endScope (Env (Push id : rest) e)  = endScope $ Env rest (Map.update pop id e)
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^...
{{< /highlight >}}

従って、これらの関数はMaybeモナドやEitherモナドに包んでエラーを表現するのがお行儀がよいです。
しかしながら、これらは呼び出すときに以下のルールを定めることで、実際にはそのパターンにマッチすることはなく、問題にならないことが分かります。

1. ```id```に束縛されている値のリストは空にならない
1. ```endScope```が呼ばれる前に必ず```beginScope```が呼ばれている。

1は```endScope```内で定義されている```pop```関数によって既に実現されています。
リストが空になる時は環境から削除されます。
2は、例えば言語処理系などでこれを用いる場合、構文解析の時点で```{}```の数が合っていることが保証されていることが多いため妥当な仮定と言えるでしょう。
余計なモナドで包むことを避けるために、これらのルールをLiquid Haskellを用いて型で表現してみます。

まず1を表現するために、```Env```の定義で値のリストの長さを0より大きいという型を与えてあげてみます。
{{< highlight Haskell >}}
{-@ data Env a = Env { stack :: [ScopeOp], env :: (Map Id { v: [a] | len v > 0 })} @-}
data Env a = Env { stack :: [ScopeOp], env :: Map.Map Id [a]} deriving (Eq, Show)
{{< /highlight >}}

2を型で表現するためには、```Env```のスコープ数を表現することができるときれいに型がつきそうです。
そこで、以下の補助関数を定義します。
```beginNum```はスタック内の```Begin```の数、つまり現在のスコープの深さを計算します。
前回は説明していなかったのですが、```measure```となる関数には停止性を保証しておく必要があるため、メトリクスを書いておきます。
```scopeNum```は```Env```のスタックをに対して```beginNum```を適用します。
これで```Env```のスコープ数を表現する準備が整いました。
```NEEnv```はスタックのスコープが空でない```Env```、```EnvN```はスタックのスコープ数が第二引数```N```となっている```Env```、```EnvX```はスコープ数が第二引数の```Env```と等しい```Env```を表します。
{{< highlight Haskell >}}
{-@ measure beginNum @-}
{-@ beginNum :: s: [ScopeOp] -> Nat / [len s] @-}
beginNum :: [ScopeOp] -> Int
beginNum [] = 0
beginNum (Begin:xs) = 1 + beginNum xs
beginNum (_:xs) = beginNum xs
{-@ measure scopeNum @-}
{-@ scopeNum :: e: Env a -> {n: Nat | n == beginNum (stack e) } @-}
scopeNum :: Env a -> Int
scopeNum (Env s _) = beginNum s

{-@ type NEEnv a = {e: Env a | scopeNum e > 0} @-}
{-@ type EnvN a N = {e: Env a | scopeNum e = N} @-}
{-@ type EnvX a E = {e: Env a | scopeNum e = scopeNum E} @-}
{{< /highlight >}}

これらを用いて先ほど定義した関数にさらに細かい型を与えてみます。
```empty```はスコープが0の```Env```を返す、```insert```はスコープの大きさを変えない、といったような型を与えることができました。
```lookup```には特に新しい型は与えません。
```beginScope```はスコープの深さを1増やす、```endScope```はスコープが空でない```Env```を受け取ってスコープの深さを1減らす、といったような型を与えることができます。
```endScope```には無限ループしないことを示すためにメトリックを与えています。
スタックの長さは単調減少するので、無限ループしないことを示すためにはメトリックとしてスタックの長さ```[len (stack e)]```を与えてあげれば十分です。
{{< highlight Haskell >}}
{-@ empty :: EnvN a 0 @-}
empty :: Env a
empty = Env [] Map.empty
{-@ insert :: Id -> a -> e:Env a -> EnvN a {scopeNum e} @-}
insert :: Id -> a -> Env a -> Env a
insert id a (Env s e) = Env { stack = Push id : s, env = Map.alter f id e }
  where
    f Nothing = Just [a]
    f (Just as) = Just (a:as)
lookup :: Id -> Env a -> Maybe a
lookup id (Env _ e) = case Map.lookup id e of
  Just (a:_) -> return a
  Nothing -> Nothing

{-@ beginScope :: e: Env a -> EnvN a {1 + scopeNum e} @-}
beginScope :: Env a -> Env a
beginScope e = e { stack = Begin : stack e }
{-@ endScope :: e: NEEnv a -> EnvN a {scopeNum e - 1} / [len (stack e)] @-}
endScope :: Env a -> Env a
endScope (Env (Push id : rest) e) = endScope $ Env rest (Map.update pop id e)
  where
    pop [] = Nothing
    pop [_] = Nothing
    pop (_:as) = Just as
endScope (Env (Begin : rest) e) = Env rest e
{{< /highlight >}}

これで先ほどの1,2のルールを表現することができました。
実際にLiquidHaskellでコンパイルしてみましょう。
今回は```ScopeOp```でバリアント型を用いたため、オプション```--exactdc```を使う必要があります。
詳しくは[こちらのissue](https://github.com/ucsd-progsys/liquidhaskell/issues/1284)をご覧ください。
ファイルの先頭に```{-@ LIQUID "--exactdc" @-}```を記述してもokです。
{{< highlight bash >}}
$ stack exec -- liquid src/Env.hs --exactdc
...
**** RESULT: SAFE **************************************************************
{{< /highlight >}}
問題なさそうです。

# 実践: インタプリタ
それでは先ほど作ったスコープ付き環境を用いて簡単な評価関数```eval```を実装してみます。
対象とする言語は、Tiger本で扱われていたプログラミング言語Linearの算術式を制限したもので、構文は以下です。
逐次実行、変数の割り当て、プリントができます。
```Exp```の```EScope Stm Exp```はスコープを作る構文で、左の```Stm```で定義した変数はそのスコープ内で有効で、右の```Exp```を評価した値を返します。
{{< highlight Haskell >}}
data Stm = SSeq Stm Stm
         | SAssign Id Exp
         | SPrint [Exp]
data Exp = EVar Id
         | EInt Int
         | EPlus Exp Exp
         | EScope Stm Exp
{{< /highlight >}}

これの```eval```を実装したものが以下です。
```SPrint```はによる副作用はIOモナドでもよいのですが、MonadWriterで代用します。
未定義変数へのアクセスが起こりうるので、MonadErrorを与えます。
簡単のため、停止性検査を無効にしておきます。
{{< highlight Haskell >}}
data EvalException = UndefinedVariableException deriving (Show)

{-@ lazy eval @-}
{-@ eval :: (MonadWriter [Int] m, MonadThrow m) => Stm -> e: Env Int -> m ((), EnvX Int e)  @-}
eval :: (MonadWriter [Int] m, MonadError EvalException m) => Stm -> Env Int -> m ((), Env Int)
eval (s1 `SSeq` s2) env0 = do
  (_, env1) <- eval s1 env0
  eval s2 env1
eval (x `SAssign` e) env0 = do
  (v, env1) <- evalExp e env0
  return ((), insert x v env1)
eval (SPrint es) env0 = (,) () <$> foldl f (pure env0) es
  where
    f m e = do
      (n, env') <- evalExp e =<< m
      tell [n]
      return env'

{-@ lazy evalExp @-}
{-@ evalExp :: (MonadWriter [Int] m, MonadThrow m) => Exp -> e: Env Int -> m (Int, EnvX Int e)  @-}
evalExp :: (MonadWriter [Int] m, MonadError EvalException m) => Exp -> Env Int -> m (Int, Env Int)
evalExp (EVar x) env0 = case lookup x env0 of
  Just v -> return (v, env0)
  Nothing -> throwError UndefinedVariableException
evalExp (EInt n) env0 = return (n, env0)
evalExp (e1 `EPlus` e2) env0 = do
  (v1, env1) <- evalExp e1 env0
  (v2, env2) <- evalExp e2 env1
  return (v1 + v2, env2)
evalExp (EScope s e) env0 = do
  let env1 = beginScope env0
  (_, env2) <- eval s env1
  (v, env3) <- evalExp e env2
  let env4 = endScope env3
  return (v, env4)
{{< /highlight >}}


ここで重要なのは、```evalExp```内の```EScope```のパターンマッチの部分です。
ちゃんと```beginScope```と```endScope```で挟んであります。
例えば、ここのうち```endScope```をコメントアウトすると、以下のようにちゃんとエラーを報告してくれます。

{{< highlight bash >}}
$ stack exec -- liquid src/Liquid/Env.hs
...
**** RESULT: UNSAFE ************************************************************


/src/Liquid/Env.hs:(111,29)-(116,18): Error: Liquid Type Mismatch

 111 | evalExp (EScope s e) env0 = do
 112 |   let env1 = beginScope env0
 113 |   (_, env2) <- eval s env1
 114 |   (v, env3) <- evalExp e env2
 115 |   -- let env4 = endScope env3
 116 |   return (v, env3)

   Inferred type
     VV : {v : (Liquid.Env.Env GHC.Types.Int) | Liquid.Env.scopeNum v >= 0}

   not a subtype of Required type
     VV : {VV : (Liquid.Env.Env GHC.Types.Int) | Liquid.Env.scopeNum VV == Liquid.Env.scopeNum env0}

   In Context
     env0 : {env0 : (Liquid.Env.Env GHC.Types.Int) | Liquid.Env.scopeNum env0 >= 0}
{{< /highlight>}}

それでは各自LiquidHaskellでコンパイルしたうえで、正しく動いているかテストしてみましょう。
以下のテスト関数を用意してください。
読みにくいですが、```a := 5 + 3; print [a, {a := 10, a}]```のようなプログラムをテストをしています。
{{< highlight Haskell >}}
run :: Stm -> Either EvalException (((), Env Int), [Int])
run s = runWriterT $ eval s empty

{-@ ignore testEval @-}
testEval :: IO ()
testEval = print $ run s
  where
    s =        ("a" `SAssign` (EInt 5 `EPlus` EInt 3)) 
        `SSeq` SPrint [ EVar "a", 
                        EScope ("a" `SAssign` EInt 10) (EVar "a")] 

{{< /highlight >}}

実行結果は以下です。
これはまた読みづらいですが、```Env```が終了時の環境で、一番右の```[8, 10]```が```SPrint```による出力結果です。
問題ないようです。

{{< highlight Haskell >}}
$ stack ghci
...
*Main Env> testEval
Right (((),Env {stack = [Push "a"], env = fromList [("a",[8])]}),[8,10])
{{< /highlight >}}

# まとめ
今回はスコープ付き環境というデータ型を題材にLiquidHaskellを用いてみました。
その中で、通常のHaskellでは表現できない、実装者の意図している暗黙の仕様をRefinemet Typeを用いて表現することができ、それをLiquidHaskllを用いて検証することができました。
さらに、検証された関数を用いて簡単な言語の評価関数の実装をしてみました。
実際に誤ったコードを書いたときに型検査に失敗するところも確認し、LiquidHaskellは確かに有用であることが分かりました。

# 次(々)回予告
先ほどのコードを読んでHaskellerの皆さんはもどかしい気持ちになったのではないでしょうか。
もう一度```eval```の型を確認してみましょう。

{{< highlight Haskell >}}
eval :: (MonadWriter [Int] m, MonadError EvalException m) => Stm -> Env Int -> m ((), Env Int)
evalExp :: (MonadWriter [Int] m, MonadError EvalException m) => Exp -> Env Int -> m (Int, Env Int)
{{< /highlight>}}

この型はStateモナドを用いると以下のように書き直せます。

{{< highlight Haskell >}}
eval :: (MonadWriter [Int] m, MonadError EvalException m) => Stm -> StateT (Env Int) m ()
evalExp :: (MonadWriter [Int] m, MonadError EvalException m) => Exp -> StateT (Env Int) m Int
{{< /highlight>}}

そのため、Stateモナドの状態に対して事前条件・事後条件を設定したくなり、これが前回の記事の冒頭で触れたHoareモナドの正体となります。
しかしながら、前回軽く触れたようにLiquidHaskellでは量化子のない述語論理式しか書けないため、そのままではHoareモナドが実装できないのですが、LiquidHaskellの発展的な機能?である**Abstract Refienement**、**Bounded Refinement**を用いるとHoareモナドを実現できます。
次回ではAbstract Refinement、Bounded Refinementの解説をし、次々回でHoareモナドの実装をして上のコードのリファクタリングをしていきます。
どうぞよろしくお願いいたします。
