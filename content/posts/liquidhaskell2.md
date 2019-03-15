---
title: "LiquidHaskell 入門 その2"
date: 2019-02-09T17:45:36+09:00
draft: true
tags: ["Haskell", "形式手法"]
---

# スコープ付き環境
前回の記事ではLiquidHaskellの入門を行いました。
今回はLiquidHaskellを用いて、通常のHaskellの型ではできない型をつけてみたいと思います。
今回題材とするのは、以下のようなスコープ付き環境を表現するデータ構造です。
データとしては、これまでの操作を記録するスタックと、idと値を結びつけるマップを持っています。
操作としては、スコープの開始を表すBeginと値の挿入を表すPushがあります。
この環境は各idに対して値のリストを持っており、上書きした値をスコープから出るときに元に戻すことが可能です。
これはstackに積んだ操作を巻き戻すことによって実現します。
このデータ構造はTiger本から拝借しました。
スコープ付き環境はスコープの概念のあるプログラミング言語のインタプリタでの変数の環境や、型検査時の型環境を表すことが可能です。
Tiger本では実際にTiger言語の型検査器の実装に用いられていました。

{{< highlight Haskell "linenos=table,linenostart=1">}}
type Id = String
data ScopeOp = Begin | Push Id deriving (Eq, Show)
data Env a = Env { stack :: [ScopeOp], env :: Map.Map Id [a] } deriving (Eq, Show)
{{< /highlight >}}

このデータ構造を操作する関数として以下のようなものがあります。
empty、insert、lookupは通常のマップと同様な関数です。
ただし、insert時にはスタックにPush idを積むことで、このスコープ内でidは定義されたということを記録します。
マップ標準の関数に加えて、beginScopeとendScopeという特徴的な関数があります。
文字通りこれらはスコープの開始、終了を行う関数です。
beginScopeはスタックにBeginを積みます。
endScopeはスタックの操作を次のBeginまで巻き戻します。
スタックの先頭がPush idならば、マップからidのリストの先頭をpopし、空になったらマップからidを消去します。
スタックの先頭がBeginならば、それをpopして終了します。
{{< highlight Haskell "linenos=table,linenostart=4" >}}
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
Stateモナドの状態にスコープ付き環境Env Intを与えています。
{{< highlight Haskell "linenos=table,linenostart=25">}}
test :: IO ()
test = flip evalStateT empty $ do
  printState
  modify (insert "x" 10) >> printState
  modify beginScope >> printState
  modify (insert "x" 12) >> printState
  modify endScope >> printState
  where
    printState = get >>= lift . print
{{< /highlight >}}

結果は以下のようになります。
スコープから出るとスコープ内で定義した"x"が解放されていることが分かります。
また、スタックに行った操作が記録され、endScopeが呼ばれるとBeginまで解放されていることも分かります。
{{< highlight bash >}}
$ stack ghci
GHCi, version 8.6.3: http://www.haskell.org/ghc/  :? for help
[1 of 2] Compiling Env              ( src/Env.hs, interpreted )
[2 of 2] Compiling Main             ( app/Main.hs, interpreted )
Ok, two modules loaded.
*Main Env> test
Env {stack = [], env = fromList []}
Env {stack = [Push "x"], env = fromList [("x",[10])]}
Env {stack = [Begin,Push "x"], env = fromList [("x",[10])]}
Env {stack = [Push "x",Begin,Push "x"], env = fromList [("x",[12,10])]}
Env {stack = [Push "x"], env = fromList [("x",[10])]}
{{< /highlight >}}


# Refinement Typeをつけてみる
ところで先ほどのコードをコンパイルすると以下の警告が出ます。
警告はWincomplete-patternsですが、lookupに対してはidに束縛されたリストが空の場合、endScopeに対してはスタックが空の場合のパターンが足りていないようです。
{{< highlight bash >}}
/home/foresta/playground/src/Env.hs:6:1: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In a case alternative: Patterns not matched: (Just [])
   |
12 | lookup id (Env _ e) = case Map.lookup id e of
   |                       ^^^^^^^^^^^^^^^^^^^^^^^...

/home/foresta/playground/src/Env.hs:6:1: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In an equation for endScope: Patterns not matched: (Env [] _)
   |
19 | endScope (Env (Push id : rest) e)  = endScope $ Env rest (Map.update pop id e)
   | ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^...
{{< /highlight >}}

これらは以下のルールを定めることで、そのパターンにマッチすることはなく問題ないことが分かります。

1. 常に束縛は空にならない (束縛という用語は正しい?)
1. endScopeが呼ばれる前に必ずbeginScopeが呼ばれている。

1は21-24行目で定義されているpop関数によって既に実現されています。
空になる束縛は環境から削除されます。
2は、例えば言語処理系などでこれを用いる場合、構文解析の時点で"{}"の数が合っていることが保証されていることが多いため妥当な仮定と言えるでしょう。
これらをLiquid Haskellを用いて型で表現してみます。

まず1は、Envの定義でマップの値の型のリストの長さを0より大きいという型を与えてあげてみます。
{{< highlight Haskell "linenos=table,linenostart=34">}}
{-@ data Env a = Env { stack :: [ScopeOp], env :: (Map Id { v: [a] | len v > 0 })} @-}
data Env a = Env { stack :: [ScopeOp], env :: Map.Map Id [a]} deriving (Eq, Show)
{{< /highlight >}}

2を型で表現するためには、Envのスコープ数を表現することができるときれいに型がつきそうです。
そこで、以下の補助関数を定義します。
beginNumはスタック内のBeginの数、つまり現在のスコープの深さを計算します。
LiquidHaskellは関数内のロジックまで見てくれないみたいなので、beginNumの仕様をそのまま型として与える必要があります。
scopeNumはEnvのスタックをに対してbeginNumを適用します。
これでEnvのスコープ数を表現する準備が整いました。
NEEnvはスタックのスコープが空でないEnv、EnvNはスタックのスコープ数が第二引数NとなっているEnv、EnvXはスコープ数が第二引数のEnvと等しいEnvを表します。
{{< highlight Haskell "linenos=table,linenostart=36">}}
{-@ measure beginNum @-}
{-@ beginNum :: s: [ScopeOp] -> {v: Nat | (v <= len s) && (len s > 0 && head s == Begin => v == 1 + beginNum (tail s)) && (len s > 0 && not (head s == Begin) => v == beginNum (tail s)) && (len s == 0 => v == 0) } @-}
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
emptyはスコープが0のEnvを返す、insertはスコープの大きさを変えない、といったような型を与えることができました。
lookupは特に新しい型は与えません。(もちろんEnvは前のものと異なっています。)
beginScopeはスコープの深さを1増やす、endScopeはスコープが空でないEnvを受け取ってスコープの深さを1減らす、といったような型を与えることができます。
endScopeには無限ループしないことを示すためにmeasureを与えています。
スタックに長さは単調減少するため、無限ループしないことを示すのにはmeasureとしてスタックの長さを与えてあげれば十分です。
{{< highlight Haskell "linenos=table,linenostart=49" >}}
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
{{< highlight bash >}}
$ stack exec -- liquid src/Env.hs
LiquidHaskell Version 0.8.4.0, Git revision ea8217bcdd78e550ef2b39237dc83fe2f1122b69
Copyright 2013-18 Regents of the University of California. All Rights Reserved.


**** DONE:  A-Normalization ****************************************************
 

**** DONE:  Extracted Core using GHC *******************************************
 

**** DONE:  Transformed Core ***************************************************
 
Working 178% [============================================================================]

**** DONE:  annotate ***********************************************************
 

**** RESULT: SAFE **************************************************************
{{< /highlight >}}
問題なさそうです。

# 実践: インタプリタ
それでは作ったスコープ付き環境を用いて簡単なインタプリタを実装してみます。
対象とする言語は、Tiger本で扱われていたプログラミング言語Linearの算術式を制限したもので、構文は以下です。
逐次実行、変数の割り当て、プリントができます。
ExpのEScope Stm Expはスコープを作る構文で、左のStmで定義した変数はそのスコープ内で有効で、右のExpを評価した値を返します。
{{< highlight Haskell "linenos=table,linenostart=74" >}}
data Stm = SSeq Stm Stm
         | SAssign Id Exp
         | SPrint [Exp]
data Exp = EVar Id
         | EInt Int
         | EPlus Exp Exp
         | EScope Stm Exp
{{< /highlight >}}

これのインタプリタを実装したものが以下です。
printはIOモナドでもよいのですが、Writerモナドで代用します。
未定義変数へのアクセスが起こりうるので、エラーモナドを与えます。
{{< highlight Haskell "linenos=table,linenostart=81">}}
data EvalException = UndefinedVariableException deriving (Show, Typeable)

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

{{< highlight bash >}}
$ stack exec -- liquid src/Liquid/Env.hs
LiquidHaskell Version 0.8.4.0, Git revision ea8217bcdd78e550ef2b39237dc83fe2f1122b69
Copyright 2013-18 Regents of the University of California. All Rights Reserved.


**** DONE:  A-Normalization ****************************************************
 

**** DONE:  Extracted Core using GHC *******************************************
 

**** DONE:  Transformed Core ***************************************************
 
Working 163% [=============================================================================]

**** DONE:  annotate ***********************************************************
 

**** RESULT: UNSAFE ************************************************************


 /mnt/c/Users/tdaic/OneDrive/playground/Haskell/src/Liquid/Env.hs:(111,29)-(116,18): Error: Liquid Type Mismatch

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

# まとめ
今回はスコープ付き環境というデータ構造を題材にLiquidHaskellを用いてみました。
さらに、検証された関数を用いて簡単な言語処理系の実装してみました。
実際に誤ったコードを書いたときに型検査に失敗するところも確認し、LiquidHaskellは確かに有用であることが分かりました。

次回は先ほどのコードをStateモナドを用いてリファクタリングしてみたいと思います。
その際に、現状のLiquidHaskellの限界について触れたいと思います。
