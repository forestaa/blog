---
title: "Amazon Aurora"
date: 2021-09-11T17:54:43+09:00
draft: true
tags: ["Database"]
---

久しぶりにブログを書きます。
最近仕事しかしてないしコードも書いてないでずっとインプットしかしてなかったので、これからはもう少し定期的にアウトプットしていきたいなと思います。
(いつまでこの気持ちが続くかは知らない。)

ここ最近はずっとデータベースの勉強をしてました。
その流れで[Amazon Aurora: Design Considerations for High Throughput Cloud-native Relational Databases](https://www.amazon.science/publications/amazon-aurora-design-considerations-for-high-throughput-cloud-native-relational-databases)を読んだので内容をまとめたいと思います。
何か新情報があるわけでもないですが自分の理解の整理のためにアウトプットします。

# Features
TODO
- ストレージサービスをコンピュートサービスから分離されたfault tolerantでself-healingなサービスとして作ることでデータベースをパフォーマンスvarianceから守り、ネットワークやストレージ層の一時的・永久的な故障から守ります。
- サービス間のネットワーク上のやりとりをredo logのみにすることでNetwork IOPSを下げた。
- checkpointやバックアップを一時的な高負荷な処理を分散された処理全体に散らすことでコンピュートサービスを軽くした。

# Architecture
{{< figure src="/images/Aurora-Architecture.png" class="center">}}

TODO
- ストレージ層とコンピュート層は分離されている
- コンピュート層は一つのインスタンスなのに対してストレージ層はEC2でCluster構築されている.
    - このおかげでstorageはいくらでも大きくなる
    - Masterは計算能力に限界がある
- Queryの実行やLogging、Recovery、Backup等はStorage層の役割となる
    - 大部分の処理をコンピュート層からストレージ層に移すことでコンピュート層の計算能力を上げる
- Storage Volume上の一つのデータは6個にコピーされる、Quoramベースの読み書き
- MasterはRedo Logのみストレージに書き込む
    - ページへの適用はストレージ層で行う
    - MasterはPage BufferにPageを最新のページを持つがストレージには書き込まない
- MasterとReplicaはstorageを共有している
- MasterはReplicaにRedo Logのみ送りReplicaはそのログをCacheに適用する
    - Repli遅延

# Storage Volume
TODO(2,3章の話を書く)
- 10GBのsegmentの話を書く。
- Quoramの話を書く。
- Operationの話を書く。
- 例の図を使ってページを更新していくことを書く。
- MySQLの比較は面倒だな?

# Write/Read/Replica
4章の話を書く。非同期にページを更新していくとして、実際にアプリケーションからwrite/readしたときの挙動が気になるという流れ。

# Recovery
# Parallel Query
なんか書くことあれば
