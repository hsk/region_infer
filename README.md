# リージョン推論

SMLで書いてあったリージョン推論をOCamlに移植した物です。

original SML code: https://gist.github.com/pasberth/311b81324bba06b023b27a08386d4ee4


構文

    x ::=                     識別子
          ('a'|...|'z'|'A'|...|'Z') ('a'|...|'z'|'A'|...|'Z'|'0'|...|'9'|'_')*
    e ::=                     式
        | x                     変数
        | e e                   関数適用
        | ^ x : t . e           λ抽象
        | let x : t := e in e   let式
        | ( e )                 括弧式
    t ::=                     型
        | int                   整数
        | t -> t                関数
        | ( t )                 括弧式

    r ::= x
        | ^ x . r at t
        | r r
        | letregion x* r

例1

    let one : (int -> int) -> int -> int
            := ^x : int -> int. ^y : int. x y in
    let two : (int -> int) -> int -> int
            := ^x : int -> int. ^y : int. x (x y) in
    one

    (letregion r21 (
      (^one.(
        letregion r20 r19 r17 r14 (
          (^two.one at r14)
          (^x.(^y.(x (x y)) at r19) at r20))) at r21)
      (^x.(^y.(x y) at r26) at r27)
    ))

    let one : int -> int :=
      ^x : int. x
    in
    one

    (letregion r3 (
      (^one.one at r3)
      (^x.x at r5)
    ))

# Reference

A Region Inference Algorithm - Martin Elsman
http://www.elsman.com/mlkit/pdf/toplas98.pdf

