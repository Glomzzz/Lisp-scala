# Lisp-Scala

## Intro

最近刚开始上 [SICP(1985年的课)](https://www.youtube.com/watch?v=-J_xL4IGhJA&list=PLE18841CABEA24090) 先整个 Lisp 解释器当练习环境。

顺便用不太熟的**Scala**与刚学会的 **Parser Combinator** 练手.

## Usage

你会注意到这里有三个不同版本的Parser

- parser 带不是很完善的错误提示
- parser_pure 无错误提示
- parser_with_context 在parse时期便检查完自由变量 （不要用（是曲折的探索）)

虽然都很草台，但是能跑，不是吗？

后面会逐步完善的。

## Todo-List

- [-] 更多内建函数
- [-] 完善错误提示
- [-] Maybe 简单的 REPL
- [-] Maybe 简单的终端界面编辑器


