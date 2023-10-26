module Main (main) where

import Kaleido.AST
import Kaleido.Parser
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec

main :: IO ()
main = specs

toplevel' :: Parser [ExprAST]
toplevel' = toplevel <* eof

specs :: IO ()
specs =
  hspec $ describe "parser" $ do
    it "can parse numbers" $ do
      parse toplevel' "<stdin>" "3;" `shouldParse` [ExprAstNumber (NumberExprAST 3)]
      parse toplevel' "<stdin>" "3.5;" `shouldParse` [ExprAstNumber (NumberExprAST 3.5)]
    it "can parse extern functions" $ do
      parse toplevel' "<stdin>" "extern sin(a);" `shouldParse` [ExprAstPrototype (PrototypeExprAST "sin" ["a"])]
      parse toplevel' "<stdin>" "extern  this   (  that   the  other ) ;"
        `shouldParse` [ ExprAstPrototype (PrototypeExprAST "this" ["that", "the", "other"])
                      ]
    it "can parse variables" $ do
      parse toplevel' "<stdin>" "y;"
        `shouldParse` [ ExprAstVariable (VariableExprAst "y")
                      ]
    it "can parse the simplest function definition" $ do
      parse toplevel' "<stdin>" "def foo() 1;"
        `shouldParse` [ ExprAstFunction
                          ( FunctionExprAST
                              (PrototypeExprAST "foo" [])
                              (ExprAstNumber $ NumberExprAST 1)
                          )
                      ]
    it "can parse plus" $ do
      parse toplevel' "<stdin>" "1+1;"
        `shouldParse` [ ExprAstBinary $
                          BinaryExprAST
                            '+'
                            (ExprAstNumber $ NumberExprAST 1)
                            (ExprAstNumber $ NumberExprAST 1)
                      ]
      parse toplevel' "<stdin>" "1+1+2;"
        `shouldParse` [ ExprAstBinary $
                          BinaryExprAST
                            '+'
                            ( ExprAstBinary $
                                BinaryExprAST
                                  '+'
                                  (ExprAstNumber $ NumberExprAST 1)
                                  (ExprAstNumber $ NumberExprAST 1)
                            )
                            (ExprAstNumber $ NumberExprAST 2)
                      ]
    it "can parse minus" $ do
      parse toplevel' "<stdin>" "1-1;"
        `shouldParse` [ ExprAstBinary $
                          BinaryExprAST
                            '-'
                            (ExprAstNumber $ NumberExprAST 1)
                            (ExprAstNumber $ NumberExprAST 1)
                      ]
      parse toplevel' "<stdin>" "1-1+2;"
        `shouldParse` [ ExprAstBinary $
                          BinaryExprAST
                            '+'
                            ( ExprAstBinary $
                                BinaryExprAST
                                  '-'
                                  (ExprAstNumber $ NumberExprAST 1)
                                  (ExprAstNumber $ NumberExprAST 1)
                            )
                            (ExprAstNumber $ NumberExprAST 2)
                      ]
    it "can parse times" $ do
      parse toplevel' "<stdin>" "1*1;"
        `shouldParse` [ ExprAstBinary $
                          BinaryExprAST
                            '*'
                            (ExprAstNumber $ NumberExprAST 1)
                            (ExprAstNumber $ NumberExprAST 1)
                      ]
      parse toplevel' "<stdin>" "1*1*2;"
        `shouldParse` [ ExprAstBinary $
                          BinaryExprAST
                            '*'
                            ( ExprAstBinary $
                                BinaryExprAST
                                  '*'
                                  (ExprAstNumber $ NumberExprAST 1)
                                  (ExprAstNumber $ NumberExprAST 1)
                            )
                            (ExprAstNumber $ NumberExprAST 2)
                      ]
    it "can parse times and plus" $ do
      parse toplevel' "<stdin>" "1*1+2;"
        `shouldParse` [ ExprAstBinary $
                          BinaryExprAST
                            '+'
                            ( ExprAstBinary $
                                BinaryExprAST
                                  '*'
                                  (ExprAstNumber $ NumberExprAST 1)
                                  (ExprAstNumber $ NumberExprAST 1)
                            )
                            (ExprAstNumber $ NumberExprAST 2)
                      ]
    it "can parse plus and times " $ do
      parse toplevel' "<stdin>" "1+1*2;"
        `shouldParse` [ ExprAstBinary $
                          BinaryExprAST
                            '+'
                            (ExprAstNumber $ NumberExprAST 1)
                            ( ExprAstBinary $
                                BinaryExprAST
                                  '*'
                                  (ExprAstNumber $ NumberExprAST 1)
                                  (ExprAstNumber $ NumberExprAST 2)
                            )
                      ]
    it "can parse the first 2.8 example" $ do
      parse toplevel' "<stdin>" "def foo(x y) x+foo(y, 4.0);"
        `shouldParse` [ ExprAstFunction
                          ( FunctionExprAST
                              { prototype = PrototypeExprAST {name = "foo", args = ["x", "y"]},
                                body =
                                  ExprAstBinary
                                    ( BinaryExprAST
                                        { op = '+',
                                          lhs = ExprAstVariable (VariableExprAst {name = "x"}),
                                          rhs =
                                            ExprAstCall
                                              ( CallExprAST
                                                  { callee = "foo",
                                                    args =
                                                      [ ExprAstVariable (VariableExprAst {name = "y"}),
                                                        ExprAstNumber (NumberExprAST {val = 4.0})
                                                      ]
                                                  }
                                              )
                                        }
                                    )
                              }
                          )
                      ]
    it "can parse the second 2.8 example" $ do
      parse toplevel' "<stdin>" "def foo(x y) x+y; y;"
        `shouldParse` [ ExprAstFunction
                          FunctionExprAST
                            { prototype =
                                PrototypeExprAST
                                  { name = "foo",
                                    args = ["x", "y"]
                                  },
                              body =
                                ExprAstBinary
                                  BinaryExprAST
                                    { op = '+',
                                      lhs =
                                        ExprAstVariable
                                          VariableExprAst
                                            { name = "x"
                                            },
                                      rhs =
                                        ExprAstVariable
                                          VariableExprAst
                                            { name = "y"
                                            }
                                    }
                            },
                        ExprAstVariable
                          VariableExprAst
                            { name = "y"
                            }
                      ]
    it "can't parse the third 2.8 example" $ do
      parse toplevel' "<stdin>" "def foo(x y) x+y );" `shouldFailWith` err 17 (utok ')' <> foldMap @[] etok "*+-/;")
